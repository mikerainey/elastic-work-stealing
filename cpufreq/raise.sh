#!/usr/bin/env bash

# Raise the limit of all hardware threads to the max.

INFILE=$1

ts=`cat $INFILE`
ar=( $ts )
hi=${ar[1]}

nb_pus=$( hwloc-ls --only pu | wc -l )

[ "$UID" -eq 0 ] || exec sudo "$0" "$@"

for ((i=0;i<$nb_pus;i++))
do
    sudo cpufreq-set -c $i -d $hi -u $hi
done
