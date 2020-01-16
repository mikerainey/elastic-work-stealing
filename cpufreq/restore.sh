#!/usr/bin/env bash

# Restore the frequency limits of the cores to their original settings.

INFILE=$1

ts=`cat $INFILE`
ar=( $ts )
lo=${ar[0]}
hi=${ar[1]}

nb_pus=$( hwloc-ls --only pu | wc -l )

[ "$UID" -eq 0 ] || exec sudo "$0" "$@"

for ((i=0;i<$nb_pus;i++))
do
    sudo cpufreq-set -c $i -d $lo -u $hi
done
