#!/usr/bin/env bash

# Capture the min and max frequencies allowed for the first hardware 
# thread and write the result to a file.

OUTFILE=$1

cpufreq-info -c 0 -l > $OUTFILE
