#!/bin/bash
Njobs=$(qstat -u $USER | wc -l)
Njobs=$((Njobs-1))

while [ $Njobs -gt 0 ]
do
sleep 1
done