#!/bin/bash

cd /home/p274829/mbd_like/

echo 'start' > test1.txt

find . -name "*.out" -type f -delete

max_sims=2000

for((s = 1; s <= max_sims; s++))
do

rm mbd_ML_job_a$s
#rm ML$s.log

done

echo 'end' >> test1.txt
