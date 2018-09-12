#!/bin/bash

cd /home/p274829/mbd_like/

#use this if you want to CLEAN MATRIX CREATION SHIT

#max_b=34
#max_k=100

#for((k = 1; k <= max_k; k++))
#do

#b_limit=$([ $k -le $max_b ] && echo "$k" || echo "$max_b")

#for((b = 0; b <= b_limit; b++))
#do

#rm creaB$k$b
#rm creaB$k$b.log

#done
#done

#use this if you want ALSO to REMOVE SIMULATION GARBAGE

find . -name "*.out" -type f -delete

max_sims=1000

for((s = 1; s <= max_sims; s++))
do

rm mbd_ML_job_a$s
#rm ML$s.log

done
