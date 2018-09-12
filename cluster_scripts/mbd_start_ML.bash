#!/bin/bash

#max_sims=2000
max_sims=$1

echo "rm -rfv errors/*"
#sbatch /home/$USER/mbd_like/install_packages.bash & sleep 60

echo "library(expoRkit)" > zzz_ML.R
echo "library(mbd)" >> zzz_ML.R
echo "library(DDD)" >> zzz_ML.R
echo "args = as.numeric(commandArgs(TRUE))" >> zzz_ML.R
echo "mbd:::mbd_ML_cluster(s=args)" >> zzz_ML.R

for((s = 1; s <= max_sims; s++))
do

echo "#!/bin/bash" > zMLjob$s
echo "#SBATCH --time=169:59:00" >> zMLjob$s #--time=229:59:00 #--time=95:59:00
#echo "module load R/3.3.1-foss-2016a" >> zMLjob$s
echo "module load R/3.4.4-foss-2018a-X11-20180131" >> zMLjob$s
echo "Rscript zzz_ML.R $s" >> zMLjob$s
echo "rm zMLjob$s" >> zMLjob$s

#NEVER ASK FOR MORE THAN 9GB OF MEMORY!
sbatch --partition=regular --mem=9GB --job-name=ML$s --mail-type=FAIL,TIME_LIMIT --mail-user=glaudanno@gmail.com zMLjob$s

done

