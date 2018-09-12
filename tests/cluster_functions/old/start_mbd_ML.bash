#!/bin/bash

max_sims=1000

echo "sbatch install_packages.bash"
echo "library(expoRkit)" > zzza.R
echo "library(mbd)" >> zzza.R
echo "args = as.numeric(commandArgs(TRUE))" >> zzza.R
echo "mbd:::mbd_ML_cluster(s=args)" >> zzza.R

for((s = 1; s <= max_sims; s++))
do

echo "#!/bin/bash" > mbd_ML_job_a$s
echo "#SBATCH --time=71:59:00" >> mbd_ML_job_a$s #--time=229:59:00
echo "module load R/3.3.1-foss-2016a" >> mbd_ML_job_a$s
echo "Rscript zzza.R $s" >> mbd_ML_job_a$s
echo "rm mbd_ML_job_a$s" >> mbd_ML_job_a$s

sbatch --partition=nodes --mem=9GB --job-name=ML$s --mail-type=FAIL,TIME_LIMIT --mail-user=glaudanno@gmail.com mbd_ML_job_a$s
#--output=ML$s.log

done

