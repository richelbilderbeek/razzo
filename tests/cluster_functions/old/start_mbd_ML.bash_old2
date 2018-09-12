#!/bin/bash

max_sims=1000

echo "home_dir = substring(getwd(),1,13)" > zzza.R
echo "lib_dir = paste(home_dir,'/R/x86_64-pc-linux-gnu-library/3.2',sep = '')" >> zzza.R
echo "fun_dir = paste(home_dir,'/mbd_like/functions',sep = '')" >> zzza.R
echo ".libPaths(lib_dir)" >> zzza.R
echo "all_files = list.files(path=fun_dir, full.names=TRUE)" >> zzza.R
echo "mbd_files = list.files(pattern=paste('mbd',sep = ''), path=fun_dir, full.names=TRUE)" >> zzza.R
echo "bad_files = list.files(pattern=paste('[.]Rproj',sep = ''), path=fun_dir, full.names=TRUE)" >> zzza.R
echo "Rfiles = list.files(pattern=paste('[.]R',sep = ''), path=fun_dir, full.names=TRUE)" >> zzza.R
echo "good_files = all_files[(all_files%in%mbd_files)&(all_files%in%Rfiles)&!(all_files%in%bad_files)]" >> zzza.R
echo "invisible( sapply(good_files, FUN=source) )" >> zzza.R
echo "args = as.numeric(commandArgs(TRUE))" >> zzza.R
echo "mbd_ML(args)" >> zzza.R

for((s = 1; s <= max_sims; s++))
do

echo "#!/bin/bash" > mbd_ML_job_a$s
echo "#SBATCH --time=229:59:00" >> mbd_ML_job_a$s
echo "module load R/3.3.1-foss-2016a" >> mbd_ML_job_a$s
echo "Rscript zzza.R $s" >> mbd_ML_job_a$s
echo "rm mbd_ML_job_a$s" >> mbd_ML_job_a$s

sbatch --partition=nodes --mem=9GB --job-name=ML$s --mail-type=FAIL,TIME_LIMIT --mail-user=glaudanno@gmail.com mbd_ML_job_a$s
#--output=ML$s.log

done


