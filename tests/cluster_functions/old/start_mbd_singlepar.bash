#!/bin/bash

max_sims=1000

echo "home_dir = substring(getwd(),1,13)" > zzz_sp.R
echo "fun_dir = paste(home_dir,'/mbd_like/functions',sep = '')" >> zzz_sp.R
echo "source(paste(fun_dir,'/mbd_setup.R',sep = ''))" >> zzz_sp.R
echo "mbd_setup(home_dir)" >> zzz_sp.R
echo "args = as.numeric(commandArgs(TRUE))" >> zzz_sp.R
echo "mbd_minusLL_vs_single_parameter(args)" >> zzz_sp.R

for((s = 1; s <= max_sims; s++))
do

echo "#!/bin/bash" > mbd_single_par$s
echo "#SBATCH --time=229:59:00" >> mbd_single_par$s
echo "module load R/3.3.1-foss-2016a" >> mbd_single_par$s
echo "Rscript zzz_sp.R $s" >> mbd_single_par$s
echo "rm mbd_single_par$s" >> mbd_single_par$s

sbatch --partition=nodes --mem=9GB --job-name=SP$s --mail-type=FAIL,TIME_LIMIT --mail-user=glaudanno@gmail.com mbd_single_par$s
#--output=ML$s.log

done


