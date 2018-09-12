#!/bin/bash

max_sims=2000

echo "sbatch install_packages.bash"
echo "library(expoRkit)" > zzz_alpha_LL.R
echo "library(mbd)" >> zzz_alpha_LL.R
echo "args = as.numeric(commandArgs(TRUE))" >> zzz_alpha_LL.R
echo "mbd:::mbd_alpha_loglik(s=args)" >> zzz_alpha_LL.R

for((s = 1; s <= max_sims; s++))
do

echo "#!/bin/bash" > mbd_alpha_LL$s
echo "#SBATCH --time=71:59:00" >> mbd_alpha_LL$s #--time=229:59:00
echo "module load R/3.3.1-foss-2016a" >> mbd_alpha_LL$s
echo "Rscript zzz_alpha_LL.R $s" >> mbd_alpha_LL$s
echo "rm mbd_alpha_LL$s" >> mbd_alpha_LL$s

sbatch --partition=nodes --mem=9GB --job-name=mbdAlpha$s --mail-type=FAIL,TIME_LIMIT --mail-user=glaudanno@gmail.com mbd_alpha_LL$s

done

