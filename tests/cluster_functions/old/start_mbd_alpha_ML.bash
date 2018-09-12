#!/bin/bash

max_sims=2000

echo "sbatch install_packages.bash"
echo "library(expoRkit)" > zzz_alpha_MLE.R
echo "library(mbd)" >> zzz_alpha_MLE.R
echo "args = as.numeric(commandArgs(TRUE))" >> zzz_alpha_MLE.R
echo "mbd:::mbd_alpha_ML(s=args)" >> zzz_alpha_MLE.R

for((s = 1; s <= max_sims; s++))
do

echo "#!/bin/bash" > mbd_alpha_MLE$s
echo "#SBATCH --time=229:59:00" >> mbd_alpha_MLE$s #--time=229:59:00
echo "module load R/3.3.1-foss-2016a" >> mbd_alpha_MLE$s
echo "Rscript zzz_alpha_MLE.R $s" >> mbd_alpha_MLE$s
echo "rm mbd_alpha_MLE$s" >> mbd_alpha_MLE$s

sbatch --partition=nodes --mem=9GB --job-name=mbdAlpha$s --mail-type=FAIL,TIME_LIMIT --mail-user=glaudanno@gmail.com mbd_alpha_MLE$s

done

