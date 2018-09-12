#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --mem=32000
#SBATCH --output=output_mbd_ML.out
#SBATCH --job-name=mbd_ML
#SBATCH --mail-type=FAIL,END
#SBATCH --mail-user=glaudanno@gmail.com
#SBATCH --time=9-23:00:00
#SBATCH --profile=task

lambda=$1
mu=$2
nu=$3
q=$4
max_sims=$5

sbatch install_packages.bash --output=testinst.out

sleep 5

cd /home/$USER/mbd_like/

if [[ -d sims/$lambda-$mu-$nu-$q/data ]]; then
  cd sims/$lambda-$mu-$nu-$q/
else
  mkdir -p ./sims/$lambda-$mu-$nu-$q/data/
  sleep 1
  cd sims/$lambda-$mu-$nu-$q/
fi

echo "library(mbd)" > zzz_sim.R
echo "args = as.numeric(commandArgs(TRUE))" >> zzz_sim.R
echo "mbd:::mbd_cluster_main(sim_pars=c(args[1],args[2],args[3],args[4]),max_sims=args[5],tips_interval=c(0,70),cond=1)" >> zzz_sim.R
module load R/3.3.1-foss-2016a
Rscript zzz_sim.R $lambda $mu $nu $q $max_sims
  
  



