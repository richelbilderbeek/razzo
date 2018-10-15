#!/bin/bash
#SBATCH --time=2:57:58

lambda=$1
mu=$2
nu=$3
q=$4
max_sims=$5

cd /home/$USER/mbd_like/

chmod +x install_packages.bash
./install_packages.bash --output=testinst.out
#sbatch install_packages.bash

sleep 60

rm testinst.out

if [[ -d sims/$lambda-$mu-$nu-$q/data ]]; then
  cd sims/$lambda-$mu-$nu-$q/
  sbatch /home/$USER/mbd_like/mbd_start_ML.bash $max_sims
else
  mkdir -p ./sims/$lambda-$mu-$nu-$q/data/
  sleep 1
  cd sims/$lambda-$mu-$nu-$q/
  
  echo "library(mbd)" > zzz_sim.R
  echo "args = as.numeric(commandArgs(TRUE))" >> zzz_sim.R
  echo "mbd:::mbd_sim_dataset(sim_pars=c(args[1],args[2],args[3],args[4]),max_sims=args[5],tips_interval=c(0,70),cond=1)" >> zzz_sim.R
  #module load R/3.3.1-foss-2016a
  module load R/3.4.4-foss-2018a-X11-20180131
  Rscript zzz_sim.R $lambda $mu $nu $q $max_sims
  
  wait zzz_sim
  
  sbatch /home/$USER/mbd_like/mbd_start_ML.bash $max_sims
fi


