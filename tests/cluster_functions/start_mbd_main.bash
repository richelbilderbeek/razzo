#!/bin/bash

lambda=$1
mu=$2
q=$3
max_sims=$4

cd /home/p274829/mbd_like/

sbatch install_packages.bash --output=testinst.out

linee=$((squeue -u p274829) | wc -l); Njobs=$(( $linee-1 ))
while [ $Njobs -gt 1 ]
do
sleep 1
linee=$((squeue -u p274829) | wc -l); Njobs=$(( $linee-1 ))
done
rm testinst.out

if [[ -d sims/$lambda-$mu-$q/data ]]; then
  cd sims/$lambda-$mu-$q/
  sbatch /home/p274829/mbd_like/start_mbd_ML.bash $max_sims
else
  mkdir -p ./sims/$lambda-$mu-$q/data/
  cd sims/$lambda-$mu-$q/
  
  echo "library(mbd)" > zzz_sim.R
  echo "args = as.numeric(commandArgs(TRUE))" >> zzz_sim.R
  echo "mbd:::mbd_sim_dataset(sim_pars=c(args[1],args[2],args[3]),max_sims=args[4])" >> zzz_sim.R
  module load R/3.3.1-foss-2016a
  Rscript zzz_sim.R $lambda $mu $q $max_sims
  
  linee=$((squeue -u p274829) | wc -l); Njobs=$(( $linee-1 ))
  while [ $Njobs -gt 1 ]
  do
  sleep 1
  linee=$((squeue -u p274829) | wc -l); Njobs=$(( $linee-1 ))
  done
  
  sbatch /home/p274829/mbd_like/start_mbd_ML.bash $max_sims
fi



