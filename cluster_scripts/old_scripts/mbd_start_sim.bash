#!/bin/bash

#echo "sbatch install_packages.bash"

echo "library(mbd)" > zzz_sim.R
echo "c_sim_pars=c(0.2,0.1,0.2,0.15)" >> zzz_sim.R
echo "c_soc=2" >> zzz_sim.R
echo "c_cond=1" >> zzz_sim.R
echo "c_age=10" >> zzz_sim.R
echo "c_max_sims=1000" >> zzz_sim.R
echo "c_multiple_births_interval=c(1,Inf)" >> zzz_sim.R
echo "c_tips_interval=c(0,50)" >> zzz_sim.R
echo "mbd:::mbd_sim_dataset(sim_pars=c_sim_pars,soc=c_soc,cond=c_cond,age=c_age,max_sims=c_max_sims,tips_interval = c_tips_interval,multiple_births_interval=c_multiple_births_interval)" >> zzz_sim.R

#echo "module load R/3.3.1-foss-2016a"
echo "module load R/3.4.4-foss-2018a-X11-20180131"
echo "Rscript zzz_sim.R" 

