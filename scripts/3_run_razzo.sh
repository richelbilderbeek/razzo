#!/bin/bash
# Runs the experiment with one job per parameter file.
#
# Usage, locally:
#
#   ./scripts/3_run_razzo
#
# Usage, on Peregrine:
#
#   sbatch ./scripts/3_run_razzo
#
# Peregrine directives:
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=3_run_razzo
#SBATCH --output=3_run_razzo.log
module load R
module load MPFR

echo "Host name: "$HOSTNAME

if [[ "$HOSTNAME" == "peregrine.hpc.rug.nl" ]]; then

  echo "On Peregrine, login node"
  for filename in $(find . | egrep "parameters\.RDa")
  do
    echo $filename
    sbatch ./scripts/run_r_cmd "razzo::run_razzo_from_file(\"$filename\")"
  done

elif [[ "$HOSTNAME" =~ ^pg-node.*$ ]]; then

  echo "On Peregrine, worker node"
  for filename in $(find . | egrep "parameters\.RDa")
  do
    echo $filename
    sbatch ./scripts/run_r_cmd "razzo::run_razzo_from_file(\"$filename\")"
  done

elif [[ "$HOSTNAME" == "sonic" ]]; then

  echo "Working from laptop"
  for filename in $(find . | egrep "parameters\.RDa")
  do
    echo $filename
    ./scripts/run_r_cmd "razzo::run_razzo_from_file(\"$filename\")"
  done

else

  echo "On some unknown environment"
  for filename in $(find . | egrep "parameters\.RDa")
  do
    echo $filename
    ./scripts/run_r_cmd "razzo::run_razzo_from_file(\"$filename\")"
  done

fi



