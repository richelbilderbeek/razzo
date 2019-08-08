#!/bin/bash
#
# Runs the failed experiments with one job per parameter file.
#
# Usage, locally:
#
#   ./scripts/rerun_failed
#
# Usage, on Peregrine:
#
#   sbatch ./scripts/rerun_failed
#
# Peregrine directives:
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=rerun_failed
#SBATCH --output=rerun_failed.log
module load R
module load MPFR

echo "Host name: "$HOSTNAME

# Collect the filenames


filenames=$(cat $(egrep -Rl FAILED | egrep "^run_") | egrep "parameters\.RDa" | egrep -o "\".*\"" | sed -e "s|\"||g" | egrep -v "^\./data" | sort | uniq)

for filename in $filenames
do
  echo $filename
  if [ ! -f $filename ]
  then
    echo "File $filename not found!"
    exit 1
  fi
done

if [[ "$HOSTNAME" == "peregrine.hpc.rug.nl" ]]; then

  echo "On Peregrine, login node"
  for filename in $filenames
  do
    echo $filename
    sbatch ./scripts/run_r_cmd "razzo::run_razzo_from_file(\"$filename\", add_verbose = TRUE)"
  done

elif [[ "$HOSTNAME" =~ ^pg-node.*$ ]]; then

  echo "On Peregrine, worker node"
  for filename in $filenames
  do
    echo $filename
    sbatch ./scripts/run_r_cmd "razzo::run_razzo_from_file(\"$filename\", add_verbose = TRUE)"
  done

elif [[ "$HOSTNAME" == "sonic" ]]; then

  echo "Working from laptop"
  for filename in $filenames
  do
    echo $filename
    ./scripts/run_r_cmd "razzo::run_razzo_from_file(\"$filename\", add_verbose = TRUE)"
  done

else

  echo "On some unknown environment"
  for filename in $filenames
  do
    echo $filename
    ./scripts/run_r_cmd "razzo::run_razzo_from_file(\"$filename\", add_verbose = TRUE)"
  done

fi
