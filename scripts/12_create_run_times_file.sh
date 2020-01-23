#!/bin/bash
# Measure all the run times and store these in one file
#
# Usage, locally:
#
#   ./scripts/12_create_run_times_file
#
# Usage, on Peregrine:
#
#   sbatch ./scripts/12_create_run_times_file
#
# Peregrine directives:
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=12_create_run_times_file
#SBATCH --output=12_create_run_times_file.log
module load R
Rscript -e 'razzo::create_run_times_file()'
