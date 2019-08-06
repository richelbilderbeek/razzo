#!/bin/bash
# Create file with all collected evidences
#
# Usage, locally:
#
#   ./scripts/9_create_marg_liks_file
#
# Usage, on Peregrine:
#
#   sbatch ./scripts/9_create_marg_liks_file
#
# Peregrine directives:
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=9_create_marg_liks_file
#SBATCH --output=9_create_marg_liks_file.log
module load R
Rscript -e 'razzo::create_marg_liks_file()'
