#!/bin/bash
# Create file with all collected ESSes
#
# Usage, locally:
#
#   ./scripts/8_create_esses_file
#
# Usage, on Peregrine:
#
#   sbatch ./scripts/8_create_esses_file
#
# Peregrine directives:
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=8_create_esses_file
#SBATCH --output=8_create_esses_file.log
module load R
Rscript -e 'razzo::create_esses_file()'
