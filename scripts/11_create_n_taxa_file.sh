#!/bin/bash
# Count all number of taxa and collect these in one file
#
# Usage, locally:
#
#   ./scripts/11_create_n_taxa_file
#
# Usage, on Peregrine:
#
#   sbatch ./scripts/11_create_n_taxa_file
#
# Peregrine directives:
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=11_create_n_taxa_file
#SBATCH --output=11_create_n_taxa_file.log
module load R
Rscript -e 'razzo::create_n_taxa_file()'
