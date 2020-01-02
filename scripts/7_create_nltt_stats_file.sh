#!/bin/bash
# Combine all generated nLTT files to one nLTT file in results
#
# Usage, locally:
#
#   ./scripts/7_create_nltt_stats_file
#
# Usage, on Peregrine:
#
#   sbatch ./scripts/7_create_nltt_stats_file
#
# Peregrine directives:
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=7_create_nltt_stats_file
#SBATCH --output=7_create_nltt_stats_file.log
module load R
Rscript -e 'razzo::create_nltt_stats_file()'
