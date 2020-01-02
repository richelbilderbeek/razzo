#!/bin/bash
#
# Regenerate/re-create all data
#
# Usage, locally:
#
#  # Create default parameter set and results
#  ./scripts/regen_data
#
#  # Create testing parameter set and results
#  ./scripts/regen_data test
#
#  # Create full parameter set and results
#  ./scripts/regen_data full
#
# Usage, on Peregrine:
#
#  # Create default parameter set and results
#  sbatch ./scripts/regen_data
#
#  # Create testing parameter set and results
#  sbatch ./scripts/regen_data test
#
#  # Create full parameter set and results
#  sbatch ./scripts/regen_data full
#
# Peregrine directives:
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=10G
#SBATCH --job-name=regen_data
#SBATCH --output=regen_data.log

experiment_type=$1
if [ "$#" -ne 1 ]; then
  experiment_type=test
fi

time ./scripts/1_install_razzo.sh
time ./scripts/2_create_parameter_files.sh test
time ./scripts/3_run_razzo.sh
time ./scripts/7_create_nltt_stats_file.sh
time ./scripts/8_create_esses_files.sh
time ./scripts/9_create_marg_liks_file.sh
time ./scripts/11_create_n_taxa_file.sh

# This one is shakey, do it last
time ./scripts/10_create_fig_1_file.sh
