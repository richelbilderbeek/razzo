#!/bin/bash
# Create input files
#
# Usage, locally:
#
#  # Create input files for default experiment type
#  ./scripts/2_create_parameter_files
#
#  # Create input files for test experiment
#  ./scripts/2_create_parameter_files test
#
#  # Create input files for full experiment
#  ./scripts/2_create_parameter_files full
#
# Usage, on Peregrine:
#
#  # Create input files for default experiment type
#  sbatch ./scripts/2_create_parameter_files
#
#  # Create input files for test experiment
#  sbatch ./scripts/2_create_parameter_files test
#
#  # Create input files for full experiment
#  sbatch ./scripts/2_create_parameter_files full
#
# Peregrine directives:
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=2_create_parameter_files
#SBATCH --output=2_create_parameter_files.log
module load R

experiment_type=$1
if [ "$#" -ne 1 ]; then
  experiment_type=test
fi

echo "Experiment type: "$experiment_type

Rscript -e "razzo::create_parameters_files(experiment_type = \"$experiment_type\")"

