#!/bin/bash
#SBATCH --account=imai_lab #modify this accordingly
#SBATCH -J spillover  # A single job name for the array
#SBATCH -p serial_requeue,sapphire# Partition
#SBATCH -c 1 # number of cores
#SBATCH -t 1-00:00  # Running time in the format - D-HH:MM
#SBATCH --mem 10000 # Memory request - 1000 corresponds to 1GB
#SBATCH -o out_%a.out # Standard output
#SBATCH -e err_%a.err # Standard error
Rscript run_experiments.R $1 $2 $3 $4