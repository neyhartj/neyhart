#!/bin/bash

# SLURM parameters
# job standard output will go to the file slurm-%j.out (where %j is the job ID)

#SBATCH --time=HH:MM:SS   # walltime limit (HH:MM:SS)
#SBATCH --nodes=1   # number of nodes
#SBATCH --ntasks-per-node=XX   # X processor core(s) per node X 2 threads per core
#SBATCH --mem=XXG   # maximum memory per node
#SBATCH --partition=short    # standard node(s)
#SBATCH --job-name="JOB-TITLE"
#SBATCH --mail-user=jeffrey.neyhart@usda.gov   # email address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL


# Change the working directory
cd 

# Load modules


# Run the script


