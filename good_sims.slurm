#!/bin/bash
#SBATCH --job-name="good sims"          # Name that appears in queue
#SBATCH --partition=small               # Resource group (small/medium/large)
#SBATCH --account=lotspeichGrp          # Research group
#SBATCH --nodes=1                       # Number of Nodes
#SBATCH --ntasks-per-node=1             # Number of CPUS
#SBATCH --mem=16G                       # Requested memory
#SBATCH --time=00-06:00:00              # Job duration in DD-HH:MM:SS
#SBATCH --output="good_sims-%j.out"     # Slurm stdout, %j is the job number

hostname

module load R/4.2.1

# Change to directory where .R script exists
cd /deac/sta/lotspeichGrp/mullae22/712_sims

# Execute add.m script
R CMD BATCH good_sims.R

slurm_mem_report -g
