#!/bin/bash
#SBATCH -J r
#SBATCH -o r_%j.txt
#SBATCH -p mic
#SBATCH --exclusive
#SBATCH --mem=250G

module purge
module load r

R --vanilla < detect_cores.R

