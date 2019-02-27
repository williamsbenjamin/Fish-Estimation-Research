#!/bin/bash
#SBATCH -J r
#SBATCH -o r_%j.txt
#SBATCH -p development
#SBATCH --exclusive
#SBATCH --mem=250G

module purge
module load r

R --vanilla < source_m2.R

