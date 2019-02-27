#!/bin/bash
#SBATCH -J r
#SBATCH -o r_%j.txt
#SBATCH -p standard-mem-m
#SBATCH --exclusive
#SBATCH --mem=250G

module purge
module load r

R --vanilla < sim_a_3.R

