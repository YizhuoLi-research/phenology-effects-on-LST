#!/bin/bash
#SBATCH -J Lyz_2014Rscript  #workingname --Rscript
#SBATCH -p hebhcnormal01
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -o log.%j
#SBATCH -e log.%j
#SBATCH --exclusive

source ~/miniconda3/bin/activate r4.3.2

Rscript /public/home/ac6u713xut/LiYZ/VegetationImpact/2014-nlstest_LST_SumDiff_NA_test.r


