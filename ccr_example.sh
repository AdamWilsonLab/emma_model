#!/bin/sh
#
#SBATCH --time=00:01:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=10000
#SBATCH --job-name="example-debug-job"
#SBATCH --output=example-debug-job.out
#SBATCH --mail-user=bmaitner@gmail.com
#SBATCH --mail-type=end
#SBATCH --partition=debug
#SBATCH --qos=debug
#SBATCH --cluster=ub-hpc

#Let's start some work
echo "Hello world from debug node: "`/usr/bin/uname -n`
#Let's finish some work
