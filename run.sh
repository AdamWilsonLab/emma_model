#!/bin/sh
#
#SBATCH --cluster=faculty
#SBATCH --qos=adamw
#SBATCH --partition=adamw
#SBATCH --job-name "EMMA model pt 2"
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=120G
#SBATCH  -C INTEL
#SBATCH --time=120:00:00
#SBATCH --mail-user=bmaitner@gmail.com
#SBATCH --mail-type=start
#SBATCH --mail-type=end


# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.
nohup nice -4 R CMD BATCH run.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
 rm -f .RData
