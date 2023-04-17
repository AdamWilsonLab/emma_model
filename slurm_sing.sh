#!/bin/bash
#SBATCH --cluster=faculty
#SBATCH --qos=adamw
#SBATCH --partition=adamw
#SBATCH --job-name "targets run"
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=4
#SBATCH --mem=60G
#SBATCH  -C INTEL
#SBATCH --time=1:00:00

  export PROJECT_FOLDER="/projects/academic/adamw/"
  export APPTAINER_CACHEDIR="/scratch/"$USER"/singularity"
  export SIF_PATH=$PROJECT_FOLDER"/users/"$USER"/singularity"
  export SIF_FILE="AdamWilsonLab-emma_docker-latest.sif"

  singularity shell \
  --bind $PROJECT_FOLDER:$PROJECT_FOLDER \
  --bind $APPTAINER_CACHEDIR/tmp:/tmp \
  --bind $APPTAINER_CACHEDIR/run:/run \
  $SIF_PATH/$SIF_FILE ./emma_model/run.sh
  