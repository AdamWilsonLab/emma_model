#!/bin/bash
#
#SBATCH --cluster=faculty
#SBATCH --qos=adamw
#SBATCH --partition=adamw
#SBATCH --job-name "EMMA model slurm_sing2.sh"
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=60G
#SBATCH  -C INTEL
#SBATCH --time=120:00:00
#SBATCH --mail-user=bmaitner@gmail.com
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH -o singularity_test2.out

  export PROJECT_FOLDER="/panasas/scratch/grp-adamw/"
  export APPTAINER_CACHEDIR="/panasas/scratch/grp-adamw/"$USER"/singularity"
  export SIF_PATH=$PROJECT_FOLDER"/"$USER"/singularity"
  export SIF_FILE="AdamWilsonLab-emma_docker-latest.sif"

  cp -r "/projects/academic/adamw/users/"$USER"/singularity/"$SIF_FILE $SIF_PATH/$SIF_FILE

  mkdir -p "$APPTAINER_CACHEDIR/tmp"
  mkdir -p "$APPTAINER_CACHEDIR/run"

  singularity exec \
  --bind $PROJECT_FOLDER:$PROJECT_FOLDER \
  --bind $APPTAINER_CACHEDIR/tmp:/tmp \
  --bind $APPTAINER_CACHEDIR/run:/run \
  $SIF_PATH/$SIF_FILE ./run_quiet.sh


  echo "v2"

