#!/bin/bash

echo "starting run.sh"

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.

Rscript run.R

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
rm -f .RData

echo "ending run.sh"
