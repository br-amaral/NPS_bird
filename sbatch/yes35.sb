#!/bin/bash --login
#SBATCH --time=02:00:00
#SBATCH --mem=32G
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=FAIL,END
#SBATCH --mail-user=bamaral@msu.edu

cd $SLURM_SUBMIT_DIR

module purge
module load R-bundle-CRAN/2023.12-foss-2023a

# Install packages if needed (run once)
# Rscript -e "install.packages(c('tidyverse', 'conflicted', 'glue', 'MCMCvis', 'viridis', 'svglite', 'ggh4x'), repos='https://cran.r-project.org/')"

# Run your script
Rscript code/fit_model/3yes.r
Rscript code/fit_model/5yes.r