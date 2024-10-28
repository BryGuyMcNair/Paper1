

#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)
iternum = as.numeric(args[[1]]) + 100

montecarloPi <- function(trials) {
  count = 0
  for(i in 1:trials) {
    if((runif(1,0,1)^2 + runif(1,0,1)^2)<1) {
      count = count + 1
    }
  }
  return((count*4)/trials)
}

montecarloPi(iternum)



# $ apptainer shell \
# /cvmfs/singularity.opensciencegrid.org/opensciencegrid/osgvo-r:3.5.0
# Singularity :~/tutorial-ScalingUp-R> Rscript mcpi.R 10
# [1] 3.14
# Singularity :~/tutorial-ScalingUp-R> exit
# $ 