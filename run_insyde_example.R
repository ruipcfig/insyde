# Example script to run INSYDE using the default settings

library(truncnorm)

# Change working directory if necessary
# setwd(".")

# Load INSYDE main function
source("insyde_function.R")

# Read hazard and exposure variables
source("hazard_variables.R")
source("exposure_variables.R")

# Read unit prices
up <- read.table("unit_prices.txt")

# Read replacement values
repValData <- read.table("replacement_values.txt", header = TRUE)
repVal <- repValData[BS, BT]

# Define whether or not to consider uncertainty
uncert <- 0

if (!uncert) {
  # Compute expected damage. Note that (only) one of the hazard variables can 
  # be passed to the function as a vector.
  modelOutput <- ComputeDamage(he, v, d, s, q, 
                  FA, IA, BA, EP, IH, BH, GL, NF, BT, BS, PD, PT, FL, YY, LM, 
                  repVal, up, uncert)
} else if (uncert) {
  # Probabilistic computation. All the hazard variables must be passed to the
  # function as scalars.
  # This example assumes that the variable he is a vector. It is therefore
  # necessary to iterate over its elements and pass them to the function one
  # at a time.
  nrSim <- 2000
  statMat <- matrix(NA, nrow = length(he), ncol = 4)
  for (i in 1:length(he)) {
    modelOutput <- ComputeDamage(he[i], v, d, s, q, 
                    FA, IA, BA, EP, IH, BH, GL, NF, BT, BS, PD, PT, FL, YY, LM,
                    repVal, up, uncert, nrSim)
    # For each element of he, calculate some summary statistics and save them
    # to a matrix.
    statMat[i, 1] <- quantile(modelOutput$absDamage, .05)
    statMat[i, 2] <- mean(modelOutput$absDamage)
    statMat[i, 3] <- quantile(modelOutput$absDamage, .95)
    statMat[i, 4] <- mean(modelOutput$relDamage)
  }
}