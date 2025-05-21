# strang_macy_2001.R
# Replication of Strang & Macy 2001, `In Search of Excellence: Fads, Success Stories, and Adaptive Emulation`
# Author: Alexander Karl
# Create Date :2025-04-29


# Setup -------------------------------------------------------------------

rm(list = ls())

timesteps <- NA
n_firms <- 100
n_innovations <- NA
K <- rep(NA, n = n_firms) # market position, vector over firms
V <- rep(NA, n = n_innovations) # value of innovation, vector over innovations
inertia <- rep(NA, n_firms) # tendency to change, vector over firms
skepticism <- rep(NA, n_firms) # readiness to draw conclusions from observed outcomes, vector over firms



# Helper Functions --------------------------------------------------------

outcome_func <- function(K, V) {
  # INPUTS:
  #   kappa: market positin of the firm
  #   alpha: 
  outcomes <- matrix(NA, dims = c(n_firms, timesteps, n_innovations))
  for (f in 1:n_firms) {  # for each firm
    for (t in 1:timesteps) { # for each timestep
      for (i in 1:n_innovations) {
        outcomes[[f, t, i]] <- alpha * K[f] + beta * V[i] + (1 - alpha - beta) * epsilon[f,t]
      }
    }
  }
  return(outcomes)
}

O_bar <- 

abandonment_decision_func <- function(f,i,t) {
  abandonment_prob <- (1 - O[[f,t,i,m]] * C[[f,i,t]]
}

prob_adopt_func <- func(A, D, )
