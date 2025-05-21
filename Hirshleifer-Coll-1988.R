# file name: Hirshleifer-Coll-1988.R
# author: Alexander Karl
# create date: 2025-02-23
# last update: 2025-05-20
# This file is part of the Social-Simulation-Replication repository: https://github.com/Mocklerough/Social-Simulation-Replications

# Authors: Jack Hirshleifer, Juan Carlos Martinez Coll
# Date: 1988
# Article: What Strategies Can Support the Evolutionary Emergence of Cooperation?
# Journal: Journal of Conflict Resolution, Vol. 32, No. 2 (Jun, 1988), pp. 367-398
# DOI: https://doi.org/10.1177/0022002788032002006
# Abstract:
#   Axelrod found TIT-FOR-TAT to be a highly successful strategy in the Prisoners' Dilemma payoff environment. 
#   He concluded that a natural selection process in favor of TIT-FOR-TAT explains the evolutionary emergence of cooperation. 
#   This article shows that, contrary to Axelrod, TIT-FOR-TAT does not approach 100% fixation in the population. 
#   More generally, TIT-FOR-TAT is not a robustly successful strategy if Axelrod's exact assumptions do not apply--for example, 
#   if there is a cost of complexity or a probability of error, or when players compete in an elimination contest rather than a round-robin tournament. 
#   is unreasonable to expect any single strategy to win out in evolutionary competition. 
#   Constructively, we show that the presence of a PUNISHER strategy typically generates, 
#   consistent with observation, an interior equilibrium in which more and less cooperative strategies simultaneously coexist.

# Overhead ----------------------------------------------------------------

rm(list = ls())
library(tidyverse) # tibble, dplyr, tidyr, ggplot2
library(gridExtra) # assist ggplot2 graphs

# Helper Functions --------------------------------------------------------

# Construct payoff matrix from given parameters in Table 1
build_payoff_mx <- function(strategies, payoff_data) {
  # INPUTS:
  #   strategies: chr, names of strategies
  #   payoff_data: values to fill matrix, from Table 1
  # OUTPUT:
  #   payoff matrix, as described in Table 1. (`a` in EQ[1-3])
  #     - rows: ego's strategy
  #     - cols: alter's strategy
  #     - cells: ego's payoff against alter
  stopifnot(length(strategies) ^ 2 == length(payoff_data))                      # error check, payoff_data must fit the strategies matrix
  payoff_mx <- matrix(data = payoff_data,                                       # matrix calculating payoff of each strategy i agaist strategy j
                      byrow = TRUE,
                      nrow = length(strategies), ncol = length(strategies), 
                      dimnames = list(strategies, strategies))
  return(payoff_mx)
}

# Calculate change in population at each time period
calc_pop_change <- function(payoff_mx, population, k) { 
  a <- payoff_mx
  p <- population
  Y <- colSums(p * t(a))     # EQ[2] Calculate Y: average payoff yields (vector, per strategy)
  M <- sum(p * Y)            # Eq[3] Calculate M: overall/societal mean of Y yields (atomic)
  delta_p <- k * p * (Y - M) # EQ[1] Calculate delta_P: change in the population at each time step (vector, per strategy)
  return(delta_p)
}


# Simulation Function -----------------------------------------------------
simulate_hirshleifer_coll <- function(payoff_mx, start_pop, k, generations) {
  # INPUTS:
  #   sim_data: List containing strategies, starting population, and payoff matrix. See build_payoff_mx
  #   k: sensitivity, how much to change the population based on its performance
  #   generations: number of iterations to run the simulation for
  # OUTPUT:
  #   matrix:
  #     - rows: each strategy
  #     - cols: each timestep from 0 to `generations`
  #     - cells: strategy's population at that timestep
  strategies  <- rownames(payoff_mx)                                            # pull out data from payoff_mx
  results <- matrix(data = NA, ncol = length(strategies), nrow = generations + 1)
  colnames(results) <- strategies
  
  results[1,] <- start_pop  # calculate population at each timestep # note: generation = 0 is row = 1
  
  for (g in 1:generations) {
    pop_change <- calc_pop_change(payoff_mx, results[g,], k)                    # use the current population to calculate next population
    results[g+1,] <- results[g,] + pop_change
  }
  
  results <-                                                                    # tidy data to use for plotting
    results |>
    as_tibble() |>
    mutate(generation = 0:generations) |>                                       # add column indicationg generation number
    pivot_longer(!generation, names_to = "strategy", values_to = "population")  # reshape data, now each combinatino of strategy-poulation-generation is one line
  
  return(results)
}

# Define Strategies -------------------------------------------------------

TABLE_1 <- list()                                                               # strategies as described in original paper

# MATRIX I
TABLE_1$MATRIX_I <- 
  build_payoff_mx(
    strategies = c("COOPERATE", "DEFECT"),
    payoff_data = c(3,1,
                    4,2))
# MATRIX II
TABLE_1$MATRIX_II <- 
  build_payoff_mx(
    strategies = c("COOPERATE", "DEFECT"),
    payoff_data = c(3,0,
                    5,1))
# MATRIX III
TABLE_1$MATRIX_III <- 
  build_payoff_mx(
    strategies = c("DEFECT", "TIT4TAT"),
    payoff_data = c(1,1,
                    1,3))


# MATRIX VI
TABLE_1$MATRIX_IV <- 
  build_payoff_mx(
    strategies = c("COOPERATE", "DEFECT", "TIT4TAT"),
    payoff_data = c(3,0,3,
                    5,1,1,
                    3,1,3))

# MATRIX V
TABLE_1$MATRIX_V <- 
  build_payoff_mx(
    strategies = c("COOPERATE", "DEFECT", "TIT4TAT"),
    payoff_data = c(3,  0,  3,
                    5,  1,  1,
                    2.5,0.5,2.5))
# MATRIX VI
TABLE_1$MATRIX_VI <- 
  build_payoff_mx(
    strategies = c("COOPERATE", "DEFECT", "TIT4TAT"),
    payoff_data = c(3,  0,  2.7,
                    5,  1,  1.3,
                    3.2,0.9,2.89))

# Run Simulations ---------------------------------------------------------


results <- list()
plot_list <- list()
for (mx_index in 1:length(TABLE_1)) {                                           # for each strategy represented in Table I of the article,
  mx_name <- names(TABLE_1[mx_index])                                           # strategy names (cooperate, defect, etc)
  mx <- TABLE_1[[mx_index]]
  results[[mx_name]] <- 
    simulate_hirshleifer_coll(                                                  # run the above defined simulation  function to find matrix payoffs
      payoff_mx = mx, 
      start_pop = rep(1/nrow(mx), nrow(mx)),                                    # assume equal pop
      k = 0.01, 
      generations = 500)
  plot_list[[mx_index]] <-                                                      # create table fit for grid.arrange() function for plotting
    results[[mx_name]] |>
    ggplot(aes(x = generation, y = population, color = strategy)) +
    geom_line() +
    scale_color_manual(values = c("COOPERATE" = "blue", "DEFECT" = "red", "TIT4TAT" = "green")) +
    labs(
      title = mx_name,
      x = "Generation",
      y = "Population")
}
grid.arrange(grobs = plot_list, ncol = 2, nrow = 3)
