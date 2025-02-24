# file name: Kollock-1993.R
# author: Alexander Karl
# date: "2025-02-17"
# This file is part of the Social-Simulation-Replication repository: https://github.com/Mocklerough/Social-Simulation-Replications

# Authors: Peter Kollock
# Date: 1993
# Article: "An eye for an eye leaves everyone blind": Cooperation and accounting systems
# Journal: American Sociological Review, 768–786
# DOI: https://doi.org/10.2307/2095950
# Abstract:
# Explores tit-for-tat type cooperation and the benefits of alternative strategies in which actors use different accounting systems to 
#   track ongoing exchanges by means of computer simulations that permit degrees of cooperation and that introduce noise into the environment. 
#   Conditions were charted under which cooperation may emerge when actors can show degrees of cooperation and when actors' moves are misperceived. 
#   Results demonstrate the significant effects of noise and the usefulness of a Prisoner's Dilemma game allowing degrees of cooperation. 
#   Findings provide evidence that strategies employing a more relaxed accounting system than tit-for-tat have many advantages. 
#   For example, the ability to dampen cycles of recrimination outweighs the increased vulnerability to exploitation in many environments. 


# Overhead ----------------------------------------------------------------

rm(list=ls())
library(tidyverse)


# Set Parameters ----------------------------------------------------------

noise_params = seq(from = 0, to = 1, by = 0.1)
rounds <- 100      # number of rounds in each prisoner's dilemma round
repetitions <- 10  # number of times each pair of strategies plays the game


# Strategies --------------------------------------------------------------
# These are anonymous functions defining each strategy. 
# `x` argument represents the vector of the partners (perceived) contributions
# `strategies$T4T(x)` calculates agent's contribution using the T4T strategy
#   INPUT: x: vector of partner's history perceived contributions so far (length(x) means no history yet)
#   OUTPUT: integer, agent's current contribution
strategies <- list(
  CYCLE     = \(x) sample(seq(0,10,1),1),
  TFT       = \(x) if (length(x) < 1) 10 else x[length(x)],
  SUS_TFT   = \(x) if (length(x) < 1)  0 else x[length(x)],
  TFT_plus  = \(x) if (length(x) < 1) 10 else x[length(x)] + 1,
  TFT_minus = \(x) if (length(x) < 1) 10 else x[length(x)] - 1,
  TF2T_max  = \(x) if (length(x) < 2) 10 else max(x[length(x)], x[length(x) - 1]),
  TF2T_min  = \(x) if (length(x) < 2) 10 else min(x[length(x)], x[length(x) - 1])
)



# Helper Functions --------------------------------------------------------

# Calculates points gained by one participant in a single round of the modified PD
earn_points <- function(actor_contribution, partner_seen_contribution) {
  
  actor_points <- 10 - actor_contribution + 2 * partner_seen_contribution
  
  return(actor_points)
}

# Produces noise to add to results based on frequency argument
make_noise <- function(contribution, freq) {
  
  if (runif(1) < freq) {
    noise <- sample(c(-5, -4, -3, -2, -1, 1, 2, 3, 4, 5), size = 1)
  } else {
    noise <- 0
  }
  
  contribution <- contribution + noise              # apply noise
  contribution <- contribution |> min(10) |> max(0) # restrict to range (0, 10)
  
  return(contribution)
}


# Simulation Functions ----------------------------------------------------

# Simulate a single instance of the Kollock's modified Prisoner's Dilemma game.

kollock_PD <- function(x.strategy, y.strategy, noise, rounds) {
  # Run a full game of Kollock's modified Prisoner Dilemma game
  # INPUTS:
  #   x.strategy: function, describles the first player's strategy as described by Kollock 1993
  #   y.strategy: same as x.strategy. 
  #     Note: using `x`/`y` and not `actor`/`participant` as Kollock used, as one pair's actor is another pair's participant, and vise-versa
  #   noise: proportion, chance of contributions being distorted. Range (0,1)
  #   rounds: number of rounds to play
  # OUTPUT:
  #   list of 2: x's and y's average payoff-per round
  
  # record actual contributions by each player
  x.c.actual <- c()
  y.c.actual <- c()
  
  # record how many points each appears to gives to the other
  x.c.seen <- c()
  y.c.seen <- c()
  
  # Record how many points the players gain each turn
  x.points <- c()
  y.points <- c()
  
  # Simulate each round
  for (r in 1:rounds) {
    
    # Each player decides contribution, based on their strategy and pair's contribution
    x.c.actual[r] <- make_noise(x.strategy(y.c.seen), noise)
    y.c.actual[r] <- make_noise(y.strategy(x.c.seen), noise)
    
    # Each player sees a contribution, with error
    x.c.seen[r] <- x.c.actual[r]
    y.c.seen[r] <- y.c.actual[r]
    
    # Assign points 
    x.points[r] <- earn_points(x.c.actual[r], y.c.seen[r])
    y.points[r] <- earn_points(y.c.actual[r], x.c.seen[r])
  }
  
  # return each player's average payoff per round
  payoffs <- list(
    x = mean(x.points),
    y = mean(y.points)
  )
  return(payoffs)
}

# Executes the full PD game, for each pair of strategies, for each noise level, for the given number of repetitions
sim_kollock_PD <- function(strategies, noise_params, rounds, repetitions) {
  # INPUTS
  #   strategies: list of functions describing PD strategies
  #   noise_params: vector of % chance of a distortion, Range: (0,1)
  #   rounds: number of rounds in each PD game
  #   repetitions: number of times each pair plays against each other
  # OUTPUT
  #   Tibble with one row per agent/partner/noise combination, and their average payoff
  #     Note: this table was designed following "tidy" data principles (i.e. tidyverse compliant)
  
  # set empty list to hold results
  # much more efficient than setting it as `= list()` and changing size every loop (reallocating memory under the hood)
  total_games <- sum(seq(length(strategies))) * length(noise_params) * repetitions * 2
  PD_results_list <- vector(mode = "list", length = total_games)
  PD_index <- 0
  
  strategy_names <- names(strategies)
  n_strategies <- length(strategies)
  
  # set time
  for (i in 1:n_strategies) {                                          # for each pair of strategies,
    for (j in 1:n_strategies) { 
      if (i <= j) {                                                    # (unless pair was already played)
        for (noise in noise_params) {                                  # for each noise level,
          for (r in 1:repetitions) {                                   # for a number of repetitions,
            x.strategy_name <- strategy_names[i]
            x.strategy <- strategies[[i]]
            y.strategy_name <- strategy_names[j]
            y.strategy <- strategies[[j]]
            
            payoffs <- kollock_PD(x.strategy, y.strategy, noise, rounds) # calculate payoff.
            
            PD_index <- PD_index + 1                                     # - assign payoff to agent X against partner Y
            PD_results_list[[PD_index]] <- 
              list(strategy = x.strategy_name, partner = y.strategy_name, 
                   noise = noise, payoff = payoffs$x)
            
            PD_index <- PD_index + 1                                    # - assign payoff to agent Y against partner X
            PD_results_list[[PD_index]] <- 
              list(strategy = y.strategy_name, partner = x.strategy_name, 
                   noise = noise, payoff = payoffs$y)
          }
        }
      }
    }
  }
  
  # Average repetition results and return
  PD_results <-
    PD_results_list |>
    bind_rows() |>                           # convert `list` to `tibble` data frame
    group_by(strategy, partner, noise) |>    # summarize multiple repetitions
    summarize(payoff = mean(payoff), .groups = "drop") |>
    mutate(noise = noise * 100)              # interpret noise as a %
  return(PD_results)
}


# Run Simulations ---------------------------------------------------------

start_time <- Sys.time()
PD_results <- sim_kollock_PD(strategies, noise_params, rounds, repetitions)
print(Sys.time() - start_time)


# Plot Results ------------------------------------------------------------

# Plot Performance Against Self
PD_results |>
  filter(strategy == partner) |>
  ggplot(aes(x = noise, y = payoff, color = strategy, group = strategy)) +
  geom_line() +
  theme_minimal() +
  #ylim(10,20) +
  labs(title = "Seven Strategies Playing Against Clones of Themselves", 
       x = "Frequency of Distortion (%)", 
       y = "Average Payoff per Round")

# Plot Round Robin Performance
PD_results |>
  group_by(strategy, noise) |>
  summarise(payoff = mean(payoff), .groups = "drop") |>
  ggplot(aes(x = noise, y = payoff, color = strategy, group = strategy)) +
  geom_line() +
  theme_minimal() +
  #ylim(13, 20) +
  labs(title = "Seven Strategies Playing Against Each Other", 
       x = "Frequency of Distortion (%)", 
       y = "Average Payoff per Round")