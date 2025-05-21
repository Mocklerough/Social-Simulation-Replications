# kitts_2006.R
# Replication of Kitts 2006, "Collective Action, Rival Incentives, and the Emergence of Antisocial Norms"
# Author: Alexander Karl
# Create Date: 2025-04-29

# MODIFICATION: The original paper describes a process of looping through a list in random order, updating an individual's
# decision to work/shirk, being asynchronously updated between members.
# Instead, I update everyone at once.
# While I can argue theoretical motivation that all actors are constantly re-evaluating in parallel,
# this change is because it's how I initially implemented the model, since R's vector algebra makes it easy.

# Setup -------------------------------------------------------------------

rm(list = ls())


# Constants ---------------------------------------------------------------


mu <- NA # mu: total value of the incentive. >=0
theta <- runif(n = n_members, min = 0, max = n_members) |> floor() |> min(n_members - 1) # range is 0:(n-1), handle the infinitessimal case runif generates exactly the max 
e <- NA # cost of promoting
alpha <- NA # group cohesiveness. 0-1 inclusive


# Helper Functions --------------------------------------------------------

production_func <- function(w, g, c) {
  # INPUTS: see utility_function()
  # OUTPUT: vector of net benefit to each member based on all member's work choice
  n <- sum(w) # total working
  n_exclusive <- n - w # total working other than self
  production <- n_exclusive * g + (g - c) * w
  return(production)
}

reward_func <- function(w, mu, lambda) {
  # INPUTS: see utility_function()
  # OUTPUT: vector of each member's reward
  n <- sum(w) # total working
  n_exclusive <- n - w # total working other than self
  reward <- mu * (1 - lambda * n_exclusive / (n_exclusive + 1)) * w
  return(reward)
}

utility_func <- function(w, g, c, mu, lambda) {
  # INPUTS:
  #   w: vector of work choices by actor. 0/1
  #     w[i] represents a single actor's choice
  #   g: parameter representing i's benefit created by each member's work
  #   c: cost of working
  #   mu: total value of the incentive. >=0
  #   lambda:rivalness of the incentive. 0-1 inclusive
  # OUTPUT: sum of production and reward function, given each member's work choice
  utility <- production_func(w, g, c) + reward_func(w, mu, lambda)
  return(utility)
}

inclination_func <- function(w, g, c, mu, lambda) {
  # INPUTS: same as utility_function()
  # OUTPUT: vector of member's inclination to work
  n <- sum(w) # total working
  n_exclusive <- n - w # total working other than self
  inclination <- (g - c) + mu * (1 - lambda * n_exclusive / (n_exclusive + 1))
  return(inclination)
}

regulatory_interest_func <- function() {
  # INPUTS: same as utility_function()
  # OUTPUT: each member's motivation to encourage or discourage others to work
  regulatory_interest <- g - w * lambda * mu / ((n + 1)^2)
  return(regulatory_interest)
}

payoff_promote_func <- function(w, g, c, mu, lambda, e) {
  # INPUTS: same as utility_function()
  # OUTPUT" each member's expected payoff for promoting
  n <- sum(w)
  n_exclusive <- n - w
  # calculate n+: number of workers member i expects to work if they promote
  n_plus <- min(n_exclusive + theta, n_workers - 1)

  payoff_promote <- 
    (n_plus - n_exclusive) * g 
      - ((n_plus / (n_plus + 1)) - (n_exclusive / (n_exclusive + 1))) * lambda * mu * w 
      - e
  return(payoff_promote)
}

payoff_oppose_func <- function(w, g, c, mu, lambda, e) {
  # INPUTS: same as utility_function()
  # OUTPUT" each member's expected payoff for opposing
  n <- sum(w)
  n_exclusive <- n - w
  n_minus <- max(0, n_exclusive - theta)
  payoff_oppose <- 
    (n_minus - n_exclusive) * g 
    - ((n_minus / (n_minus + 1)) - (n_exclusive / (n_exclusive + 1))) * lambda * mu * w 
    - e
}

decide_enforcement <- function(w, g, c, mu, lambda, e) {
  # Each member decides whether it is better to support, oppose, or do nothing
  payoff_promote <- payoff_promote_func(w, g, c, mu, lambda, e)
  payoff_promote_less_cost <- payoff_promote - e
  payoff_oppose <- payoff_oppose_func(w, g, c, mu, lambda, e)
  payoff_oppose_less_cost <- payoff_oppose - e
  enforcement <- c()
  for (i in 1:length(w)){
    if (payoff_promote_less_cost[i] <= 0 & payoff_oppose_less_cost[i] <= 0) {
      enforcement[i] <- 0 # abstain
    } else if (payoff_promote_less_cost == payoff_oppose_less_cost) {
      enforcement[i] <- 0 # abstain
    } else if (payoff_promote_less_cost > payoff_oppose_less_cost) {
      enforcement[i] <- 1 # promote
    } else if (payoff_promote_less_cost < payoff_oppose_less_cost) {
      enforcement[i] <- -1 # oppose
    } else {
      enforcement[i] <- NA # indicates error in conditional logic
    }
  }
  return(enforcement)
}

valence_func <- function(enforcement) {
  # OUTPUT: Valence - the total social pressure encouraging promotion or opposition
  valence <- sum(enforcement)
  return(valence)
}

work_decision_func <- function(w, g, c, mu, lambda, e) {
  # OUTPUT: vector of each member's decision to work (TRUE) or shirk (FALSE)
  enforcement <- decide_enforcement(w, g, c, mu, lambda, e)
  valence <- valence_func(enforcement)
  inclination_to_work <- inclination_func(w, g, c, mu, lambda)
  w <- (alpha * valence + (1 - valence) * inclination_to_work) > 0
  return(w)
}

# sequential_decision_func <- function(w) {
#   # this function would allows actors to sequentially change their work decision
#   # instead of the work_decision_func, all at once
#   decision_order <- sample(w, length(w))
#   for each member in decision_order:
#     determine the current state
#     determine current member's decision
#     update w[i] - this will the effect the decision of member + 1
#   return(w)
#  }

simulation_func <- function(n_members, g, c, mu, alpha, e, lambda, steps) {
  # this runs a single run of the simulation
  # I didn't catch how the decision to work or shirk were made initially.
  # I'll assume a random 50/50 shot for all actors
  
  # TODO: I hit my time limit I set for this assignment, there's an error in the sim logic
  # about when/where to recalculate decision and enforcement values.
  # Plus some issues with the simulation set-up.
  
  w <- rbinom(n = 1, size = n_members) # 50/50 shot for everyone
  for (i in 1:steps) {
    w <- work_decision_func(w, g, c, mu, lambda, e)
  }
  participate_prop <- mean(w)
  return(participate_prop)
}


factorial_simulation_func <- function(steps = 10000) {
  # run the simulation experiment multiple times with different parameter values
  # INPUT:
  #   steps: the number of times to re-calculate work decisions within a simulation.
  #       set as 10,000 by Kitts, making it weasy to change for model development
  #       but holding constant at 10 000 for final results
  # output: a table of values from each simulation
  
  # these values are constant across all values as set by Kitts 2006
  g <- 1          # parameter representing i's benefit created by each member's work
  c <- 5          # cost of working
  n_members <- 10 # number of members in the simulation
  e <- 1 # I think I missed what value this was defined as.
  
  results <- list()
  results_idx <- 0 
  for (lambda in 0:1) { # non-rival vs. maximal rival
    for (mu in 1:50) { # worthless to valuable incentives
      for (alpha in seq(0, 0.98, by = 0.02)) { # no to almost total peer influence
        results_idx <- results_idx + 1
        participate_prop <- simulation_func((n_members, g, c, mu, alpha, e, lambda, steps))
        results[[results_idx]] <- c(lambda, mu, alpha, participate_prop)
      }
    }
  }
  results <- data.frame(results, row.names = c("lambda", "mu", "alpha", "participate_prop"))
  return(results)
}

sim_results <- factorial_simulation_func(steps = 5)


