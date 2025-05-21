# file name: Dimaggio_Gario_2011.R
# author: Alexander Karl
# create date: 2025-04-01
# last updated: 2025-05-16

# Article: How Network Externalities Can Exacerbate Intergroup Inequality1
# Authors: Paul DiMaggio and Filiz Garip
# Date: 2011
# Journal: American Journal of Sociology


# Overhead ----------------------------------------------------------------

rm(list = ls())


# Constants ---------------------------------------------------------------

kappa = 0.1 # multiplicative constant for the pure income effect
delta = 0.1 # multiplicative constant for network effects
alpha = 0.5 # exponent of income
gamma = 0.5 # exponent of proportion of adopters

p_0 <- 60.00 <- # initial internet price
p_min = 28.74 # equilibriup price level
a = 3.3 / 12 # speed of reversion to p_min (when scaled with n[t-1])

timesteps <- NA                                                                 # number of timesteps to run over (each is about 1 month)
  
# Generate Initial Population ---------------------------------------------

n_agents <- 2257                                                                # population based on article's empirical data

# Couldn't locate/download the GSS 2002 data
# note these are independent + not accurate point estimates
agents <- data.frame(                                                           # generate random data in lieu of GSS data
  id = 1:n_agents,
  race = sample(0:1, size = n_agents, replace = TRUE, prob = c(0.2,0.8)),       # 0/1 for black/white
  income = rlnorm(n_agents, meanlog = log(55000) - 0.5, sdlog = 0.6),           # numeric/lognormal
  educ = sample(1:4, size = n_agents, replace = TRUE, prob = c(0.2,0.4,0.4,0.2)) # unclear, presumably a 4-level ordinal
)


# Social Distance Function ------------------------------------------------

soc_dist <- function(i,j) {                                                     # function to calculate social distance between two agents
  # INPUTS:
  #   i,j: integer, each agent # to compare against each other. 
  if (i == j) {                                                                 # no social distance between one's self
    soc_dist <- NA
  } else {                                                                      # calculate followign article's formula, as a funciton of difference b/w qualitative variables
    w_r <- 0.83 * (agents[i,"race"]      - agents[j,"race"])                    # drawign weights from the article
    w_i <- 0.53 * (agents[i,"income"]    - agents[j,"income"])
    w_e <- 0.53 * (agents[i,"educ"] - agents[j,"educ"])
    soc_dist <- sqrt(w_i^2 + w_e^2 + w_r^2)
  }
  return(soc_dist)
}


# Generate Social Network -------------------------------------------------

soc_dist_mx <- matrix(NA, nrow = n_agents, ncol = n_agents)                     # construct empty matrix to storeagents
colnames(soc_dist_mx) <- 1:n_agents
rownames(soc_dist_mx) <- 1:n_agents

for (i in 1:n_agents) {                                                         # Calculate everyone's social distance from each other 
  for (j in 1:n_agents) {
    soc_dist_mx[i,j] <- soc_dist(i,j)
  }
}

# Under conditions of homophily, networks are generated as follows: 

#We refer to this set of agents as i’s “in-group.” 
# In our application, n is arbitrarily chosen to be three times the target number of relations for that person. 
# Across these five conditions, homophily bias is assigned the following values: 0, 0.25, 0.50, 0.75, and 1.0 
# Even under homophily bias of 1.0, networks will be somewhat heterogeneous because income, education, and race are only moderately correlated, and individuals who satisfy the condition of being “closest” to ego, and therefore eligible for selection, may not be identical to ego on each dimension. 
# Ties between agents are established such that in-group selection occurs with probability h. 
# The probability of forming a tie to an individual from one’s own in-group is given by Pr (T) p h  (1  h) *Pr (T),

# NOTE: somewhere in my simplifications, these fell out of the equation. Presumably in the tie formation logic around Skvoretz
h_vectors <- c(0.0, 0.25, 0.50, 0.75, 1.0) 

# note: need to review paper for how this was set
n_target_ties <- 10

# Assumption: Ties are directional, and i's ties are not formed with j's ties in mind
ties <- matrix(NA, ncol = n_target_ties, nrow = n_agents)                       # define network matrix indicating ties between each pair of agents
for (i in 1:n_agents) {
  n_in_group <- n_target_ties * 3                                               # in-group is 3x target # of ties
  soc_dist_i <- soc_dist_mx[i,]                                                 # get all of i's ties
  soc_dist_i <- soc_dist_i[!is.na(soc_dist_i)]                                  # remove NA (mx[i,i])
  soc_dist_in_group <- sort(soc_dist_i)[1:n_target_ties]                        # grab the lowest soc dist ties
  in_group <- colnames(soc_dist_in_group)
  # Skvoretz 1990 `Biased Net Theory` is paywalled. Substituting with a weighted chance of tie formation
  # note this eq adjusts for inverse correlation between distance and prob of tie formation, though it's arbitrarily scaled
  invert_soc_dist <- sum(soc_dist_i) / soc_dist_i
  tie_prob <- invert_soc_dist / sum(invert_soc_dist)
  ties_i <- sample(in_group, size=n_target_ties, prob = c(invert_soc_dist))
  ties[i,] <- ties_i                                                            # after calculating, assign to ties matrix and return
  return(ties)
}


# Initial Setup -----------------------------------------------------------


p <- c() # vector to hold internet price over time

# set up initial adopter - need to revivew how these were initially defined
agents$adopter <- rbinom(n_agents, size=1, prob=.01)
# calculate # of ties who adopted, for each alter
count_adopting_ties <- function() {
  adopting_ties <- c()
  for (i in 1:n_agents) {
   adopting_ties[i] <- sum(agents[ties[i], "adopter"]) #using ties[i] as the set of row indexes to i's ties' adopter status]]
  }
  return(adopting_ties)
}

  




# Simulate ----------------------------------------------------------------

p_vec <- c()
for (t in 1:timesteps) {                                                          # define reservation prices at time t

  adopting_ties <- count_adopting_ties()
  r <- (kappa * agents$income^gamma) + (agents$income^gamma * delta * adopting_ties^alpha)
  # update price accordingly
  p <- p + A * sum(agents$adopter) * (p_min - p) # should this be subtractive?
  p_vec[t] <- p
  return(p_vec)
}

# report changes over time
plot(1:timesteps, p_vec)




