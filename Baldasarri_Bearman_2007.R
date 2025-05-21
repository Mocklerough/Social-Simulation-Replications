# Baldasarri_Bearman_2007.R
# Author: Alex Karl
# Create Date: 2025-04-14

# Article: Dynamics of Political Polarization



# Set-up ------------------------------------------------------------------

rm(list = ls())


# User-Defined Variables --------------------------------------------------

# use index to identify each actor across vars

n_actors <- 100
n_issues <- 4
issue_mean <- 0
issue_sd <- 33.3
issue_bounds <- c(-100,100)
timesteps <- 100

# set 4 sets of ideologies for each of 100 actors
ideo_list_t0 <- list()
for (i in 1:n_issues) {
  ideo_list_t0[[i]] <- rnorm(n = 100, mean = issue_mean, sd = issue_sd)
  # check for ideology scores out of range
  bad_ideo_indices <- (ideo_list_t0[[i]] < issue_bounds[1]) | (ideo_list_t0[[i]] > issue_bounds[2])
  while (sum(bad_ideo_indices) > 0) { q
    ideo_list[[i]][bad_ideo_indices] <- rnorm(n = sum(bad_ideo_indices), mean = issue_mean, sd = issue_sd)
    bad_ideo_indices <- (ideo_list_t0[[i]] < issue_bounds[1]) | (ideo_list_t0[[i]] > issue_bounds[2])
  }
}


# calculate actual ideological distance
real_ideo_dist_t0 <- matrix(NA, nrow = n_actors, ncol = n_actors)
for(i in 1:n_actors) {
  for (j in 1:n_actors) {
    if (i > j) {
      euc_dist <- 
        sqrt(
          (ideo_list[[1]][i] - ideo_list[[1]][j])^2 + 
          (ideo_list[[2]][i] - ideo_list[[2]][j])^2 + 
          (ideo_list[[3]][i] - ideo_list[[3]][j])^2 + 
          (ideo_list[[4]][i] - ideo_list[[4]][j])^2
        )
      real_ideo_dist_t0[i,j] <- euc_dist
      real_ideo_dist_t0[j,i] <- euc_dist
    }
  }
}
max_ideo_dist_t0 <- max(real_ideo_dist_t0, na.rm = TRUE)
real_ideo_dist_t0 <- real_ideo_dist_t0 / max_ideo_dist_t0

# set default perceived ideological distance
avg_ideo_dist_t0 <- mean(real_ideo_dist_t0, na.rm = TRUE)
perc_ideo_dist_t0 <- matrix(avg_ideo_dist_t0, nrow = n_actors, ncol = n_actors)
# No dist between self
for (i in 1:n_actors) {
  perc_ideo_dist_t0[i,i] <- NA # no value for self-self
}


# Interest func -----------------------------------------------------------

homophily_func <- function(i,j,t) {
  homophily <- sqrt(sum((perc_ideo_dist_t0[[i]] - perc_ideo_dist_t0[[j]] )^2)) / max_ideo_dist_t0
  # update homophily data
  perc_ideo_dist[t][i,j] <- homophily
  return(homophily)
}

interest_func <- function(i, j, t) {
  # calculate i's interest in j (at time t)
  n_ <- 1 # not sure what this parameter is meant to be
  n_ * ( mean(abs(ldeo_list[i]))^2 + mean(abs(ldeo_list[j]))^2 ) * ( 1 - homophily_func(i,j,t))
  
}
# Simulation  -------------------------------------------------------------

# record percieved ideological distance over time
perc_ideo_dist_list <- list()

for (t in 1:timesteps) {
  # set perceived ideological distance at t to last timestep's value
  if (t == 1) {
    perc_ideo_dist_list[[1]] <- perc_ideo_dist_t0
  } else {
    perc_ideo_dist_list[[i]] <- perc_ideo_dist_list[[i-1]]
  }
  # take a random sample of potential interlocutors
  # NOTE: skipping the level of interest
  # NOTE: for ease of sake, reducing selection: each person looks at 10 random people, then selects the one they want to interact with most
  # NOTE: this implies everyone selects one other, and everyone can be selected any number of times. E(interactions) = 2, per person
  interaction_pairs <- data.frame(row.names = c("ego", "alter")) # we slip into psuedocode about here
  row <- 0
  for (i in 1:n_actors) {
    row <- row + 1
    sample_actors <- sample(1:n_actors)
    sample_actors <- sample_actors[sample_actors != i] # may cut out one. Ought to code so that a replacement is selected
    sample_actors_score <- c()
    for (j in sample_actors) {
      sample_actors_score <- append(sample_actors_score, interest_func(i, j, t))
    }
    # select weighted based on sample actor scores
    sample_actors_score <- sample_actors_score / sum(sample_actors_score) # weights, making them sum to one
    # select most preferable match, append to pair df.
    interaction_pairs[row, 1] <- i
    interaction_pairs[row, 2] <- sample_actors[sort(sample_actors_score, decreasing = TRUE)[1]] # sort(desc)[1] gets the index of the best match
  }
}

# then graph past here
