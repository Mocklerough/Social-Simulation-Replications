# file name: Schelling_1978.R
# author: Alexander Karl
# date: 2025-03-10

# Article: Sorting and Mixing: Race and Sex
# Authors: Thomas C. Schelling
# Date: 1978
# Book: Micromotives and Macrobehavior, 
# Chapter: Chapter 4, pp. 137-166. "A Self-Forming Neighborhood Model"

# Differences from Shelling:
#   grid shape: the grid must be square (shape not specified)
#   agent movement: Each turn, all dissatisfied agents are removed from the board at once, 
#                   then, in a random order, select a new square.
#                   Agents do not remember the square they left, so they might move back with probability 1/#-empty-squares
#                   -> This is a substantive change that may effect results.


# Overhead ----------------------------------------------------------------
rm(list = ls())

# User Inputs -------------------------------------------------------------

blue_size <- 45
red_size <- 45

blue_min_homophily <- 1 / 3
red_min_homophily <- 1 / 3

rounds <- 100
round_time <- 0.1 # seconds; how long to wait between rounds, to see the neighborhood plot change.

# Helper Data/Functions ---------------------------------------------------

# obj to make it easier to grab homophily
min_homophily <- list(blue = blue_min_homophily,
                      red = red_min_homophily)

# function to search graph for an empty space for each agent to move to
find_new_homes <- function(grid_mx, agents) {
  # RETURN: updated grid_mx
  for (agent in agents) {
    while (TRUE) { 
      # randomly search grid to find an empty spot
      # NOTE: computationally inefficient. It's fine for low grid size,
      #   but making a very large grid with few empty squares will have performance issues.
      i <- sample(1:grid_dim, size = 1)
      j <- sample(1:grid_dim, size = 1)
      if (grid_mx[i,j] == "empty") {
        grid_mx[i,j] <- agent
        break
      }
    }
  }
  return(grid_mx)
}

# function to check who is dissatisfied
check_neighbors <- function(grid_mx, min_homophily) {
  # RETURN: matrix of TRUE/FALSE (satisfied/unsatisfied)
  
  # matrix to check satisfaction
  satisfaction_mx <- matrix(data = NA, nrow = grid_dim, ncol = grid_dim)
  
  # check everyone's satisfaction
  for (i in 1:nrow(grid_mx)) {
    for (j in 1:nrow(grid_mx)) {
      # get ij's group
      ij_group <- grid_mx[i,j]
      if (ij_group == "empty") { # skip if no one lives here
        satisfaction_mx[i,j] <- TRUE
      } else {
        # get ij's neighbors as a matrix
        i_coords <- c(i-1,i,i+1)
        i_coords <- i_coords[i_coords > 0 & i_coords <= grid_dim]
        j_coords <- c(j-1,j,j+1)
        j_coords <- j_coords[j_coords > 0 & j_coords <= grid_dim]
        ij_neighbors <- grid_mx[i_coords, j_coords]
        # remove ij
        ij_neighbors[2,2] <- NA
        # calculate neighbor homophily: # similar neighbors / # neighbors
        ij_homophily <- sum(ij_neighbors == ij_group, na.rm = TRUE) / sum(ij_neighbors != "empty", na.rm = TRUE)
        # check and record if ij is satisfied
        satisfaction_mx[i,j] <- ij_homophily >= min_homophily[[ij_group]]
      }
    }
  }
  return(satisfaction_mx)
}

# function to print grid
plot_neighborhood <- function(grid_mx, round) {
  plot_mx <- matrix(data = 0, nrow = grid_dim, ncol = grid_dim)
  plot_mx[grid_mx == "blue"] <- 1
  plot_mx[grid_mx == "red"] <- 2
  #print(grid_mx)
  image(plot_mx, col=c("white", "blue", "red"), main="Neighborhood Grid", xlab = paste("round", round))
}


# Simulation - Setup ------------------------------------------------------

# Define neighborhood grid
grid_dim <- sqrt(red_size + blue_size + 1) |> ceiling() # grid must be large enough have <= 1 empty space
grid_mx <- matrix(data = "empty", nrow = grid_dim, ncol = grid_dim)

# Define population of agents
unassigned_agents <- c(rep("blue", times = blue_size), rep("red", times = red_size))
unassigned_agents <- sample(unassigned_agents) # randomly order

# find initial homes for everyone
grid_mx <- find_new_homes(grid_mx, unassigned_agents)
plot_neighborhood(grid_mx, round = 0)


# Simulation - Run --------------------------------------------------------

for (round in 1:rounds) {
  # check who is dissatisfied
  satisfaction_mx <- check_neighbors(grid_mx, min_homophily)
  # get vector of each unsatisfied individual
  unsatisfied_agents <- as.vector(grid_mx)[!as.vector(satisfaction_mx)] |> sample()
  if (length(unsatisfied_agents) == 0) {
    print(paste0("Everyone is Satisfied. Round: ", round))
    break
  }
  # remove them all at once
  grid_mx[!satisfaction_mx] <- "empty"
  # Assign everyone to a new spot
  grid_mx <- find_new_homes(grid_mx, unsatisfied_agents)

  # show results
  Sys.sleep(round_time)
  plot_neighborhood(grid_mx, round)
}
