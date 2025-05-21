# file name: Schelling_1978.R
# author: Alexander Karl
# CREATE DATE: 2025-03-10
# LAST UPDATED: 2025-05-19

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

blue_size <- 45                                                                 # number of agents in each group
red_size <- 45

blue_min_homophily <- 1 / 3                                                     # minimum proportion of similar neighbors, agent will move if below this threshold
red_min_homophily <- 1 / 3

rounds <- 100                                                                   # turns in the simulation
round_time <- 0.1                                                               # seconds; how long to wait between rounds, to see the neighborhood plot change.

# Helper Data/Functions ---------------------------------------------------

min_homophily <- list(blue = blue_min_homophily, red = red_min_homophily)       # simple data structure to hold homophily values together

find_new_homes <- function(grid_mx, agents) {
  # function to search graph for an empty space for each agent to move to
  # RETURN: updated grid_mx
  for (agent in agents) {                                                       # every agent (one after the other)
    while (TRUE) { 
      # NOTE: computationally inefficient. It's fine for low grid size,
      #   but making a very large grid with few empty squares will have performance issues.
      i <- sample(1:grid_dim, size = 1)                                         # ransomly select a coordinate on a 2d grid
      j <- sample(1:grid_dim, size = 1)
      if (grid_mx[i,j] == "empty") {                                            # agent moves to this spot if empty, else it repeats the while loop
        grid_mx[i,j] <- agent
        break                                                                   # agent ends the while loop once they find an empty space
      }
    }
  }
  return(grid_mx)                                                               # returns 2D matrix, values are "blue", "red"
}


check_neighbors <- function(grid_mx, min_homophily) {
  # function to check who is dissatisfied
  # RETURN: matrix of TRUE/FALSE (satisfied/unsatisfied)
  satisfaction_mx <- matrix(data = NA, nrow = grid_dim, ncol = grid_dim)        # 2D matrix to check each agent's satisfaction
  
  for (i in 1:nrow(grid_mx)) {                                                  # check each cell for agent's satisfaction
    for (j in 1:nrow(grid_mx)) {
      ij_group <- grid_mx[i,j]                                                  # identify if this agent is blue or red
      if (ij_group == "empty") {                                                # skip if no one lives here
        satisfaction_mx[i,j] <- TRUE
      } else {
        # get ij's neighbors as a matrix
        i_coords <- c(i-1,i,i+1)                                                # identify coordinates of i-j's neighbors, plus/minus one cell around i-j
        i_coords <- i_coords[i_coords > 0 & i_coords <= grid_dim]               # handle edges - make sure neighbors' coordinates are not out of bounds
        j_coords <- c(j-1,j,j+1)
        j_coords <- j_coords[j_coords > 0 & j_coords <= grid_dim]
        ij_neighbors <- grid_mx[i_coords, j_coords]                             # using those coordinates, extract from the neighborhood grid
        ij_homophily <-                                                         # calculate neighbor homophily: # similar neighbors / # neighbors
          (sum(ij_neighbors == ij_group, na.rm = TRUE) - 1) /                   # subtract one, agent i-j is not it's own neighbor
            ((sum(ij_neighbors != "empty", na.rm = TRUE) - 1))
        if (is.nan(ij_homophily)) { ij_homophily <- 1}                          # handle case when i-j has no neighbors, assume satisfaction
        # check and record if ij is satisfied
        satisfaction_mx[i,j] <- ij_homophily >= min_homophily[[ij_group]]       # record i-j's satisfaction in parallel matrix
      }
    }
  }
  return(satisfaction_mx)
}

# function to print grid
plot_neighborhood <- function(grid_mx, round) {                                 # recode neighborhood matrix to work with the image() function
  plot_mx <- matrix(data = 0, nrow = grid_dim, ncol = grid_dim)
  plot_mx[grid_mx == "blue"] <- 1
  plot_mx[grid_mx == "red"] <- 2
  image(plot_mx, col=c("white", "blue", "red"), 
        main="Neighborhood Grid", 
        xlab = paste("round", round))
}


# Simulation - Setup ------------------------------------------------------

grid_dim <- sqrt(red_size + blue_size + 1) |> ceiling()                         # determine grid size based on user-specified agents
unassigned_agents <- c(rep("blue", times = blue_size),                          # create a vector of "red" and "blue" agents
                       rep("red", times = red_size),
                       rep("empty", times = grid_dim^2 - red_size - blue_size)) # including non-filled spaces
unassigned_agents <- sample(unassigned_agents)                                  # randomly shuffle agents and empty spaces
grid_mx <- matrix(data = unassigned_agents, nrow = grid_dim, ncol = grid_dim)   # create 2D matrix with shuffled agents
plot_neighborhood(grid_mx, round = 0)                                           # view neighborhood


# Simulation - Run --------------------------------------------------------

for (round in 1:rounds) {                                                       # for each round in the simulation.
  satisfaction_mx <- check_neighbors(grid_mx, min_homophily)                    # check who is dissatisfied
  unsatisfied_agents <-                                                         # get vector of each unsatisfied individual
    as.vector(grid_mx)[!as.vector(satisfaction_mx)] |>                          # checking the neighborhood matrix against the satisfaction matrix
    sample()                                                                    # shuffle; this gives a list of "blue" and "red", not specifically remembering where they came from
  if (length(unsatisfied_agents) == 0) {                                        # end simulation if no one wants to move
    print(paste0("Everyone is Satisfied. Round: ", round))
    break
  }
  grid_mx[!satisfaction_mx] <- "empty"                                          # remove all unsatisfied agents at once; allowing their spaces to immediately become available
  grid_mx <- find_new_homes(grid_mx, unsatisfied_agents)                        # Assign everyone to a new spot

  Sys.sleep(round_time)                                                         # pause; allows visual inspection of neighborhood graph as it changes
  plot_neighborhood(grid_mx, round)                                             # show results
}
