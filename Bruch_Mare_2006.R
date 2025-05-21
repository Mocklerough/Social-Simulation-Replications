# file name: Bruch_Mare_2006.R
# author: Alexander Karl
# create date: 2025-03-19
# last update: 2025-05-15

# Article: Neighborhood Choice and Neighborhood Change
# Authors: Elizabeth E. Bruch & Robert D. Mare
# Date: 2006
# Journal: American Journal of Sociology. Vol 112, No. 3, pp.667-709.
# DOI: https://doi.org/10.1086/507856

# Abstract: 
#   This article examines the relationships between the residential
#   choices of individuals and aggregate segregation patterns. Analyses
#   based on computational models show that high levels of segregation
#   occur only when individuals’ preferences follow a threshold function.
#   If individuals make finer-grained distinctions among neighborhoods
#   that vary in racial composition, preferences alone do not
#   lead to segregation. Vignette data indicate that individuals respond
#   in a continuous way to variations in the racial makeup of neighborhoods
#   rather than to a threshold. Race preferences alone may be
#   insufficient to account for the high levels of segregation observed in
#   American cities.


# Overhead ----------------------------------------------------------------

rm(list = ls())

# Variable Parameters ------------------------------------------------------

proporiton_black <- 0.50                                                        # proporiton of black agents, also sets proportion white as 1 - x
vacancy_rate <- 0.15                                                            # proportion empty cells. to force a square matrix, this is a minimum % of cells given the city grid length
preferred_homophily_black <- 0.50                                               # goal minimum homophily that black agents seek
preferred_homophily_white <- 0.50                                               # goal minimum homophily that white agents seek

# Constant Parameters -----------------------------------------------------

city_grid_length <- 500                                                         # dimensions of the city grid
city_size <- city_grid_length^2                                                 # use city grid length to define city matrix
neighborhood_radius <- 2                                                        # how many cells away from self that an agent looks for neighbors

# Dependent Parameters ----------------------------------------------------

proportion_white <- 1 - proporiton_black                                        # set proportion white based on proportion black

n_black_agents = round(city_size * (1 - vacancy_rate) * proporiton_black)       # convert proporitons to integer values based on vacant city spaces
n_white_agents = round(city_size * (1 - vacancy_rate) * proportion_white)
n_empty_spaces <- city_size - n_black_agents - n_white_agents


# A Closer Look at the Agent-Based Model ----------------------------------

# The model... uses a 500 x 500 cell grid populated by interacting agents.
# When the model is initialized, the agents are evenly distributed across the grid 
# MODELING CHOICE: Bruch & Mare do not describe this process, besides it being non-random and has an index of dissimilarity of 0
#       but do not specify how. I populate cells alternating between B and W, then remove 15% of each at random
unassigned_agents <- c(rep("B", times=n_black_agents),                          # generate all black & white agents, plus empty spaces
                       rep("W", times=n_white_agents),
                       rep(NA,  times=n_empty_spaces)) |>
  sample()                                                                      # randomly shuffle them together.
city <- matrix(unassigned_agents, nrow = city_grid_length, ncol = city_grid_length) # assign the shuffled list to fill a matrix
# and the index of dissimilarity is zero.

get_neighbors <- function(i, j, radius = neighborhood_radius) {                 # function to identify neighbors based on i-j coordinates
  # check immediate neighborhood
  # check for corners/sides
  axes_i <- max(0, i - radius):min(city_grid_length, i + radius)
  axes_j <- max(0, i - radius):min(city_grid_length, j + radius)
  
  neighbors <- city[axes_i, axes_j] |>  as.vector()                             # flatten 2d slice of the neighborhood to a 1d vector for calculations
  neighbors <- neighbors[!is.na(neighbors)]                                     # remove empty spaces. returns NaN if no one in neighborhood
  return(neighbors)
}

index_of_dissimilarity <- function(city) {
  agent_dissimilarity <- c()                                                    # collect agent dissimilarity score for each agent
  for (i in 1:city_grid_length) {                                               # loop over each i-j coordinate: ie. each agent
    for(j in 1:city_grid_length) {
      agent <- city[i,j]
      if (!is.na(agent)) {                                                      # skip blank cells
      neighbors <- get_neighbors(i,j)                                           # use the above function to get agent i-j's neighbors
        if (agent == "B") {                                                     # calculate their dissimilarity, separate logic for black and white agents
          agent_dissimilarity[length(agent_dissimilarity) + 1] <- 
            abs(sum(neighbors == "B") / n_black_agents - sum(neighbors == "W") / n_white_agents)
        } else if (agent == "W") {
          agent_dissimilarity[length(agent_dissimilarity) + 1] <- 
            abs(sum(neighbors == "E") / n_white_agents - sum(neighbors == "B") / n_black_agents)
        }
      }
    }
  }
  # MODELING CHOICE: modify iod to be the average per  agent
  # Since IOD is otherwise a sum over all individuals, then with about 500^2 entities, it will necessarily be large
  # Does not change results, just scales them
  iod <- sum(agent_dissimilarity) / 2 / (n_black_agents + n_white_agents)       # calculate index of dissimilarity for entire model
  return(iod)
}

index_of_dissimilarity(city)




