# Bruch and Mare only desccribe the initial placement of agents within the city as that "
# they are arranged with an index of dissimilarity of zero across a fixed grid of tracts."
# They do not describe how this is done, however they state they donot take Schelling's 
# strategy of random placements.

# *D = 'index of dissimilarity', where 0 represents a perfectly homogenous mixing

# This is problematic because You cannot gurantee that D = 0 for most ratios of B/W populations with finite numbers of agents,
# Further, their arguments that Shelling's random placement (RP) "the initial values of the segregation
# scores are affected by the proportion minority in the city’s population" is untrue, as D accounts for proportion.

# I take it to assume that Bruch and Mare used Structured Alternating Placement (SAP) (EX:B-W-B-W-B-W)
# This script compares the actual D score of SAP against RP

rm(list=ls())
library(grid)

city_grid_length <- 50
city_size <- city_grid_length^2

vacancy_rate <- 0.15 

prop_black <- 0.5
prop_white <- 1 - prop_black

n_black_agents <- round(city_size * (1 - vacancy_rate) * prop_black)
n_white_agents <- round(city_size * (1 - vacancy_rate) * prop_white)
n_empty_spaces <- city_size - n_black_agents - n_white_agents


# Plotting function -------------------------------------------------------

plot_grid <- function(grid) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(city_grid_length, city_grid_length)))
  
  for (i in 1:city_grid_length) {
    for (j in 1:city_grid_length) {
      grid.rect(
        x = j / city_grid_length,
        y = (city_grid_length - i + 1) / city_grid_length,
        width = 1 / city_grid_length,
        height = 1 / city_grid_length,
        gp = gpar(fill = city_RP[i, j], col = "black")
      )
    }
  }
  
}


# Random Placement --------------------------------------------------------

unassigned_agents <- c(rep("blue", times=n_black_agents),
                       rep("red", times=n_white_agents),
                       rep("white",  times=n_empty_spaces)) |>
  sample()


city_RP <- matrix(unassigned_agents, nrow = city_grid_length, ncol = city_grid_length)
plot_grid(city_rp)

# Structured Alternating Placement ----------------------------------------

city_sap <- matrix("", nrow = 50, ncol = 50)
for (i in 1:50) {
  for (j in 1:50) {
    city_sap[i,j] <- ifelse((i + j) %% 2 == 0, "blue", "red")
  }
}
remove_black <- round(50*50*0.15)
remove_white <- round(50*50*0.15)

while (remove_black > 0 & remove_white > 0) {
  i <- sample(1:50,1)
  j <- sample(1:50,1)
  agent <- city_sap[i,j]
  if (!is.na(agent) & agent != "white") {
    if (city_sap[i,j] == "blue" & remove_black > 0) {
      city_sap[i,j] <- "white"
      remove_black <- remove_black - 1
    }
    if (city_sap[i,j] == "red" & remove_white > 0) {
      city_sap[i,j] <- "white"
      remove_white <- remove_white - 1
    }
  }
}


grid.newpage()
pushViewport(viewport(layout = grid.layout(50, 50)))

for (i in 1:50) {
  for (j in 1:50) {
    grid.rect(
      x = j / 50,
      y = (50 - i + 1) / 50,
      width = 1 / 50,
      height = 1 / 50,
      gp = gpar(fill = city_sap[i, j])
    )
  }
}
