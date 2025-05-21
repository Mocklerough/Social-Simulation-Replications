# leik_meeker_1995.R
# Author: Alexander Karl
# Create Date: 2025-05-21
# Last Update: 2025-05-21

# Article: Computer Simulation for Exploring Theories: Models of Interpersonal Cooperation and Competition
# Authors: Robert K. Leik & Barbara F. Meeker
# Journal: Sociological Perspectices, Vol. 38, No. 4, pp. 463-482

# ABSTRACT: 
#   Computer simulation for exploring substantive theories is both powrful and convenient, 
#   but there arefew standards fordoing simulations. Some critia are suggest and their 
#   imprtance illustrated in detail via our adaptation and augmentation of a species 
#   compition model to reflect interpersonal cooperation or competition. Outcomes of the 
#   simulation and their substantie interpretation can vary due to minor variation in 
#   parametrs, to convenientfunctionalfonns assumed by the program, and to settings 
#   offunction parameters which seen to have no direct relevance for the substantive 
#   intrpretation of the modeL Under certain circumstances, chaos-like patterns emerge 
#   from the simulation.


# Overhead ----------------------------------------------------------------

rm(list = ls())


# Richardson's (10960) Arms Race Model ------------------------------------
# defining model parameters as NA, to illustrate required parameters with their definitions

determine_LOA <- function(LOA_ego_t_minus_1, LOA_alter_t_minus_1, t, s, f, g) { # generic function to determine either nation's LOA
  LOA_ego_t <- 
    LOA_ego_t_minus_1 +                                                         # return LOA at time t, to append to the LOA vector
    s * LOA_alter_t_minus_1 - 
    f * LOA_ego_t_minus_1 + 
    g 
  return(LOA_ego_t)
}

simulate_richardson <-                                                          # function to run the entire Richardson simulation model
  function(                                                                     # define Richardson model parameters
    rounds,                                                                     # number of rounds to run the simulation for
    LOA_x_t1, LOA_y_t1,                                                         # Initial Level of Arms for nation X & Y                                                                 # Initial Level of Arms for nation Y
    s_x, s_y,                                                                   # Level of sensitivity of nation X to nation Y's Level of Arms & vice-versa
    f_x, f_y,                                                                   # Level of fatigue of nation X and nation Y from their own Level of Arms                                                                        # Level of fatigue of nation Y from their own Level of Arms 
    g_x, g_y                                                                    # Constant motivational effect of nation X nad nation Y                                                                   # Constant motivational effect of nation Y
    ) {
  
  LOA_x <- rep(NA, rounds)                                                      # Levels of Arms for nation X, defined as empty vector using round # to index
  LOA_y <- rep(NA, rounds)                                                      # Levels of Arms for nation Y, defined as empty vector using round # to index
  
  LOA_x[1] <- LOA_x_t1                                                          # initial value of x's Level of Arms
  LOA_y[1] <- LOA_y_t1                                                          # initial value of x's Level of Arms
  
  for (t in 2:rounds) {                                                         # run for all rounds, starting with round  2
    LOA_x[t] <- determine_LOA(LOA_x[t-1], LOA_y[t-1], t, s_x, f_x, g_x)         # calculate LOA for nation X at this timestep
    LOA_y[t] <- determine_LOA(LOA_y[t-1], LOA_x[t-1], t, s_y, f_y, g_y)         # calculate LOA for nation Y at this timestep
  }
  LOA <- list(x = LOA_x, y = LOA_y)                                             # package together and return
  return(LOA)
}


# Simulate Richardson:  -----------------------------------------------------

# set parameters for Figure 2 - defined in article
rounds <- 250 


LOA <- 
  simulate_richardson(
    rounds = 250,
    LOA_x_t1 = 1, LOA_y_t1 = 1,
    s_x = 0.10,    s_y = 0.10,
    f_x = 0.09,    f_y = 0.05,
    g_x = 0.25,    g_y = 0.20)

plot(1:rounds, LOA$x, type = "l", col = "blue", lwd = 2, 
     xlab = "Rounds", ylab = "LOA", main = "FIGURE 2: An Example of Fixed-Parameter Competition")
lines(1:rounds, LOA$y, col = "red", lwd = 2)

# NOTE: both X and Y increase over time. While there is a fatigue penalty, 
# decreasing would require it be set much higher than the vigor and growth parameters 
# for either one of X or Y



# Simulate Leek & Meeker's Modified Version: ------------------------------

determine_response <- function(R_ego, R_alter, I_ego, G_ego, V_ego, t) {        # similar to Richardson's formula, modified to include X*Y interactions
  R_ego[t+1] <-                                                                
    R_ego[t] - 
    I_ego * R_ego[t] * R_alter[t] - 
    G_ego * R_ego[t]^2 + 
    R_ego[t] * V_ego                                                            # NOTE: argicle specifies V-sub(x,t), but later implies it is a constant, not a variable changing over time. Implement as a constant
  return(R_ego)
}

simulate_leek_meeker <- 
  function(
    rounds,                                                                     # number of rounds to run the simulation for
    R_x_t1, R_y_t1,                                                             # Initial Resources for nation X & Y                                                                 # Initial Level of Arms for nation Y
    I_x, I_y,                                                                   # Interaction effect between populations
    G_x, G_y,                                                                   # growth, applies a penalty to population growth
    V_x, V_y                                                                    #vigor, or constant impetus towards growth
  ) { 
    R_x <- rep(NA, rounds)                                                      # Resources for nation X, defined as empty vector using round # to index
    R_y <- rep(NA, rounds)                                                      # Resources for nation Y, defined as empty vector using round # to index
    
    R_x[1] <- R_x_t1                                                            # initial value of x's Resources
    R_y[1] <- R_y_t1                                                            # initial value of x's Resources
    
    for (t in 1:(rounds-1)) {                                                       # run for all rounds, starting with round  2
      R_x <- determine_response(R_x, R_y, I_x, G_x, V_x, t)                   # calculate R for nation X at this timestep
      R_y <- determine_response(R_y, R_x, I_y, G_y, V_y, t)                   # calculate R for nation Y at this timestep
    }
    R <- list(x = R_x, y = R_y)                                                 # package together and return
    return(R)
  }

R <- simulate_leek_meeker(
    rounds = 250,
    R_x_t1 = 1, R_y_t1 = 1,
    I_x = 0.10, I_y = 0.10,
    G_x = 0.09, G_y = 0.05,
    V_x = 0.25, V_y = 0.20
  )

plot(1:rounds, R$x, type = "l", col = "blue", lwd = 2, ylim = c(0, max(c(R$x, R$y))),
     xlab = "Rounds", ylab = "Resources", main = "FIGURE 2: An Example of Fixed-Parameter Competition")
lines(1:rounds, R$y, col = "red", lwd = 2)

