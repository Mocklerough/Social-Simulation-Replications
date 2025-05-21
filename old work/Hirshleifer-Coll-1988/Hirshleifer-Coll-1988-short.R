



# Input Parameters --------------------------------------------------------


strategies <- c("COOPERATE", "DEFECT")
payoff_mx <- matrix(data = c(3,0,5,1), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(strategies, strategies))
start_pop = c(0.7, 0.3)
k = 0.01
generations = 500


# Run Simulation ----------------------------------------------------------


results <- matrix(data = NA, ncol = length(strategies), nrow = generations + 1)
colnames(results) <- strategies
results[1,] <- start_pop 
for (g in 1:generations) {
  a <- payoff_mx
  p <- results[g,]
  Y <- colSums(p * t(a))     # EQ[2] Calculate Y: average payoff yields (vector, per strategy)
  M <- sum(p * Y)            # Eq[3] Calculate M: overall/societal mean of Y yields (atomic)
  delta_p <- k * p * (Y - M) # EQ[1] Calculate delta_P: change in the population at each time step (vector, per strategy)
  results[g+1,] <- results[g,] + delta_p
}


# Plot --------------------------------------------------------------------

plot(x = 0:generations, y = )
plot(x = 0:generations, y = )

plot(x=0:generations, type = "n", ylim = range(c(y1, y2)), 
     xlab = "X-axis", 
     ylab = "Y-axis", 
     main = "Overlapping Line Plots")

# Add the first line
lines(x=0:generations, y=results[,"COOPERATE"], col = "blue")

# Add the second line
lines(x=0:generations, y=results[,"DEFECT"], col = "red")