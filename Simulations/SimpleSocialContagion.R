# Set the number of agents in the population
N <- 1000

# Initialize the population with a random trait (0 or 1)
trait <- sample(0:1, N, replace = TRUE)

# Set the probability of an agent adopting the trait from a neighbor
p <- 0.5

# Set the number of time steps to simulate
T <- 100

# Loop over time steps
for (t in 1:T) {
  
  # Loop over agents
  for (i in 1:N) {
    
    # Choose a random neighbor
    j <- sample(1:N, 1)
    
    # If the chosen neighbor has the trait and the current agent does not,
    # adopt the trait with probability p
    if (trait[j] == 1 && trait[i] == 0) {
      trait[i] <- rbinom(1, 1, p)
    }
  }
}

# Calculate the proportion of agents with the trait at the end of the simulation
prop_with_trait <- mean(trait)
