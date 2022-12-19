# Install and load the igraph package
install.packages("igraph")
library(igraph)

# Set the number of agents in the population
N <- 1000

# Initialize the population with a random trait (0 or 1)
trait <- sample(0:1, N, replace = TRUE)

# Set the probability of an agent adopting the trait from a neighbor
p <- 0.5

# Generate a random social network using the Erdos-Renyi model
g <- erdos.renyi.game(N, 0.1)

# Calculate the PageRank centrality for each agent
centrality <- page.rank(g)$vector

# Set the number of time steps to simulate
T <- 100

# Loop over time steps
for (t in 1:T) {
  
  # Loop over agents
  for (i in 1:N) {
    
    # Choose a random neighbor
    j <- sample(neighbors(g, i), 1)
    
    # Calculate the proportion of neighbors with the trait
    prop_neighbors_with_trait <- mean(trait[neighbors(g, i)])
    
    # Calculate the probability of adoption based on the centrality of the chosen neighbor
    p_adoption <- p * prop_neighbors_with_trait * centrality[j]
    
    # If the chosen neighbor has the trait and the current agent does not,
    # adopt the trait with probability p_adoption
    if (trait[j] == 1 && trait[i] == 0) {
      trait[i] <- rbinom(1, 1, p_adoption)
    }
  }
}

# Calculate the proportion of agents with the trait at the end of the simulation
prop_with_trait <- mean(trait)