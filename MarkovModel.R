# Set the initial state
state = 1

# Set the transition probabilities
trans_probs = matrix(c(0.7, 0.3,
                       0.4, 0.6), nrow = 2, byrow = TRUE)

# Set the number of iterations
num_iter = 10

# Create a vector to store the states
states = numeric(num_iter)

# Iterate over the number of iterations
for (i in 1:num_iter) {
  # Sample the next state according to the transition probabilities
  state = sample(1:2, size = 1, prob = trans_probs[state, ])
  
  # Store the state in the vector
  states[i] = state
}

# Print the states
print(states)
