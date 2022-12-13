// Set the number of states
int K = 2;

// Set the initial state
int state[1] = {1};

// Set the transition probabilities
matrix[K, K] trans_probs = {
  {0.7, 0.3},
  {0.4, 0.6}
};

// Set the number of iterations
int num_iter = 10;

// Create a vector to store the states
int states[num_iter];

// Iterate over the number of iterations
for (i in 1:num_iter) {
  // Sample the next state according to the transition probabilities
  state[1] = categorical_rng(trans_probs[state[1],]);
  
  // Store the state in the vector
  states[i] = state[1];
}

// Print the states
print(states);