data {
  int num_iter;
  int state;
}

parameters {
  simplex[2] trans_probs[2];
}

model {
  for (i in 1:num_iter) {
    // Sample the next state according to the transition probabilities
    state ~ categorical(trans_probs[state]);
  }
}
