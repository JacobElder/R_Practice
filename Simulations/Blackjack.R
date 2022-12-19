# function to generate a random card
generate_card <- function() {
  # generate a random number between 1 and 13
  card <- sample(1:13, 1)
  
  # assign a suit to the card
  suit <- sample(c("hearts", "diamonds", "clubs", "spades"), 1)
  
  # return the card and suit as a character string
  return(paste(card, suit))
}

# function to calculate the total value of a hand of cards
calculate_total <- function(hand) {
  # initialize the total to zero
  total <- 0
  
  # loop through each card in the hand
  for (card in hand) {
    # split the card into its value and suit
    split_card <- strsplit(card, " ")[[1]]
    value <- as.numeric(split_card[1])
    suit <- split_card[2]
    
    # if the value is an ace, add 11 to the total
    if (value == 1) {
      total <- total + 11
    } 
    # if the value is a face card (jack, queen, or king), add 10 to the total
    else if (value > 10) {
      total <- total + 10
    } 
    # otherwise, add the value of the card to the total
    else {
      total <- total + value
    }
  }
  
  # return the total
  return(total)
}

# function to play a game of blackjack
play_blackjack <- function() {
  # deal the initial two cards to the player and the dealer
  player_hand <- c(generate_card(), generate_card())
  dealer_hand <- c(generate_card(), generate_card())
  
  # print the initial hands
  cat("Player hand:", player_hand, "\n")
  cat("Dealer hand:", dealer_hand[1], "\n")
  
  # initialize a flag to indicate whether the player wants to hit or stand
  hit <- TRUE
  
  # loop until the player stands or busts
  while (hit) {
    # ask the player if they want to hit or stand
    response <- readline(prompt = "Hit or stand (h/s)? ")
    
    # if the player wants to hit, deal them another card
    if (response == "h") {
      player_hand <- c(player_hand, generate_card())
      cat("Player hand:", player_hand, "\n")
      # if the player busts, end the loop
      if (calculate_total(player_hand) > 21) {
        hit <- FALSE
      }
    } 
    # if the player wants to stand, end the loop
    else {
      hit <- FALSE
    }
  }
  
  # if the player busts, the dealer wins
  if (calculate_total(player_hand) > 21) {
    cat("Player busts! Dealer wins.\n")
  } 
  # otherwise, the dealer hits until their total is at least 17
  else {
    while (calculate_total(dealer_hand) < 17) {
    }
  }
      
      
      