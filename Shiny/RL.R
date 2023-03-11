library(shiny)

# Softmax choice function
logsumexp <- function (x) {
  y <- max(x)
  y + log(sum(exp(x - y)))
}
softmax <- function (x) {
  exp(x - logsumexp(x))
}


# Define the user interface for the app
ui <- fluidPage(
  titlePanel("Multi-Armed Bandit Task"),
  
  # Add a sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      sliderInput("reward", "Reward Amount:", min = 0, max = 1, value = 0.5),
      selectInput("arm", "Arm:", c("Arm 1" = 1, "Arm 2" = 2, "Arm 3" = 3)),
      sliderInput("learning_rate", "Learning Rate:", min = 0, max = 1, value = 0.1),
      sliderInput("temperature", "Temperature:", min = 0, max = 1, value = 0.1)
    ),
    
    # Add a plot to the main panel
    mainPanel(
      plotOutput("bandit_plot"),
      actionButton("play", "Play")
    )
  )
)

# Define the server logic for the app
server <- function(input, output) {
  
  # Initialize variables
  values <- c(0, 0, 0)
  #counts <- c(0, 0, 0)
  #rewards <- c(0, 0, 0)
  
  # Create a reactive expression to simulate the bandit task
  play_bandit <- reactive({
    #counts[input$arm] <- counts[input$arm] + 1
    #rewards[input$arm] <- rewards[input$arm] + input$reward
    #values[input$arm] <- rewards[input$arm] / counts[input$arm]
    
    # Update the model's expectations using a softmax function
    # exp_values <- exp(values / input$temperature)
    # probs <- exp_values / sum(exp_values)
    probs <- softmax(values * input$temperature)
    
    # Select an arm using the model's expectations
    arm <- sample(1:3, size=1, prob=probs)
    
    # Update the model's expectations based on the selected arm
    values[arm] <- values[arm] + input$learning_rate * (input$reward - values[arm])
    
    # Return the selected arm and the updated values
    list(arm=arm, values=values)
  })
  
  # Render the initial plot
  output$bandit_plot <- renderPlot({
    barplot(values, xlab="Arm", ylab="Expected Value", main="Multi-Armed Bandit Task")
  })
  
  # Update the plot and the model's expectations when the "Play" button is clicked
  observeEvent(input$play, {
    result <- play_bandit()
    output$bandit_plot <- renderPlot({
      barplot(result$values, xlab="Arm", ylab="Reward Expectation")
    })
  }
  )
  
  output$plot <- renderPlot({
    if (is.null(output$bandit_plot)) return()
    output$bandit_plot
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
