library(shiny)
library(tidyverse)

# Define the user interface for the app
ui <- fluidPage(
  
  # Add a title and sidebar
  titlePanel("Bootstrapped Distribution"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_sims", "Number of Simulations:",
                  min = 1, max = 10000, value = 100),
      sliderInput("sampleSize", "Sample Size:",
                  min = 1, max = 10000, value = 100),
    selectInput("distT", "Distribution Type:",
                c("Normal" = "norm",
                  "Exponential" = "exp",
                  "Gamma" = "gam",
                  "Poisson" = "pois",
                  "Binomial" = "binom")),
    actionButton("simulate", "Simulate")
    ),
    
    # Add a plot to the main panel
    mainPanel(plotOutput("dist_plot"))
  )
)

# Define the server logic for the app
server <- function(input, output) {
  
  # Create a reactive expression to simulate the bootstrapped distribution
  #dist_data <- reactive({
  dist_data <- reactive({
    #set.seed(123)
    #bootstrapped_mean <- replicate(input$num_sims, mean(sample(1:10, size=10, replace=TRUE)))
    if(input$distT == "norm"){
      bootstrapped_mean <- replicate(input$num_sims, mean(rnorm(input$sampleSize)))
    }else if(input$distT == "exp"){
      bootstrapped_mean <- replicate(input$num_sims, mean(rexp(input$sampleSize)))
    }else if(input$distT == "gam"){
      bootstrapped_mean <- replicate(input$num_sims, mean(rgamma(input$sampleSize, shape=1)))
    }else if(input$distT == "pois"){
      bootstrapped_mean <- replicate(input$num_sims, mean(rpois(input$sampleSize, lambda = 1)))
    }else if(input$distT == "binom"){
      bootstrapped_mean <- replicate(input$num_sims, mean(rbinom(input$sampleSize, size = 1, prob = .5)))
    }
    ggplot(data=tibble(mean=bootstrapped_mean), aes(x=mean)) +
      geom_histogram(bins=30) + jtools::theme_apa()
  })

  # # Render the initial plot
  # output$dist_plot <- renderPlot({
  #   dist_data()
  # })
  
  observeEvent(input$simulate, {
     dist_data <- NULL
  })  

  # Update the plot when the "Simulate" button is clicked
  observeEvent(input$simulate, {
    output$dist_plot <- renderPlot({
      #dist_data()
      if (is.null(dist_data)) return()
      dist_data()
      #}
    })
  }
  
  
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)