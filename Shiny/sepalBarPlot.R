library(shiny)
library(ggplot2)

# Define the user interface for the app
ui <- fluidPage(
  
  # Add a title and sidebar
  titlePanel("Dynamic Bar Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X Variable:",
                  c("Species" = "Species")),
      radioButtons("y_var", "Y Variable:",
                   c("Width" = "Sepal.Width",
                     "Length" = "Sepal.Length"))
    ),
    
    # Add a plot to the main panel
    mainPanel(plotOutput("barplot"))
  )
)

# Define the server logic for the app
server <- function(input, output) {
  
  # Create a reactive expression to generate the plot
  plot_data <- reactive({
    data(iris)
    ggplot(iris, aes_string(x=input$x_var, y=input$y_var)) +
      geom_bar(stat="identity")
  })
  
  # Render the plot
  output$barplot <- renderPlot({
    plot_data()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)