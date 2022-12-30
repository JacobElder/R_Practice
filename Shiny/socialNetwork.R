library(shiny)
library(igraph)

# Define the user interface for the app
ui <- fluidPage(
  
  # Add a title and sidebar
  titlePanel("Social Network Change Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("graph_type", "Graph Type:",
                  c("Barabasi-Albert Model" = "barabasi.game",
                    "Erdos-Renyi Model" = "erdos.renyi.game")),
      numericInput("num_nodes", "Number of Nodes:", value = 50, min = 2),
      numericInput("prob", "Probability (for Erdos-Renyi Model Only):", value = 0.5, min = 0, max = 1),
      actionButton("generate_graph", "Generate Graph")
    ),
    
    # Add a plot to the main panel
    mainPanel(plotOutput("network_plot"))
  )
)

# Define the server logic for the app
server <- function(input, output) {
  
  # Create a reactive expression to generate the initial graph
  graph_data <- reactive({
    if (input$graph_type == "barabasi.game") {
      graph <- barabasi.game(input$num_nodes)
    } else {
      graph <- erdos.renyi.game(input$num_nodes, p=input$prob)
    }
    plot(graph, layout=layout_nicely)
  })
  
  # Render the initial plot
  output$network_plot <- renderPlot({
    graph_data()
  })
  
  # Create a reactive expression to update the graph when the "Generate Graph" button is clicked
  observeEvent(input$generate_graph, {
    if (input$graph_type == "barabasi.game") {
      graph <- barabasi.game(input$num_nodes)
    } else {
      graph <- erdos.renyi.game(input$num_nodes, p=input$prob)
    }
    plot(graph, layout=layout_nicely)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
