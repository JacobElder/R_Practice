# Install and load the igraph package
install.packages("igraph")
library(igraph)

# Create a list of friend connections
friends = list(
  1 => c(2, 3, 4),
  2 => c(1, 3, 4),
  3 => c(1, 2, 4),
  4 => c(1, 2, 3)
)

# Create a graph object from the friend connections
g = graph_from_edgelist(as.matrix(friends), directed = FALSE)

# Plot the graph
plot(g)

# Calculate and print the degree of each vertex
degrees = degree(g)
print(degrees)

# Calculate and print the number of triangles in the graph
num_triangles = transitivity(g)
print(num_triangles)