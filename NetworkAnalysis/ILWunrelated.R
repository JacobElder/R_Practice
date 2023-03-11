library(igraph)
# create graph
set.seed(52)
pilotAdj <- sapply(1:10, function(x) sample(c(0,1),10,replace=T))
diag(pilotAdj) <- 0 # No self-connections
pilotGraph <- graph_from_adjacency_matrix(pilotAdj, mode="directed")

# similarity metrics
jmat <- similarity.jaccard(pilotGraph)
dmat<-similarity.dice(pilotGraph)
ilwmat<-similarity.invlogweighted(pilotGraph)

# compare similarity
cor(c(jmat), c(dmat))
cor(c(jmat), c(ilwmat))
cor(c(dmat), c(ilwmat))

