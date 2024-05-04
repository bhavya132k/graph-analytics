library(igraph, readr, sna)

setwd("/Users/bhavya/Academics/ThirdSem/BDA/Project1")

socEpinions <- read_tsv("socEpinions.tsv")
head(socEpinions)

opTab <- as.matrix(socEpinions[,-3])
head(opTab)

n <- NROW(opTab)
V1 <- opTab[1:n,1]
V2 <- opTab[1:n,2]

relations12 <- data.frame(from=V1, to=V2)

Graph <- graph_from_data_frame(relations12, directed=TRUE)
plot.igraph(Graph)

#=============================================================================================
#================================Simplification of the Graph==================================
#=============================================================================================

# Compute communities (clusters)
# Clust <- cluster_walktrap(Graph, steps = 5)
Clust <- walktrap.community(Graph, steps = 3)

# Contract the vertices based on community membership
E(Graph)$weight <- 1
V(Graph)$weight <- 1
Graph_contracted <- contract.vertices(Graph, Clust$membership, vertex.attr.comb=list(weight="sum", name=function(x)x[1]))

# Simplify the graph by removing multiple edges and loops
Graph_simplified <- simplify(Graph_contracted, edge.attr.comb=list(weight="sum", function(x)length(x)))

# Remove vertices with a degree less than a threshold (e.g., 20)
Graph_simplified <- delete.vertices(Graph_simplified, which(degree(Graph_simplified) < 20))

# Plot the simplified graph
plot.igraph(Graph_simplified)

#=============================================================================================
#==============================Application of Functions on Graph==============================
#=============================================================================================

aGraph <- rgraph(n=200, m=1, tprob = 0.5, diag = FALSE)
aGraph <- rgraph(n=200, m=1, tprob = 0.5, diag = FALSE, mode='undirected', return.as.edgelist=TRUE)

myGraph <- graph_from_edgelist(aGraph[,-3], directed=FALSE)
plot.igraph(myGraph)

myGraph_adj <- igraph::as_adjacency_matrix(myGraph)

aGraph_density <- gden(aGraph)

igraph::edge_density(myGraph)
igraph::edge_density(myGraph, loops=T)

myGraph_ego <- ego.extract(aGraph)
myGraph_ego[1]
myGraph_ego[2]

myGraph_degree <- igraph::degree(myGraph)

myGraph_between <- igraph::centr_betw(myGraph)

myGraph_closeness <- igraph::centr_clo(myGraph)

myGraph_sp <- igraph::shortest.paths(myGraph)
igraph::get.shortest.paths(myGraph)

hist(igraph::degree(myGraph), breaks=10)

igraph::diameter(myGraph)

#=======================================================================================

V(myGraph)$central_degree <- centr_degree(myGraph)$res
V(myGraph)$name[V(myGraph)$central_degree ==max(centr_degree(myGraph)$res)]

longestpath <- induced_subgraph(myGraph, get_diameter(myGraph))
plot(longestpath)

myGraph_largestCliqsPath <- igraph::largest_cliques(myGraph)
myGraphLargestClique <- induced.subgraph(myGraph, myGraph_largestCliqsPath[[1]])
plot(myGraphLargestClique)

myGraph.ego=sna::ego.extract(opTab[1:600,1:2])
myGraph.ego[1]
myGraph.ego[2]

power_centrality(myGraph)
power_centrality(myGraph, exponent=0.1)

#=============================================================================================
#======================Simplification according to Document Provided==========================
#=============================================================================================

myGV1 <- V(Graph)[degree(Graph) < 1]

myGV3 <- V(Graph)[degree(Graph) < 3]
Graph3 <- delete_vertices(Graph, myGV3)
plot(Graph3)

myGV5 <- V(Graph)[degree(Graph) < 5]
Graph5 <- delete_vertices(Graph, myGV5)
plot(Graph5)

myGV10 <- V(Graph)[degree(Graph) < 10]
Graph10 <- delete_vertices(Graph, myGV10)
plot(Graph10)

myGV50 <- V(Graph)[degree(Graph) < 50]
Graph50 <- delete_vertices(Graph, myGV50)
plot(Graph50)

myGV100 <- V(Graph)[degree(Graph) < 100]
Graph100 <- delete_vertices(Graph, myGV100)
plot(Graph100)

myGV150 <- V(Graph)[degree(Graph) < 150]
Graph150 <- delete_vertices(Graph, myGV150)
plot(Graph150)

myGV200 <- V(Graph)[degree(Graph) < 200]
Graph200 <- delete_vertices(Graph, myGV200)
plot(Graph200)

myGV250 <- V(Graph)[degree(Graph) < 250]
Graph250 <- delete_vertices(Graph, myGV250)
plot(Graph250)

myGV300 <- V(Graph)[degree(Graph) < 300]
Graph300 <- delete_vertices(Graph, myGV300)
plot(Graph300)

myGV500 <- V(Graph)[degree(Graph) < 500]
Graph500 <- delete_vertices(Graph, myGV500)
plot(Graph500)

myGV800 <- V(Graph)[degree(Graph) < 800]
Graph800 <- delete_vertices(Graph, myGV800)
plot(Graph800)

myGV1000 <- V(Graph)[degree(Graph) < 1000]
Graph1000 <- delete_vertices(Graph, myGV1000)
plot(Graph1000)

myGV1200 <- V(Graph)[degree(Graph) < 1200]
Graph1200 <- delete_vertices(Graph, myGV1200)
plot(Graph1200)

myGV1500 <- V(Graph)[degree(Graph) < 1500]
Graph1500 <- delete_vertices(Graph, myGV1500)
plot(Graph1500)

#==========================================================================================

df200 <- relations12[1:200,]
myGraph200 <- graph_from_data_frame(df200, directed=TRUE)
plot(myGraph200)

df500 <- relations12[1:500,]
myGraph500 <- graph_from_data_frame(df500, directed=TRUE)
plot(myGraph500)

df1000 <- relations12[1:1000,]
myGraph1000 <- graph_from_data_frame(df1000, directed=TRUE)
plot(myGraph1000)

df1500 <- relations12[1:1500,]
myGraph1500 <- graph_from_data_frame(df1500, directed=TRUE)
plot(myGraph1500)

df2000 <- relations12[1:2000,]
myGraph2000 <- graph_from_data_frame(df2000, directed=TRUE)
plot(myGraph2000)

df5000 <- relations12[1:5000,]
myGraph5000 <- graph_from_data_frame(df5000, directed=TRUE)
plot(myGraph5000)

#===============================================================================================

agraph <- rgraph(n=200,m=1,tprob=0.5,diag=FALSE)
agraph <- rgraph(n=200,m=1,tprob=0.15,diag=FALSE, mode="undirected",return.as.edgelist = TRUE)

agraphDrop<- agraph[,-3]
iGraph <- graph_from_edgelist(agraphDrop,directed=FALSE)

V(iGraph)
E(iGraph)

iGraph.adj <- get.adjacency(iGraph)
iGraph.density <- edge_density(iGraph)

iGraph.ego <- ego.extract(iGraph)
iGraph.ego[1]
# Assume 'Graph' is your igraph object and you want the ego graph of vertex id 1
iGraph.ego <- make_ego_graph(Graph, nodes = 1, order = 1, mode = c("all"))

iGraph.btw <- centr_betw(iGraph)
iGraph.cls <- centr_clo(iGraph)

# iGraph.sp <- shortest.paths(iGraph)
iGraph.distances <- distances(iGraph)
iGraph.geos <- geodist(iGraph)

iGraph.np <- iGraph.adj%*%iGraph.adj
hist(igraph::degree(iGraph))

bGraph = rgraph(25,1,0.2,"graph", FALSE)
bGraph.adj <- graph_from_adjacency_matrix

bGraph.d <- diameter(bGraph.adj)

node <- c(13)
bGraph.13clique = max_cliques(bGraph.adj, min=NULL, max=NULL, subset=node) 
bGraph.13clique

bGraph.lgcliques = clique_num(bGraph.adj)
bGraph.lgcliques

