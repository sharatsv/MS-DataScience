# Social network visualization

library(igraph)
my.dir <- '/users/venkatasharatsripada/Downloads/'
link.data <- read.csv(paste0(my.dir, 'LINKS-421-719Network.csv'), stringsAsFactors = F, header=T)
node.data <- read.csv(paste0(my.dir, 'NODES-421-719Network.csv'), stringsAsFactors = F, header=T)


# Clean-data
colnames(link.data)
colnames(link.data) <- gsub("\\.","", colnames(link.data))
link.data$X <- gsub(" |-", "", link.data$X)

cbind(link.data$X, colnames(link.data)[-1])

node.data$Name <- gsub("\\.","", node.data$Name)

# Use cbind as a means to check if the data columns match up
cbind(node.data$Name, link.data$X)

M <- as.matrix(link.data[,-1])
rownames(M) <- colnames(M)
dim(M)


# Check for any NA scoping for human error - replace with zero
any(is.na(M))

M[is.na(M)] <- 0

# Check for anything > 1
M[M > 1]

g <- graph_from_adjacency_matrix(M)

# vertices
vcount(g)
# edges
ecount(g)

# Basic plot - Issue of overplotting
plot.igraph(g)

# Clean-up the problem of overplotting
# Remove self-loops first
g <- simplify(g)

par(mar = c(0,0,0,0))
plot.igraph(g)

# Suppress the arrow-size
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

# Another way of doing this is using the attributes
E(g)$arrow.size = 0
E(g)$arrow.width = 0
# Notice two new attribs:
#  - arrow.size (e/n) - edge, numeric
#  - arrow.wifth (e/n) - edge, numeric .. apart from the existing name - vertex, char.
g
plot.igraph(g)

# Similarly set the vertex attributes
V(g)$color <- 'gold'
V(g)$frame.color <- 'white'
V(g)$label.color <- 'black'
E(g)$color <- 'cadetblue'
V(g)$size <- 5

plot.igraph(g)

# See the node JoHunter - This isolated node is called an Isolate

E(g)$curved <- .4

# Most important node of the network - Centrality
par(mar = c(3,10,1,1))
barplot(sort(degree(g)), horiz=T, las=2, main='Most connections')
V(g)$degree <- degree(g)
V(g)$deg.out <- degree(g, mode='out')
V(g)$deg.in <- degree(g, mode='in')

# Most outgoing or friendly
barplot(V(g)$deg.out, names.arg = V(g)$name, horiz=T, las=2
        , main = 'Most outgoing or friendly')

# Most incoming or favored
barplot(V(g)$deg.in, names.arg = V(g)$name, horiz=T, las=2
        , main='Most incoming or favored')

g.bak <- g
g <- as.undirected(g)
# g <- g.bak

# Distance between nodes using closeness()
V(g)$close <- closeness(g, normalized = T, mode=)

# Chances of having to pass through someone to get to someone else, using betweenness()
V(g)$bet <- betweenness(g, directed = F)

library(plotrix)
my.pallet <- colorRampPalette(c('steelblue1', 'violet', 'tomato', 'red', 'red'))
V(g)$color <- rev(my.pallet(200))[round(1 + rescale(V(g)$close, c(1,199)), 0)]

V(g)$size <- 2 + rescale(V(g)$degree, c(0,13))
V(g)$label.cex <- .7 + rescale(V(g)$bet, c(0,1.25))
plot.igraph(g)


# Homopholy - Chances of being linked to others based on location, attributes etc. 
# Possible explanation for clusters through Homopholy
V(g)$class <- node.data$Class
V(g)$country <- node.data$Country
V(g)$year <- node.data$year

# Also remove the isolate
g <- delete_vertices(g, 'JoHunter')

plot.igraph(g)

V(g)$shape <- 'circle'
V(g)$shape[V(g)$class == 'Wednesday'] <- 'square'
V(g)$shape[V(g)$class == 'Both'] <- 'rectangle'
plot.igraph(g)
# No real cluster or relation based on the class

V(g)$color <- 'gold'
V(g)$color[V(g)$country == 'India'] <- 'springgreen4' 
V(g)$color[V(g)$country == 'China'] <- 'red'
V(g)$color[V(g)$country == 'Both'] <- 'purple'
plot.igraph(g)
# We start to see the clustering based on the countries

# The cluster of Indian students show 2x clusters - let's see if the year is criteria there
V(g)$label.color <- 'blue'
V(g)$label.color[V(g)$year == 1] <- 'black'
plot.igraph(g)


# Using clustering algos to see the network
fc <- cluster_fast_greedy(as.undirected(g))
print(modularity(fc)) # 0.7 is tight cohesive network
                      # 0.2 is pretty lose .. so, 0.5 is showing a pretty cohesive network

membership(fc)
V(g)$cluster <- membership(fc)
length(fc) # shows number of clusters
sizes(fc) # number of members in the cluster

par(mar = c(0,0,0,0))
plot_dendrogram(fc, palette = rainbow(7))


# Use the ist719NetworkObject.Rda
load(paste0(my.dir, 'ist719NetworkObject.rda'))
plot.igraph(g)

l <- layout_in_circle(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)


l <- layout_with_fr(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

E(g)$color <- 'grey'
E(g)[from("LeelaDeshmukh")]$color <- 'red'
l <- layout_as_star(g, center='LeelaDeshmukh')
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

l <- layout_with_kk(g) # Maximizes distance between the nodes
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

V(g)$x <- 0
V(g)$y <- 0
coords <- cbind(V(g)$x, V(g)$y)
iteration <- c(500, 100, 20, 10, 5, 3, 2, 1)

for(i in 1:length(iteration)){
  l <- layout_with_fr(g, coords=coords, dim=2, niter = iteration[i])
  V(g)$x <- l[,1]
  V(g)$y <- l[,2]
  plot.igraph(g)
  mtext(paste('Layout FR:', iteration[i], side=3, 
              line=0, cex=1.5, adj=0))
}


l <- layout_with_gem(g)
l <- layout_with_dh(g)
l <- layout_on_grid(g)


# bi-partite networks
my.linked.list <- data.frame(person=V(g)$name, event=V(g)$country)
g.bak.1 <- g
g <- graph_from_data_frame(my.linked.list, directed=F)

V(g)$type <- FALSE
node.data$NewName <- gsub("\\ ", "", node.data$Name)
V(g)$type[V(g)$name %in% node.data$NewName] <- TRUE

l <- layout_as_bipartite(g, types = V(g)$type)
V(g)$x <- l[,2]
V(g)$y <- l[,1]

par(mar = c(0,0,0,0))
plot.igraph(g)
V(g)$size <- 0
