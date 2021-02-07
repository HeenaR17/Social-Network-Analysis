#SNA
library(igraph)
g <- graph(c(1,2,2,3,3,4,4,1), )
plot(g, 
     vertex.color="green", 
     vertex.size=40, 
     edge.color="red")
g[]

g1 <- graph(c("Heena","Tina","Tina","Disha","Disha","Heena","Heena","Disha","Li","Disha"))
plot(g1,
     vertex.color="green", 
     vertex.size=40, 
     edge.color="red")
g1

#Network measures
degree(g1, mode="all")
degree(g1, mode="in")
degree(g1, mode="out")

diameter(g1, directed=F, weights=NA)
edge_density(g1,loops=F)
ecount(g1)/(vcount(g1)*(vcount(g1)-1))
reciprocity(g1)
closeness(g1, mode="all", weights=NA)
betweenness(g1, directed=T, weight=NA)
edge_betweenness(g1)

#Read data file
data <- read.csv(file.choose(), header=T)
y <- data.frame(data$first, data$second)

#Create network
net <- graph.data.frame(y, directed=T)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

#Histogram of node degree
hist(V(net)$degree,
     col="green",
     main='Histogram of node degree',
     ylab='Fbrequency',
     xlab='Degree of vertices')

#Network diagram
set.seed(222)
plot(net, vertex.color ="green", edge.arrow.size=0.3)

#Highlighting degrees and layouts
plot(net,vertex.color = rainbow(52), vertex.size=V(net)$degree*0.4,edge.arrow.size=0.2,layout=layout.fruchterman.reingold)
plot(net,vertex.color = rainbow(52), vertex.size=V(net)$degree*0.4,edge.arrow.size=0.2,layout=layout.graphopt)
plot(net,vertex.color = rainbow(52), vertex.size=V(net)$degree*0.4,edge.arrow.size=0.2,layout=layout.kamada.kawai)

#Hubs and authorities
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(net,vertex.size=hs*30,main="Hubs",vertex.color=rainbow(52),edge.arrow.size=0.2,layout=layout.kamada.kawai)
plot(net,vertex.size=as*30,main="Authorities",vertex.color=rainbow(52),edge.arrow.size=0.2,layout=layout.kamada.kawai)

#Community detection
net <- graph.data.frame(y,directed=F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,net)

