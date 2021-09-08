# ###################################
#       Archaeological SNA in R Demo
#       University of the Philippines
#       Dean Arcega
# ###################################

rm(list=ls())

#Run this if the packages are not yet installed
install.packages(c("igraph", "ggraph", "tidyverse", "tidygraph"))

library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)

#You might need to run this to change the working directory. 
#Change directory to where you cloned the repository.
setwd("C:/Users/Dean/Documents/GitHub/archaeological-SNA-R-demo")
#Use "/" or "\\" instead of "\" (R does not read lone backslashes)

# Read our prepared matrix ------------------------------------------------

#This dataset is co-occurence of foreign tradeware ceramics among
#sites in the Laguna de Bay and Pasig river area

matrix <- read_csv("laguna_pasig_foreign_tradeware.csv")

##Assign Sitenames as row names for Node Labels##

#Save sitenames as a vector
sites <- matrix$Node

#Remove the node column 
matrix <- select(matrix, -Node)

#Make the rownames the sitenames instead
rownames(matrix) <- sites

## Now we can make our network object from an incidence matrix
network <- graph_from_incidence_matrix(matrix)
    
#Enter the matrix!
network

#Let's visualize it
plot(network) #Run this again. Notice how the spacing is randomized.


# Querying Network Attributes and Centrality Measures ---------------------

#This object has many attributes. We can query them:
V(network)$name 

#we can also convert our matrix into an edgelist 
as_edgelist(network)

#And our centrality measures:
degree(network) #more connections, more power
#Pila has the most complete set of artifacts

eigen_centrality(network)$vector #BETTER connections, more power 
#takes into account ego's alters

power_centrality(network) #Bonacich

# positive values imply that vertices become more powerful as their 
# alters become more powerful (as occurs in cooperative relations), while 
# negative values imply that vertices become more powerful only as their alters 
# become weaker (as occurs in competitive or antagonistic relations) -- igraph

betweenness(network) #which nodes are important in network flow?
closeness(network) #reciprocal of average distance to all nodes

#Our centrality measures are highly correlated,
#but they highlight different aspects of power

cor.test(degree(network), eigen_centrality(network)$vector)
cor.test(betweenness(network), closeness(network))

# Exploratory visualization -----------------------------------------------------

#Circles for sites, squares for artifacts
V(network)$shape <- ifelse(V(network)$type == FALSE, "circle", "square")

plot(network) #Notice how there is overlap for some

#We can also make node size proportional to degree
plot(network,
     vertex.size = degree(network))

# Play around with layout algorithms.
# Try to optimize two things:
# (1) edges are of more or less equal length 
# (2) there are as few crossing edges as possible

plot(network,
    layout = layout.fruchterman.reingold) #emphasizes centrality (puts central in middle)
#Sta Ana, Pila always in the center

plot(network,
     layout = layout.kamada.kawai) #More dispersed, less clutter

plot(network,
     layout = layout.circle)

#But for spares affiliation networks like ours, 
#a bipartite layout might do the trick

plot(network, 
     layout = layout.bipartite,
     vertex.label.cex = .6) #This decreases the font size

#Notice how the two different modes are highlighted

# Visualization for Publications ----------------------------------------------------
#The neater you want it to look, the more you have to code...
my_graph <- ggraph(network) + 
    geom_edge_link(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
    geom_node_point(color="#69b3a2", size=5) +
    geom_node_text(aes(label=name), repel = TRUE, size=4, color="#69b3a2") +
    theme_void() +
    theme(
        legend.position="none",
        plot.margin=unit(rep(1,4), "cm")
    ) 

#Notice how the syntax is different. We are calling the ggraph package now.

my_graph

#Let's try making an arc graph

#These have the advantage of highlighting clusters, 
#but are poor visualizations of network structure

arc_graph <- ggraph(network, layout="linear") + 
    geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
    geom_node_point(color="#69b3a2", size=5) +
    geom_node_text(aes(label=name), repel = T, size=2, color="#69b3a2", 
                    nudge_y=-0.1) +
    theme_void() +
    theme(
        legend.position="none",
        plot.margin=unit(rep(2,4), "cm")
    ) 

arc_graph


