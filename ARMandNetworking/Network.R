######################
##
## Gates 2021
##
## Practicing with Networks
##
################################

##################
## NOTE: Some R packages
## cause trouble with each other
## when loaded at the same time
##
## SOLUTION: use detach
##
## Example: igraph and Statnet packages
## cause some problems when loaded at the same time.
## It is best to detach one before loading the other.
## library(igraph)          TO LOAD
## detach(package:igraph)   TO DETACH
##
################################################################

library(igraph)
library(visNetwork)
library(networkD3)
#data(MisLinks, MisNodes)
library(igraph)
#https://dplyr.tidyverse.org/reference/mutate.html
library(dplyr)
## for pipes %>%
library(magrittr)
library(tkplot)




############################################
##
## Example : igraph
##
## For igraph, you need a LIST of edges
##
## ####################################
## The DATA:
## TinyEdgeDoc.csv
#######################################
# from    to    weight
# 2    3    11
# 2    4    4
# 2    1    17
# 3    4    6
# 1    2    1
##################################
##TinyNodeDoc.csv
####################################
# id    label
# 1    coffee
# 2    chocolate
# 3    soymilk
# 4    cake
# 5    bananas
######################################


# location="C:/Users/profa/Documents/RStudioFolder_1"
# setwd(location)

Myedges <- read.csv("TinyEdgeDoc.csv")
head(Myedges)
Mynodes <- read.csv("TinyNodeDoc.csv")
head(Mynodes)

## Make sure it works
My_igraph1 <- graph(edges=c(1,2, 2,3, 3,1, 1,1), n=3, directed=F)
plot(My_igraph1)

## Now - use a csv file and update the file data into the
## needed format.
## In Myedges, we have a a column called "from"
## and a column called "to".
## let's use
## https://rdrr.io/cran/igraph/man/graph_from_data_frame.html
## A data frame containing a symbolic edge list in the
## first two columns. Additional columns are considered as edge attributes.

##Parameters:
## d, vertices, and directed.
## d refers to the edge list
## vertices to the node list, and
## directed can be either TRUE or FALSE

(My_igraph2 <-
    graph_from_data_frame(d = Myedges, vertices = Mynodes, directed = TRUE))

E(My_igraph2)
E(My_igraph2)$weight

V(My_igraph2)$size = 10
## or you can set this in plot....

(E_Weight<-Myedges$weight)
(E(My_igraph2)$weight <- edge.betweenness(My_igraph2))
E(My_igraph2)$color <- "purple"

layout1 <- layout.fruchterman.reingold(My_igraph2)

## plot or tkplot........
#tkplot
plot(My_igraph2, edge.arrow.size = 0.3,
     vertex.size=E_Weight*5,
     vertex.color="lightblue",
     layout=layout1,
     edge.arrow.size=.5,
     vertex.label.cex=0.8,
     vertex.label.dist=2,
     edge.curved=0.2,
     vertex.label.color="black",
     edge.weight=5,
     edge.width=E(My_igraph2)$weight,
     #edge_density(My_igraph2)
     ## Affect edge lengths
     rescale = FALSE,
     ylim=c(0,14),
     xlim=c(0,20)
     )




#############################
## Example : visNetwork
##
## RE: http://www.htmlwidgets.org/showcase_visNetwork.html
##
#######################################################
#https://www.rdocumentation.org/packages/igraph/versions/1.2.6/topics/layout_with_fr
(nodes<-Mynodes)
edges<-Myedges
head(edges)
head(nodes)

visNetwork(nodes, edges, layout = "layout_with_fr",
           arrows="middle")


edges
(edges <- mutate(edges, width = weight, length=20))


visNetwork(nodes, edges) %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visEdges(arrows = "middle")


############################################
## Example : from a dataframe
#############################################

## Build the edges

(edges2 <- data.frame(
  from = edges$from,
  to = edges$to,
  # add labels on edges
  label = edges$weight,
  # length
  #length = c(100,500),
  # width
  width = edges$weight)
  # arrows
  #arrows = c("to", "from", "middle", "middle;to"),
  # dashes
  #dashes = c(TRUE, FALSE),
  # tooltip (html or character)
  #title = paste("Edge", 1:8),
  # smooth
  #smooth = c(FALSE, TRUE),
  # shadow
  #shadow = c(FALSE, TRUE, FALSE, TRUE)
)


## Build the nodes
(nodes2 <- data.frame(id = nodes$id, label = nodes$label))

MyVis<-visNetwork(nodes2, edges2, height = "500px", width = "100%")
MyVis %>% visNodes(color = list(background = "yellow",
                                border = "black"))


########################################################
## Examples 3 - from a dataset (record data)
## visNetwork
## To build a network - you must have relationships
## in your data.
##
## Not all data can be directly converted to a graph
##
## Building nodes - options:
#########################################################
##
#############################
## The DATA
#################################
##Node_List_Example.csv
#####################################
# node_id    Team    Tryouts
# Sally    Dance    1
# Ben    Soccer    5
# Kevin    Dance    2
# Sam    Soccer    10
# Kat    Dance    4
# Jin    Soccer    8
# Tania    Soccer    6
########################################


MyNodes <- read.csv("Node_List_Example.csv")
str(MyNodes)
MyNodes$Team<-as.factor(MyNodes$Team)
str(MyNodes)
head(MyNodes)

nodes3 <- data.frame(id = MyNodes$node_id,
                    label = MyNodes$node_id,                                 # add labels on nodes
                    #group = MyNodes$Team,                                     # add groups on nodes
                    value = 1:7,                                                # size adding value
                    shape = c("square", "triangle", "box",
                              "circle", "dot", "star", "diamond"),
                              #"ellipse"),
                              #, "database", "text", "diamond"),                   # control shape of nodes
                    title = MyNodes$Team,
                      #paste0("<p><b>", 1:7,"</b><br>Node !</p>"),         # tooltip (html or character)
                    color = c("red", "grey", "orange",
                              "lightblue", "pink", "blue", "green"),# color
                    shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE))                 # shadow

head(nodes3)

####  THE DATA:############################
## Edge_List_Example.csv
###########################################
# from    to    edgelabel    Team
# Sally    Ben    1    Dance
# Ben    Jin    5    Soccer
# Kevin    Kat    2    Dance
# Sam    Kat    10    Soccer
# Kat    Kat    4    Dance
# Jin    Sally    8    Soccer
# Tania    Jin    6    Soccer
######################################

## Build the edges
(MyEdges <- read.csv("Edge_List_Example.csv"))
(edges3 <- data.frame(from = MyEdges$from,
                    to = MyEdges$to,
                    label = MyEdges$edgelabel,
                      #paste("Edge", 1:7),                                 # add labels on edges
                    length = MyEdges$edgelabel                                        # length
                    #arrows = c("to", "from", "middle", "middle;to"),            # arrows
                    #dashes = c(TRUE, FALSE),                                    # dashes
                    #title = paste("Edge", 1:7)                                # tooltip (html or character)
                    #smooth = c(FALSE, TRUE),                                    # smooth
                    #shadow = c(FALSE, TRUE, FALSE, TRUE))
                    ))

visNetwork(nodes3, edges3, width = "100%")


####################################
## Example: Decision Trees
##
## Interactive and with visTree
###############################################

library(rpart)
##install.packages("sparkline")
library(sparkline)

# Decision tree
head(iris)

Iris_DT <- rpart(Species~., data=iris)

visTree(Iris_DT, data = iris, main = "Iris Decision Tree",
        highlightNearest = list(enabled = TRUE,
                                degree = list(from = 50000, to = 0),
                                hover = TRUE,
                                algorithm = "hierarchical"))
## other options
## edgesFontSize = , nodesFontSize = , minNodeSize =
## height = "800px", maxNodeSize = 30, edgesFontSize =  ,
##




###############################################
##
##  Intro NetworkD3
##
###############################################

## We need to prepare and format the data to work with NetworkD3
## FOr NetworkD3 we need to build a dataframe
## that specifies the:
## Source
## Target
## Value
## NodeID    which must start at 0




### Simple Example
(NetDataLinks<-read.csv("NetworkD3_links.csv"))


## OK  - here is the catch
## The NetworkD3 will not work unless you create or have
## columns that are the ID# of each node.

## Let's first convert our links and nodes into a standard graph
(MyGraph1 <- igraph::simplify(igraph::graph.data.frame(MyLinks, directed=TRUE)))

## Now we have a graph object and so can access the E and V
E(MyGraph1)
V(MyGraph1)
vcount(MyGraph1)
V(MyGraph1)$name

## Now we can BUILD the nodes
nodesList <- data.frame(ID = c(0:(igraph::vcount(MyGraph1) - 1)),
                       # because networkD3 library requires IDs to start at 0
                       nName = igraph::V(MyGraph1)$name)
## Node Degree
(nodesList <- cbind(nodesList, nodeDegree=igraph::degree(MyGraph1,
                              v = igraph::V(MyGraph1), mode = "all")))

## Now we can BUILD the Edges (Links)

## This function will get the ID# for a node of any name
getNodeID <- function(x){
  which(x == igraph::V(MyGraph1)$name) - 1  #IDs start at 0
}

## Check it!  IMPORTANT - this check will change depending on your data
## I am checking coffee because coffee is one of my nodes.
getNodeID("coffee")

head(NetDataLinks)

edgeList1 <- plyr::ddply(
  NetDataLinks, .variables = c("source", "target" , "edgevalue"),
  function (x) data.frame(SourceID = getNodeID(x$source),
                          TargetID = getNodeID(x$target)))

head(edgeList1)
nrow(edgeList1)

#############
## OK! Now we can use NetworkD3
#############################################

(MyD3<-networkD3::forceNetwork(Links = edgeList1,
                    Nodes = nodesList,
                    Source = "SourceID",
                    Target = "TargetID",
                    Value = "edgevalue",
                    NodeID = "nName",
                    Group = "nodeDegree",
                    opacity = 0.9
              ))

networkD3::saveNetwork(MyD3,
                       "NetD3_test.html", selfcontained = TRUE)


sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)


###################################
################### Sankey
#####################################################
networkD3::sankeyNetwork(Links = edgeList1,
                               Nodes = nodesList,
                               Source = "SourceID",
                               Target = "TargetID",
                               Value = "edgevalue",
                               NodeID = "nName")








#############################################
## Radial and dendrograms from hclust objects
#####################################################
## See Clustering and Distance with Novels.R