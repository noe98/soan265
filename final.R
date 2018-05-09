setwd("C:/Users/Griffin Noe/Google Drive/Soan 265/Scripts/final")
library(igraph)
library(RColorBrewer)
library(RCurl); library(ergm)

setupFunction <- function(){
  edgeList <- read.csv("links.csv", header=F)
  attrList <- read.csv("nodes.csv", header=T)
  edgeList <- as.matrix(edgeList)
  g <- graph.edgelist(edgeList,directed=F)
  
  V(g)$smoke = attrList$Smoke[match(V(g)$name,attrList$id)]
  V(g)$drugs = attrList$Drugs[match(V(g)$name,attrList$id)]
  V(g)$drink = attrList$Alcohol[match(V(g)$name,attrList$id)]
  V(g)$sport = attrList$Sport[match(V(g)$name,attrList$id)]
  V(g)$famly = attrList$Family[match(V(g)$name,attrList$id)]
}
igraphToStatnetFunction <- function(){
  class(g)
  library(intergraph)
  net<-asNetwork(g)
  class(net)
  detach(package:igraph)
  library(statnet)
}
palletteFunction <- function(){
  colfunc <- colorRampPalette(c("red", "blue"))
  pal2 <- colfunc(2)
  pal3 <- colfunc(3)
  pal4 <- colfunc(4)
  pal5 <- colfunc(5)
}
factorAttributeFunction <- function(){
  smoke <- as.factor(get.vertex.attribute(net, "smoke"))
  drugs <- as.factor(get.vertex.attribute(net, "drugs"))
  drink <- as.factor(get.vertex.attribute(net, "drink"))
  sport <- as.factor(get.vertex.attribute(net, "sport"))
  famly <- as.factor(get.vertex.attribute(net, "famly"))
}
homophilyPlotFunction <- function(){
  op <- par(mfrow=c(3,2))
  plot(net, vertex.cex=3,main="smoke",displaylabels=F,vertex.col=pal3[smoke])
  plot(net, vertex.cex=3,main="drugs",displaylabels=F,vertex.col=pal4[drugs])
  plot(net, vertex.cex=3,main="drink",displaylabels=F,vertex.col=pal5[drink])
  plot(net, vertex.cex=3,main="sport",displaylabels=F,vertex.col=pal2[sport])
  plot(net, vertex.cex=3,main="family",displaylabels=F,vertex.col=pal2[famly])
  
}
centralityPlotFunction <- function(){
  op <- par(mfrow=c(3,1))
  gplot(net, usearrows=F, vertex.cex=degree(net,gmode="graph")/2)
  gplot(net, usearrows=F, vertex.cex=betweenness(net,gmode="graph")/7)
  gplot(net, usearrows=F, vertex.cex=closeness(net,gmode="graph")*100000)
}
ergmSummaryFunction <- function(){
  ergmn <- ergm(net~
                  edges+
                  triangle+
                  nodematch("smoke",diff=T)+
                  nodematch("drugs",diff=T)+
                  nodematch("drink",diff=T)+
                  nodematch("sport",diff=T)+
                  nodematch("famly",diff=T))
  return(summary(ergmn))
}

setupFunction()
igraphToStatnetFunction()
palletteFunction()
factorAttributeFunction()
# homophilyPlotFunction()
centralityPlotFunction()
# print(ergmSummaryFunction())