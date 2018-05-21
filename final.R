setwd("C:/Users/Griffin Noe/Google Drive/Soan 265/Final/soan265")
library(igraph)
library(RColorBrewer)
library(RCurl); library(ergm)
library(stargazer)

# setup -------------------------------------------------------------------
edgeList <- read.csv("links.csv", header=F)
attrList <- read.csv("nodes.csv", header=T)
edgeList <- as.matrix(edgeList)
g <- graph.edgelist(edgeList,directed=T)
g <- g + vertices("M","R","T")
# g <- as.undirected(g)

V(g)$smoke = attrList$Smoke[match(V(g)$name,attrList$id)]
V(g)$drugs = attrList$Drugs[match(V(g)$name,attrList$id)]
V(g)$drink = attrList$Alcohol[match(V(g)$name,attrList$id)]
V(g)$sport = attrList$Sport[match(V(g)$name,attrList$id)]
V(g)$famly = attrList$Family[match(V(g)$name,attrList$id)]
V(g)$smbin = attrList$smo[match(V(g)$name,attrList$id)] #smoking binary
V(g)$drbin = attrList$dru[match(V(g)$name,attrList$id)] #drugs binary
V(g)$albin = attrList$alc[match(V(g)$name,attrList$id)] #alcohol binary


# conversion --------------------------------------------------------------
class(g)
library(intergraph)
net<-asNetwork(g)
class(net)
detach(package:igraph)
library(statnet)

# attributeFactoring ------------------------------------------------------
smoke <- as.factor(get.vertex.attribute(net, "smoke"))
drugs <- as.factor(get.vertex.attribute(net, "drugs"))
drink <- as.factor(get.vertex.attribute(net, "drink"))
sport <- as.factor(get.vertex.attribute(net, "sport"))
famly <- as.factor(get.vertex.attribute(net, "famly"))
smbin <- as.factor(get.vertex.attribute(net, "smbin"))
drbin <- as.factor(get.vertex.attribute(net, "drbin"))
albin <- as.factor(get.vertex.attribute(net, "albin"))


# Five Number Summary -----------------------------------------------------
cat('The size is: ', network.size(net), '\n')
cat('The density is: ', gden(net), '\n')
cat('The number of components are: ', components(net), '\n')
lgc <- component.largest(net,result='graph')
gd <- geodist(lgc)
cat('The diameter of the largest component: ', max(gd$gdist), '\n')
cat('The clustering coefficient is: ', gtrans(net,mode='graph'))

# pallette ----------------------------------------------------------------
colfunc <- colorRampPalette(c("red", "blue"))
pal2 <- colfunc(2)
pal3 <- colfunc(3)
pal4 <- colfunc(4)
pal5 <- colfunc(5)

# homophilyPlots ----------------------------------------------------------
op <- par(mfrow=c(2,2))
plot(net, vertex.cex=3,main="smoke",displaylabels=F,vertex.col=pal3[smoke])
legend('topleft',
       legend=c('Never','Occasional','More Than Once Per Week'),
       col=pal3,pch=19,pt.cex = 1.5,bty='n')
plot(net, vertex.cex=3,main="drugs",displaylabels=F,vertex.col=pal4[drugs])
legend('topleft',
       legend=c('Never','Once','Occasional','Regular'),
       col=pal4,pch=19,pt.cex = 1.5,bty='n')
plot(net, vertex.cex=3,main="drink",displaylabels=F,vertex.col=pal5[drink])
legend('bottomleft',
       legend=c('Never','Once/Twice Per Year','Once a Month','Once a Week','More Than Once per Week'),
       col=pal5,pch=19,pt.cex = 1.5,bty='n')
plot(net, vertex.cex=3,main="sport",displaylabels=F,vertex.col=pal2[sport])
legend('topleft',legend=c('No','Yes'),col=pal2,pch=19,pt.cex=1.5,bty='n')
# plot(net, vertex.cex=3,main="alcohol",displaylabels=F,vertex.col=pal2[albin])
# plot(net, vertex.cex=3,main="drugs",displaylabels=F,vertex.col=pal2[drbin])
# plot(net, vertex.cex=3,main="smoking",displaylabels=F,vertex.col=pal2[smbin])
# plot(net, vertex.cex=3,main="family",displaylabels=F,vertex.col=pal2[famly])
# legend("topleft",legend=c('1','2','3','4','5(Yes)'),col=pal5,pch=19,pt.cex=1.5,bty='n')

# centralityPlots ---------------------------------------------------------
# op <- par(mfrow=c(2,1))
# gplot(net, usearrows=F, main='degree centrality',vertex.cex=degree(net,gmode="graph")/2)
# gplot(net, usearrows=F, main='betweenness', vertex.cex=betweenness(net,gmode="graph")/7)

# ergmSummary -------------------------------------------------------------

#Null Model
ergm1 <- ergm(net~edges)

#Focal Variable
ergm2 <- ergm(net~
                edges+
                nodematch("sport",diff=F))

#Uniform Homophilous Dyadic Characterstics
ergm3 <- ergm(net~
                edges+
                isolates+
                nodematch("smoke",diff=F)+
                nodematch("drugs",diff=F)+
                nodematch("drink",diff=F)+
                nodematch("sport",diff=F))

#Nodal Characteristics
ergm4 <- ergm(net~
                edges+
                isolates+
                nodematch("smoke",diff=F)+
                nodematch("drugs",diff=F)+
                nodematch("drink",diff=F)+
                nodematch("sport",diff=F)+
                nodefactor("smoke")+
                nodefactor("drugs")+
                nodefactor("drink")+
                nodefactor("sport"))

#Structural Indicators
ergm5 <- ergm(net~
                edges+
                isolates+
                nodematch("smoke",diff=F)+
                nodematch("drugs",diff=F)+
                nodematch("drink",diff=F)+
                nodematch("sport",diff=F)+
                nodefactor("smoke")+
                nodefactor("drugs")+
                nodefactor("drink")+
                nodefactor("sport")+
                gwdsp+
                gwesp+
                mutual)

ergm6 <- ergm(net~
                edges+
                isolates+
                nodematch("smoke",diff=F)+
                nodematch("drugs",diff=F)+
                nodematch("drink",diff=F)+
                nodematch("sport",diff=F)+
                nodefactor("smoke")+
                nodefactor("drugs")+
                nodefactor("drink")+
                nodefactor("sport")+
                gwdsp+
                gwesp+
                mutual+
                dgwesp+
                dgwdsp+
                simmelian)

# print(summary(ergm1));print(summary(ergm2));print(summary(ergm3))
stargazer(ergm1,ergm2,ergm3,ergm4,ergm5,ergm6,type='html',out='ergms.html')
# stargazer(ergm6,type='html',out='ergms.html')
op <- par(mfrow=c(2,3))
# plot(gof(ergm6))
simergm1 <- simulate(ergm1)
simergm2 <- simulate(ergm2)
simergm3 <- simulate(ergm3)
simergm4 <- simulate(ergm4)
simergm5 <- simulate(ergm5)
simergm6 <- simulate(ergm6)
# plot(net, main='actual',node.col='black')
plot(simergm1, main='Null Model')
plot(simergm2, main='Focal Variable')
plot(simergm3, main='Homophilous Dyadic')
plot(simergm4, main='Nodal')
plot(simergm5, main='Structural')
plot(simergm6, main='Structural 2')
