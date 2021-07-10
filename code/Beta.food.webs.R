library(String2AdjMatrix)
library(bipartite)
library(betalink)
library(igraph)
library(cheddar)
library(sp)

ToIgraph <- function(community, weight=NULL)
{
  if(is.null(TLPS(community)))
  {
    stop('The community has no trophic links')
  }
  else
  {
    tlps <- TLPS(community, link.properties=weight)
    if(!is.null(weight))
    {
      tlps$weight <- tlps[,weight]
    }
    return (graph.data.frame(tlps,
                             vertices=NPS(community),
                             directed=TRUE))
  }
}

#EVO
i.EVO.ELS2_1<-ToIgraph(EVO.ELS2_1)
plot.igraph(i.EVO.ELS2_1)
a.EVO.ELS2_1<-as_adjacency_matrix(i.EVO.ELS2_1,sparse=T)

i.EVO.ELS2_2<-ToIgraph(EVO.ELS2_2)
plot.igraph(i.EVO.ELS2_2)
a.EVO.ELS2_2<-as_adjacency_matrix(i.EVO.ELS2_2,sparse=T)

i.EVO.ELS2_3<-ToIgraph(EVO.ELS2_3)
plot.igraph(i.EVO.ELS2_3)
a.EVO.ELS2_3<-as_adjacency_matrix(i.EVO.ELS2_3,sparse=T)

i.EVO.ELS3_1<-ToIgraph(EVO.ELS3_1)
plot.igraph(i.EVO.ELS3_1)
a.EVO.ELS3_1<-as_adjacency_matrix(i.EVO.ELS3_1,sparse=T)

i.EVO.ELS3_2<-ToIgraph(EVO.ELS3_2)
plot.igraph(i.EVO.ELS3_2)
a.EVO.ELS3_2<-as_adjacency_matrix(i.EVO.ELS3_2,sparse=T)

i.EVO.ELS3_3<-ToIgraph(EVO.ELS3_3)
plot.igraph(i.EVO.ELS3_3)
a.EVO.ELS3_3<-as_adjacency_matrix(i.EVO.ELS3_3,sparse=T)

i.EVO.ELS4_1<-ToIgraph(EVO.ELS4_1)
plot.igraph(i.EVO.ELS4_1)
a.EVO.ELS4_1<-as_adjacency_matrix(i.EVO.ELS4_1,sparse=T)

i.EVO.ELS4_2<-ToIgraph(EVO.ELS4_2)
plot.igraph(i.EVO.ELS4_2)
a.EVO.ELS4_2<-as_adjacency_matrix(i.EVO.ELS4_2,sparse=T)

i.EVO.ELS4_3<-ToIgraph(EVO.ELS4_3)
plot.igraph(i.EVO.ELS4_3)
a.EVO.ELS4_3<-as_adjacency_matrix(i.EVO.ELS4_3,sparse=T)

i.EVO.ELS5_1<-ToIgraph(EVO.ELS5_1)
plot.igraph(i.EVO.ELS5_1)
a.EVO.ELS5_1<-as_adjacency_matrix(i.EVO.ELS5_1,sparse=T)

i.EVO.ELS5_2<-ToIgraph(EVO.ELS5_2)
plot.igraph(i.EVO.ELS5_2)
a.EVO.ELS5_2<-as_adjacency_matrix(i.EVO.ELS5_2,sparse=T)

i.EVO.ELS5_3<-ToIgraph(EVO.ELS5_3)
plot.igraph(i.EVO.ELS5_3)
a.EVO.ELS5_3<-as_adjacency_matrix(i.EVO.ELS5_3,sparse=T)

i.EVO.ELS6_1<-ToIgraph(EVO.ELS6_1)
plot.igraph(i.EVO.ELS6_1)
a.EVO.ELS6_1<-as_adjacency_matrix(i.EVO.ELS6_1,sparse=T,type="both")
aa.EVO.ELS6_1<-as.matrix(a.EVO.ELS6_1)

i.EVO.ELS6_2<-ToIgraph(EVO.ELS6_2)
plot.igraph(i.EVO.ELS6_2)
a.EVO.ELS6_2<-as_adjacency_matrix(i.EVO.ELS6_2,sparse=T)
aa.EVO.ELS6_2<-as.matrix(a.EVO.ELS6_2)

i.EVO.ELS6_3<-ToIgraph(EVO.ELS6_3)
plot.igraph(i.EVO.ELS6_3)
a.EVO.ELS6_3<-as_adjacency_matrix(i.EVO.ELS6_3,sparse=T)
aa.EVO.ELS6_3<-as.matrix(a.EVO.ELS6_3)

i.EVO.ELS7_1<-ToIgraph(EVO.ELS7_1)
plot.igraph(i.EVO.ELS7_1)
a.EVO.ELS7_1<-as_adjacency_matrix(i.EVO.ELS7_1,sparse=T)

i.EVO.ELS7_2<-ToIgraph(EVO.ELS7_2)
plot.igraph(i.EVO.ELS7_2)
a.EVO.ELS7_2<-as_adjacency_matrix(i.EVO.ELS7_2,sparse=T)

i.EVO.ELS7_3<-ToIgraph(EVO.ELS7_3)
plot.igraph(i.EVO.ELS7_3)
a.EVO.ELS7_3<-as_adjacency_matrix(i.EVO.ELS7_3,sparse=T)


i.EVO.ELS8_1<-ToIgraph(EVO.ELS8_1)
plot.igraph(i.EVO.ELS8_1)
a.EVO.ELS8_1<-as_adjacency_matrix(i.EVO.ELS8_1,sparse=T)

i.EVO.ELS8_2<-ToIgraph(EVO.ELS8_2)
plot.igraph(i.EVO.ELS8_2)
a.EVO.ELS8_2<-as_adjacency_matrix(i.EVO.ELS8_2,sparse=T)

i.EVO.ELS8_3<-ToIgraph(EVO.ELS8_3)
plot.igraph(i.EVO.ELS8_3)
a.EVO.ELS8_3<-as_adjacency_matrix(i.EVO.ELS8_3,sparse=T)

ig.EVO.ELS8_3 <- as.igraph(i.EVO.ELS8_3)
############

N<-list(i.EVO.ELS2_1,i.EVO.ELS2_2,i.EVO.ELS2_3,i.EVO.ELS3_1,i.EVO.ELS3_2,i.EVO.ELS3_3,i.EVO.ELS4_1,i.EVO.ELS4_2,i.EVO.ELS4_3,i.EVO.ELS5_1,i.EVO.ELS5_2,i.EVO.ELS5_3,i.EVO.ELS6_1,i.EVO.ELS6_2,i.EVO.ELS6_3,i.EVO.ELS7_1,i.EVO.ELS7_2,i.EVO.ELS7_3,i.EVO.ELS8_1,i.EVO.ELS8_2,i.EVO.ELS8_3)

names(N)<-c("ELS2_1","ELS2_2","ELS2_3","ELS3_1","ELS3_2","ELS3_3","ELS4_1","ELS4_2","ELS4_3","ELS5_1","ELS5_2","ELS5_3","ELS6_1","ELS6_2","ELS6_3","ELS7_1","ELS7_2","ELS7_3","ELS8_1","ELS8_2","ELS8_3")

betalink(i.EVO.ELS5_3,i.EVO.ELS5_2)


evoenv<-env%>%dplyr::filter(Network=="EVO")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")


GeoDist <- spDists(as.matrix(evoenv, latlon=TRUE))
colnames(GeoDist) <- rownames(GeoDist) <- rownames(evoenv)
GeoDist <- as.dist(GeoDist)

evoenv<-env%>%dplyr::filter(Network=="EVO")%>%dplyr::select(Site,Elevation)%>%column_to_rownames(var="Site")

net.dist <- dist(as.matrix(evoenv, latlon=TRUE))
colnames(net.dist) <- rownames(GeoDist) <- rownames(evoenv)
GeoDist <- as.dist(net.dist)


evoenv<-env%>%dplyr::filter(Network=="EVO")%>%dplyr::select(Site,River.dist.lake)%>%column_to_rownames(var="Site")

river.dist <- dist(as.matrix(evoenv))
colnames(river.dist) <- rownames(GeoDist) <- rownames(evoenv)
GeoDist <- as.dist(net.dist)
evoenv<-env%>%dplyr::filter(Network=="EVO")%>%dplyr::select(Site,SHRUB_SCRUB,Up.Lake.area,Head.river.dist,River.dist.lake,Elevation)%>%column_to_rownames(var="Site")

env.dist<-dist(evoenv)
metaweb<-metaweb(N)
net.beta <- network_betadiversity(N)
net.beta$GEO<-GeoDist
net.beta$netdist<-net.dist
net.beta$env<-env.dist
net.beta$riverdist<-river.dist



ggplot(net.beta, aes(x=S,y=ST))+geom_point()+geom_smooth(method = "lm")+geom_abline(intercept = 0, slope = 1, color="red", 
                                                                                   linetype="dashed", size=1.5)+ylim(0,1)+xlim(0,1)

ggplot(net.beta, aes(x=GEO,y=S))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=GEO,y=OS))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=GEO,y=WN))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=GEO,y=ST))+geom_point()+geom_smooth(method = "lm")


ggplot(net.beta, aes(x=netdist,y=S))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=netdist,y=OS))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=netdist,y=WN))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=netdist,y=ST))+geom_point()+geom_smooth(method = "lm")

ggplot(net.beta, aes(x=env,y=S))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=env,y=OS))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=env,y=WN))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=env,y=ST))+geom_point()+geom_smooth(method = "lm")

ggplot(net.beta, aes(x=river.dist,y=S))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=river.dist,y=OS))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=river.dist,y=WN))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=river.dist,y=ST))+geom_point()+geom_smooth(method = "lm")





name_networks
#R mangal paper code
#Prepare the environment
library(devtools)
# This line is needed on some linux distributions
if(getOption('unzip')=='') options ('unzip' = 'unzip')
# This installs the rmangal package
install_github('mangal-wg/rmangal')
library(rmangal)

mangal_url <-'http://mangal.io/'
api <-mangalapi(mangal_url)
df_from_A(a.EVO.ELS5_1)
