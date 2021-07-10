library(String2AdjMatrix)
library(bipartite)
library(betalink)
library(igraph)

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


i.EVO.ELS5_3<-ToIgraph(EVO.ELS5_3)
plot.igraph(i.EVO.ELS5_3)
a.EVO.ELS5_3<-as_adjacency_matrix(i.EVO.ELS5_3,sparse=T)

i.EVO.ELS5_2<-ToIgraph(EVO.ELS5_2)
plot.igraph(i.EVO.ELS5_2)
a.EVO.ELS5_2<-as_adjacency_matrix(i.EVO.ELS5_2,sparse=T)

i.EVO.ELS5_1<-ToIgraph(EVO.ELS5_1)
plot.igraph(i.EVO.ELS5_1)
a.EVO.ELS5_1<-as_adjacency_matrix(i.EVO.ELS5_1,sparse=T)


i.EVO.ELS6_3<-ToIgraph(EVO.ELS6_3)
plot.igraph(i.EVO.ELS6_3)
a.EVO.ELS6_3<-as_adjacency_matrix(i.EVO.ELS6_3,sparse=T)

i.EVO.ELS6_2<-ToIgraph(EVO.ELS6_2)
plot.igraph(i.EVO.ELS6_2)
a.EVO.ELS6_2<-as_adjacency_matrix(i.EVO.ELS6_2,sparse=T)

i.EVO.ELS6_1<-ToIgraph(EVO.ELS6_1)
plot.igraph(i.EVO.ELS6_1)
a.EVO.ELS6_1<-as_adjacency_matrix(i.EVO.ELS6_1,sparse=T)

i.EVO.ELS5_3<-ToIgraph(EVO.ELS5_3)
plot.igraph(i.EVO.ELS5_3)
a.EVO.ELS5_3<-as_adjacency_matrix(i.EVO.ELS5_3,sparse=T)

i.EVO.ELS5_2<-ToIgraph(EVO.ELS5_2)
plot.igraph(i.EVO.ELS5_2)
a.EVO.ELS5_2<-as_adjacency_matrix(i.EVO.ELS5_2,sparse=T)

i.EVO.ELS5_1<-ToIgraph(EVO.ELS5_1)
plot.igraph(i.EVO.ELS5_1)
a.EVO.ELS5_1<-as_adjacency_matrix(i.EVO.ELS5_1,sparse=T)
df<-betalink::df_from_A(a.EVO.ELS5_1)
ok<-as.list(i.EVO.ELS5_1)
df_from_A(a.EVO.ELS5_1)
N<-as.list(i.EVO.ELS5_1,i.EVO.ELS5_2)
name_networks("EVO.ELS5_1","EVO.ELS5_2")
betalink(i.EVO.ELS5_3,i.EVO.ELS5_2)

betalinkr_multi(a.EVO.ELS5_1,a.EVO.ELS5_2,a.EVO.ELS5_3)

network_betadiversity(N)


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
