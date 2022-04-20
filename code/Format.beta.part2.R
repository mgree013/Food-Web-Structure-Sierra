#Project:The Geography of High Elevation Benthic Food Webs
#Date: 03/21/19-Present
#Authors:Matthew D. Green, David Herbst, and Kurt E. Anderson

#Data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.2fqz612qw
################################################################################################################################

#Part 2: Beta Diversity Analysis and Figures
################################################################################################################################
#To Do:

#Meeting with Kurt: Add in resources for consumers. Use FFG from Poff etc.. 



#Igrpah and Cheddar communicate function
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

########################################################################
setwd()
#Load Rock Communities and Process for Beta diversity
RCLS1.1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS1.1')
i.ROCK.RCLS1.1<-ToIgraph(RCLS1.1)
plot.igraph(i.ROCK.RCLS1.1)
a.ROCK.RCLS1.1<-as_adjacency_matrix(i.ROCK.RCLS1.1,sparse=T)

RCLS2.1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS2.1')
i.ROCK.RCLS2.1<-ToIgraph(RCLS2.1)
plot.igraph(i.ROCK.RCLS2.1)
a.ROCK.RCLS2.1<-as_adjacency_matrix(i.ROCK.RCLS2.1,sparse=T)

RCLS2.2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS2.2')
i.ROCK.RCLS2.2<-ToIgraph(RCLS2.2)
plot.igraph(i.ROCK.RCLS2.2)
a.ROCK.RCLS2.2<-as_adjacency_matrix(i.ROCK.RCLS2.2,sparse=T)

RCLS2.3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS2.3')
i.ROCK.RCLS2.3<-ToIgraph(RCLS2.3)
plot.igraph(i.ROCK.RCLS2.3)
a.ROCK.RCLS2.3<-as_adjacency_matrix(i.ROCK.RCLS2.3,sparse=T)

RCLS2.4<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS2.4')
i.ROCK.RCLS2.4<-ToIgraph(RCLS2.4)
plot.igraph(i.ROCK.RCLS2.4)
a.ROCK.RCLS2.4<-as_adjacency_matrix(i.ROCK.RCLS2.4,sparse=T)

RCLS2.5<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS2.5')
i.ROCK.RCLS2.5<-ToIgraph(RCLS2.5)
plot.igraph(i.ROCK.RCLS2.5)
a.ROCK.RCLS2.5<-as_adjacency_matrix(i.ROCK.RCLS2.5,sparse=T)

RCLS2.6<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS2.6')
i.ROCK.RCLS2.6<-ToIgraph(RCLS2.6)
plot.igraph(i.ROCK.RCLS2.6)
a.ROCK.RCLS2.6<-as_adjacency_matrix(i.ROCK.RCLS2.6,sparse=T)

RCLS3.1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS3.1')
i.ROCK.RCLS3.1<-ToIgraph(RCLS3.1)
plot.igraph(i.ROCK.RCLS3.1)
a.ROCK.RCLS3.1<-as_adjacency_matrix(i.ROCK.RCLS3.1,sparse=T)

RCLS3.2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS3.2')
i.ROCK.RCLS3.2<-ToIgraph(RCLS3.2)
plot.igraph(i.ROCK.RCLS3.2)
a.ROCK.RCLS3.2<-as_adjacency_matrix(i.ROCK.RCLS3.2,sparse=T)

RCLS3.3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS3.3')
i.ROCK.RCLS3.3<-ToIgraph(RCLS3.3)
plot.igraph(i.ROCK.RCLS3.3)
a.ROCK.RCLS3.3<-as_adjacency_matrix(i.ROCK.RCLS3.3,sparse=T)

RCLS3.4<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS3.4')
i.ROCK.RCLS3.4<-ToIgraph(RCLS3.4)
plot.igraph(i.ROCK.RCLS3.4)
a.ROCK.RCLS3.4<-as_adjacency_matrix(i.ROCK.RCLS3.4,sparse=T)

RCLS3.5<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS3.5')
i.ROCK.RCLS3.5<-ToIgraph(RCLS3.5)
plot.igraph(i.ROCK.RCLS3.5)
a.ROCK.RCLS3.5<-as_adjacency_matrix(i.ROCK.RCLS3.5,sparse=T)

RCLS4.1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS4.1')
i.ROCK.RCLS4.1<-ToIgraph(RCLS4.1)
plot.igraph(i.ROCK.RCLS4.1)
a.ROCK.RCLS4.1<-as_adjacency_matrix(i.ROCK.RCLS4.1,sparse=T,type="both")
aa.ROCK.RCLS4.1<-as.matrix(a.ROCK.RCLS4.1)

RCLS4.2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS4.2')
i.ROCK.RCLS4.2<-ToIgraph(RCLS4.2)
plot.igraph(i.ROCK.RCLS4.2)
a.ROCK.RCLS4.2<-as_adjacency_matrix(i.ROCK.RCLS4.2,sparse=T)
aa.ROCK.RCLS4.2<-as.matrix(a.ROCK.RCLS4.2)

RCLS4.3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS4.3')
i.ROCK.RCLS4.3<-ToIgraph(RCLS4.3)
plot.igraph(i.ROCK.RCLS4.3)
a.ROCK.RCLS4.3<-as_adjacency_matrix(i.ROCK.RCLS4.3,sparse=T)
aa.ROCK.RCLS4.3<-as.matrix(a.ROCK.RCLS4.3)

RCLS5.1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS5.1')
i.ROCK.RCLS5.1<-ToIgraph(RCLS5.1)
plot.igraph(i.ROCK.RCLS5.1)
a.ROCK.RCLS5.1<-as_adjacency_matrix(i.ROCK.RCLS5.1,sparse=T)

RCLS5.2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS5.2')
i.ROCK.RCLS5.2<-ToIgraph(RCLS5.2)
plot.igraph(i.ROCK.RCLS5.2)
a.ROCK.RCLS5.2<-as_adjacency_matrix(i.ROCK.RCLS5.2,sparse=T)

RCLS5.3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS5.3')
i.ROCK.RCLS5.3<-ToIgraph(RCLS5.3)
plot.igraph(i.ROCK.RCLS5.3)
a.ROCK.RCLS5.3<-as_adjacency_matrix(i.ROCK.RCLS5.3,sparse=T)

RCLS6.1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS6.1')
i.ROCK.RCLS6.1<-ToIgraph(RCLS6.1)
plot.igraph(i.ROCK.RCLS6.1)
a.ROCK.RCLS6.1<-as_adjacency_matrix(i.ROCK.RCLS6.1,sparse=T)

RCLS6.2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS6.2')
i.ROCK.RCLS6.2<-ToIgraph(RCLS6.2)
plot.igraph(i.ROCK.RCLS6.2)
a.ROCK.RCLS6.2<-as_adjacency_matrix(i.ROCK.RCLS6.2,sparse=T)

RCLS6.3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS6.3')
i.ROCK.RCLS6.3<-ToIgraph(RCLS6.3)
plot.igraph(i.ROCK.RCLS6.3)
a.ROCK.RCLS6.3<-as_adjacency_matrix(i.ROCK.RCLS6.3,sparse=T)

RCLS6.4<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS6.4')
i.ROCK.RCLS6.4<-ToIgraph(RCLS6.4)
plot.igraph(i.ROCK.RCLS6.4)
a.ROCK.RCLS6.4<-as_adjacency_matrix(i.ROCK.RCLS6.4,sparse=T)

RCLS6.5<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS6.5')
i.ROCK.RCLS6.5<-ToIgraph(RCLS6.5)
plot.igraph(i.ROCK.RCLS6.5)
a.ROCK.RCLS6.5<-as_adjacency_matrix(i.ROCK.RCLS6.5,sparse=T)

RCLS7.1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS7.1')
i.ROCK.RCLS7.1<-ToIgraph(RCLS7.1)
plot.igraph(i.ROCK.RCLS7.1)
a.ROCK.RCLS7.1<-as_adjacency_matrix(i.ROCK.RCLS7.1,sparse=T)

RCLS7.2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS7.2')
i.ROCK.RCLS7.2<-ToIgraph(RCLS7.2)
plot.igraph(i.ROCK.RCLS7.2)
a.ROCK.RCLS7.2<-as_adjacency_matrix(i.ROCK.RCLS7.2,sparse=T)

RCLS7.3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS7.3')
i.ROCK.RCLS7.3<-ToIgraph(RCLS7.3)
plot.igraph(i.ROCK.RCLS7.3)
a.ROCK.RCLS7.3<-as_adjacency_matrix(i.ROCK.RCLS7.3,sparse=T)

RCLS7.4<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS7.4')
i.ROCK.RCLS7.4<-ToIgraph(RCLS7.4)
plot.igraph(i.ROCK.RCLS7.4)
a.ROCK.RCLS7.4<-as_adjacency_matrix(i.ROCK.RCLS7.4,sparse=T)

RCLS7.5<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS7.5')
i.ROCK.RCLS7.5<-ToIgraph(RCLS7.5)
plot.igraph(i.ROCK.RCLS7.5)
a.ROCK.RCLS7.5<-as_adjacency_matrix(i.ROCK.RCLS7.5,sparse=T)

RCLS7.6<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS7.6')
i.ROCK.RCLS7.6<-ToIgraph(RCLS7.6)
plot.igraph(i.ROCK.RCLS7.6)
a.ROCK.RCLS7.6<-as_adjacency_matrix(i.ROCK.RCLS7.6,sparse=T)

RCLS7.7<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS7.7')
i.ROCK.RCLS7.7<-ToIgraph(RCLS7.7)
plot.igraph(i.ROCK.RCLS7.1)
a.ROCK.RCLS7.7<-as_adjacency_matrix(i.ROCK.RCLS7.7,sparse=T)

RCLS8.1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS8.1')
i.ROCK.RCLS8.1<-ToIgraph(RCLS8.1)
plot.igraph(i.ROCK.RCLS6.4)
a.ROCK.RCLS8.1<-as_adjacency_matrix(i.ROCK.RCLS8.1,sparse=T)

RCLS8.2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS8.2')
i.ROCK.RCLS8.2<-ToIgraph(RCLS8.2)
plot.igraph(i.ROCK.RCLS8.2)
a.ROCK.RCLS8.2<-as_adjacency_matrix(i.ROCK.RCLS8.2,sparse=T)

RCLS8.3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS8.3')
i.ROCK.RCLS8.3<-ToIgraph(RCLS8.3)
plot.igraph(i.ROCK.RCLS8.3)
a.ROCK.RCLS8.3<-as_adjacency_matrix(i.ROCK.RCLS8.3,sparse=T)

RCLS8.4<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS8.4')
i.ROCK.RCLS8.4<-ToIgraph(RCLS8.4)
plot.igraph(i.ROCK.RCLS8.4)
a.ROCK.RCLS8.4<-as_adjacency_matrix(i.ROCK.RCLS8.4,sparse=T)

RCLS8.5<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS8.5')
i.ROCK.RCLS8.5<-ToIgraph(RCLS8.5)
plot.igraph(i.ROCK.RCLS8.5)
a.ROCK.RCLS8.5<-as_adjacency_matrix(i.ROCK.RCLS8.5,sparse=T)

RCLS8.7<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/communities/RCLS8.7')
i.ROCK.RCLS8.7<-ToIgraph(RCLS8.7)
plot.igraph(i.ROCK.RCLS8.7)
a.ROCK.RCLS8.7<-as_adjacency_matrix(i.ROCK.RCLS8.7,sparse=T)

#Calculate beta diversity for all
#EVO
EVO.ELS2_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS2_1')
i.EVO.ELS2_1<-ToIgraph(EVO.ELS2_1)
plot.igraph(i.EVO.ELS2_1)
a.EVO.ELS2_1<-as_adjacency_matrix(i.EVO.ELS2_1,sparse=T)

EVO.ELS2_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS2_2')
i.EVO.ELS2_2<-ToIgraph(EVO.ELS2_2)
plot.igraph(i.EVO.ELS2_2)
a.EVO.ELS2_2<-as_adjacency_matrix(i.EVO.ELS2_2,sparse=T)

EVO.ELS2_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS2_3')
i.EVO.ELS2_3<-ToIgraph(EVO.ELS2_3)
plot.igraph(i.EVO.ELS2_3)
a.EVO.ELS2_3<-as_adjacency_matrix(i.EVO.ELS2_3,sparse=T)

EVO.ELS3_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS3_1')
i.EVO.ELS3_1<-ToIgraph(EVO.ELS3_1)
plot.igraph(i.EVO.ELS3_1)
a.EVO.ELS3_1<-as_adjacency_matrix(i.EVO.ELS3_1,sparse=T)

EVO.ELS3_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS3_2')
i.EVO.ELS3_2<-ToIgraph(EVO.ELS3_2)
plot.igraph(i.EVO.ELS3_2)
a.EVO.ELS3_2<-as_adjacency_matrix(i.EVO.ELS3_2,sparse=T)

EVO.ELS3_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS3_3')
i.EVO.ELS3_3<-ToIgraph(EVO.ELS3_3)
plot.igraph(i.EVO.ELS3_3)
a.EVO.ELS3_3<-as_adjacency_matrix(i.EVO.ELS3_3,sparse=T)

EVO.ELS4_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS4_1')
i.EVO.ELS4_1<-ToIgraph(EVO.ELS4_1)
plot.igraph(i.EVO.ELS4_1)
a.EVO.ELS4_1<-as_adjacency_matrix(i.EVO.ELS4_1,sparse=T)

EVO.ELS4_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS4_2')
i.EVO.ELS4_2<-ToIgraph(EVO.ELS4_2)
plot.igraph(i.EVO.ELS4_2)
a.EVO.ELS4_2<-as_adjacency_matrix(i.EVO.ELS4_2,sparse=T)

EVO.ELS4_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS4_3')
i.EVO.ELS4_3<-ToIgraph(EVO.ELS4_3)
plot.igraph(i.EVO.ELS4_3)
a.EVO.ELS4_3<-as_adjacency_matrix(i.EVO.ELS4_3,sparse=T)

EVO.ELS5_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS5_1')
i.EVO.ELS5_1<-ToIgraph(EVO.ELS5_1)
plot.igraph(i.EVO.ELS5_1)
a.EVO.ELS5_1<-as_adjacency_matrix(i.EVO.ELS5_1,sparse=T)

EVO.ELS5_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS5_2')
i.EVO.ELS5_2<-ToIgraph(EVO.ELS5_2)
plot.igraph(i.EVO.ELS5_2)
a.EVO.ELS5_2<-as_adjacency_matrix(i.EVO.ELS5_2,sparse=T)

EVO.ELS5_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS5_3')
i.EVO.ELS5_3<-ToIgraph(EVO.ELS5_3)
plot.igraph(i.EVO.ELS5_3)
a.EVO.ELS5_3<-as_adjacency_matrix(i.EVO.ELS5_3,sparse=T)

EVO.ELS6_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS6_1')
i.EVO.ELS6_1<-ToIgraph(EVO.ELS6_1)
plot.igraph(i.EVO.ELS6_1)
a.EVO.ELS6_1<-as_adjacency_matrix(i.EVO.ELS6_1,sparse=T,type="both")
aa.EVO.ELS6_1<-as.matrix(a.EVO.ELS6_1)

EVO.ELS6_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS6_2')
i.EVO.ELS6_2<-ToIgraph(EVO.ELS6_2)
plot.igraph(i.EVO.ELS6_2)
a.EVO.ELS6_2<-as_adjacency_matrix(i.EVO.ELS6_2,sparse=T)
aa.EVO.ELS6_2<-as.matrix(a.EVO.ELS6_2)

EVO.ELS6_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS6_3')
i.EVO.ELS6_3<-ToIgraph(EVO.ELS6_3)
plot.igraph(i.EVO.ELS6_3)
a.EVO.ELS6_3<-as_adjacency_matrix(i.EVO.ELS6_3,sparse=T)
aa.EVO.ELS6_3<-as.matrix(a.EVO.ELS6_3)

EVO.ELS7_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS7_1')
i.EVO.ELS7_1<-ToIgraph(EVO.ELS7_1)
plot.igraph(i.EVO.ELS7_1)
a.EVO.ELS7_1<-as_adjacency_matrix(i.EVO.ELS7_1,sparse=T)

EVO.ELS7_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS7_2')
i.EVO.ELS7_2<-ToIgraph(EVO.ELS7_2)
plot.igraph(i.EVO.ELS7_2)
a.EVO.ELS7_2<-as_adjacency_matrix(i.EVO.ELS7_2,sparse=T)

EVO.ELS7_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS7_3')
i.EVO.ELS7_3<-ToIgraph(EVO.ELS7_3)
plot.igraph(i.EVO.ELS7_3)
a.EVO.ELS7_3<-as_adjacency_matrix(i.EVO.ELS7_3,sparse=T)

EVO.ELS8_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS8_1')
i.EVO.ELS8_1<-ToIgraph(EVO.ELS8_1)
plot.igraph(i.EVO.ELS8_1)
a.EVO.ELS8_1<-as_adjacency_matrix(i.EVO.ELS8_1,sparse=T)

EVO.ELS8_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS8_2')
i.EVO.ELS8_2<-ToIgraph(EVO.ELS8_2)
plot.igraph(i.EVO.ELS8_2)
a.EVO.ELS8_2<-as_adjacency_matrix(i.EVO.ELS8_2,sparse=T)

EVO.ELS8_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/communities/ELS8_3')
i.EVO.ELS8_3<-ToIgraph(EVO.ELS8_3)
plot.igraph(i.EVO.ELS8_3)
a.EVO.ELS8_3<-as_adjacency_matrix(i.EVO.ELS8_3,sparse=T)

#Cascade
Cascade.CLS1_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS1_1')
i.Cascade.CLS1_1<-ToIgraph(Cascade.CLS1_1)
plot.igraph(i.Cascade.CLS1_1)
a.Cascade.CLS1_1<-as_adjacency_matrix(i.Cascade.CLS1_1,sparse=T)

Cascade.CLS1_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS1_3')
i.Cascade.CLS1_3<-ToIgraph(Cascade.CLS1_3)
plot.igraph(i.Cascade.CLS1_3)
a.Cascade.CLS1_3<-as_adjacency_matrix(i.Cascade.CLS1_3,sparse=T)

Cascade.CLS2_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS2_1')
i.Cascade.CLS2_1<-ToIgraph(Cascade.CLS2_1)
plot.igraph(i.Cascade.CLS2_1)
a.Cascade.CLS2_1<-as_adjacency_matrix(i.Cascade.CLS2_1,sparse=T)

Cascade.CLS2_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS2_3')
i.Cascade.CLS2_3<-ToIgraph(Cascade.CLS2_3)
plot.igraph(i.Cascade.CLS2_3)
a.Cascade.CLS2_3<-as_adjacency_matrix(i.Cascade.CLS2_3,sparse=T)

Cascade.CLS3_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS3_1')
i.Cascade.CLS3_1<-ToIgraph(Cascade.CLS3_1)
plot.igraph(i.Cascade.CLS3_1)
a.Cascade.CLS3_1<-as_adjacency_matrix(i.Cascade.CLS3_1,sparse=T)

Cascade.CLS3_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS3_2')
i.Cascade.CLS3_2<-ToIgraph(Cascade.CLS3_2)
plot.igraph(i.Cascade.CLS3_2)
a.Cascade.CLS3_2<-as_adjacency_matrix(i.Cascade.CLS3_2,sparse=T)

Cascade.CLS3_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS3_3')
i.Cascade.CLS3_3<-ToIgraph(Cascade.CLS3_3)
plot.igraph(i.Cascade.CLS3_3)
a.Cascade.CLS3_3<-as_adjacency_matrix(i.Cascade.CLS3_3,sparse=T)

Cascade.CLS4_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS4_1')
i.Cascade.CLS4_1<-ToIgraph(Cascade.CLS4_1)
plot.igraph(i.Cascade.CLS4_1)
a.Cascade.CLS4_1<-as_adjacency_matrix(i.Cascade.CLS4_1,sparse=T)

Cascade.CLS4_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS4_2')
i.Cascade.CLS4_2<-ToIgraph(Cascade.CLS4_2)
plot.igraph(i.Cascade.CLS4_2)
a.Cascade.CLS4_2<-as_adjacency_matrix(i.Cascade.CLS4_2,sparse=T)

Cascade.CLS4_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS4_3')
i.Cascade.CLS4_3<-ToIgraph(Cascade.CLS4_3)
plot.igraph(i.Cascade.CLS4_3)
a.Cascade.CLS4_3<-as_adjacency_matrix(i.Cascade.CLS4_3,sparse=T)

Cascade.CLS5_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus/communities/CLS5_1')
i.Cascade.CLS5_1<-ToIgraph(Cascade.CLS5_1)
plot.igraph(i.Cascade.CLS5_1)
a.Cascade.CLS5_1<-as_adjacency_matrix(i.Cascade.CLS5_1,sparse=T)

#RAE
RAE.RLS1_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS1_3')
i.RAE.RLS1_3<-ToIgraph(RAE.RLS1_3)
plot.igraph(i.RAE.RLS1_3)
a.RAE.RLS1_3<-as_adjacency_matrix(i.RAE.RLS1_3,sparse=T)

RAE.RLS2_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS2_1')
i.RAE.RLS2_1<-ToIgraph(RAE.RLS2_1)
plot.igraph(i.RAE.RLS2_1)
a.RAE.RLS2_1<-as_adjacency_matrix(i.RAE.RLS2_1,sparse=T)

RAE.RLS3_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS3_1')
i.RAE.RLS3_1<-ToIgraph(RAE.RLS3_1)
plot.igraph(i.RAE.RLS3_1)
a.RAE.RLS3_1<-as_adjacency_matrix(i.RAE.RLS3_1,sparse=T)

RAE.RLS3_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS3_2')
i.RAE.RLS3_2<-ToIgraph(RAE.RLS3_2)
plot.igraph(i.RAE.RLS3_2)
a.RAE.RLS3_2<-as_adjacency_matrix(i.RAE.RLS3_2,sparse=T)

RAE.RLS4_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS4_1')
i.RAE.RLS4_1<-ToIgraph(RAE.RLS4_1)
plot.igraph(i.RAE.RLS4_1)
a.RAE.RLS4_1<-as_adjacency_matrix(i.RAE.RLS4_1,sparse=T)

RAE.RLS4_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS4_2')
i.RAE.RLS4_2<-ToIgraph(RAE.RLS4_2)
plot.igraph(i.RAE.RLS4_2)
a.RAE.RLS4_2<-as_adjacency_matrix(i.RAE.RLS4_2,sparse=T)

RAE.RLS4_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS4_3')
i.RAE.RLS4_3<-ToIgraph(RAE.RLS4_3)
plot.igraph(i.RAE.RLS4_3)
a.RAE.RLS4_3<-as_adjacency_matrix(i.RAE.RLS4_3,sparse=T)

RAE.RLS5_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS5_1')
i.RAE.RLS5_1<-ToIgraph(RAE.RLS5_1)
plot.igraph(i.RAE.RLS5_1)
a.RAE.RLS5_1<-as_adjacency_matrix(i.RAE.RLS5_1,sparse=T)

RAE.RLS5_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS5_2')
i.RAE.RLS5_2<-ToIgraph(RAE.RLS5_2)
plot.igraph(i.RAE.RLS5_2)
a.RAE.RLS5_2<-as_adjacency_matrix(i.RAE.RLS5_2,sparse=T)

RAE.RLS5_3<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS5_3')
i.RAE.RLS5_3<-ToIgraph(RAE.RLS5_3)
plot.igraph(i.RAE.RLS5_3)
a.RAE.RLS5_3<-as_adjacency_matrix(i.RAE.RLS5_3,sparse=T)

RAE.RLS6_1<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS6_1')
i.RAE.RLS6_1<-ToIgraph(RAE.RLS6_1)
plot.igraph(i.RAE.RLS6_1)
a.RAE.RLS6_1<-as_adjacency_matrix(i.RAE.RLS6_1,sparse=T)

RAE.RLS6_2<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/communities/RLS6_2')
i.RAE.RLS6_2<-ToIgraph(RAE.RLS6_2)
plot.igraph(i.RAE.RLS6_2)
a.RAE.RLS6_2<-as_adjacency_matrix(i.RAE.RLS6_2,sparse=T)

#BUbbbs
BUBBS.Outlet.10477.trt.2003<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.10477.trt.2003')
i.BUBBS.Outlet.10477.trt.2003<-ToIgraph(BUBBS.Outlet.10477.trt.2003)
plot.igraph(i.BUBBS.Outlet.10477.trt.2003)
a.BUBBS.Outlet.10477.trt.2003<-as_adjacency_matrix(i.BUBBS.Outlet.10477.trt.2003,sparse=T)

BUBBS.Outlet.10477.trt.2004<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.10477.trt.2004')
i.BUBBS.Outlet.10477.trt.2004<-ToIgraph(BUBBS.Outlet.10477.trt.2004)
plot.igraph(i.BUBBS.Outlet.10477.trt.2004)
a.BUBBS.Outlet.10477.trt.2004<-as_adjacency_matrix(i.BUBBS.Outlet.10477.trt.2004,sparse=T)

BUBBS.Outlet.10477.trt.2011<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.10477.trt.2011')
i.BUBBS.Outlet.10477.trt.2011<-ToIgraph(BUBBS.Outlet.10477.trt.2011)
plot.igraph(i.BUBBS.Outlet.10477.trt.2011)
a.BUBBS.Outlet.10477.trt.2011<-as_adjacency_matrix(i.BUBBS.Outlet.10477.trt.2011,sparse=T)

BUBBS.Outlet.10487.trt.2003<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.10487.trt.2003')
i.BUBBS.Outlet.10487.trt.2003<-ToIgraph(BUBBS.Outlet.10487.trt.2003)
plot.igraph(i.BUBBS.Outlet.10487.trt.2003)
a.BUBBS.Outlet.10487.trt.2003<-as_adjacency_matrix(i.BUBBS.Outlet.10487.trt.2003,sparse=T)

BUBBS.Outlet.10487.trt.2004<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.10487.trt.2004')
i.BUBBS.Outlet.10487.trt.2004<-ToIgraph(BUBBS.Outlet.10487.trt.2004)
plot.igraph(i.BUBBS.Outlet.10487.trt.2004)
a.BUBBS.Outlet.10487.trt.2004<-as_adjacency_matrix(i.BUBBS.Outlet.10487.trt.2004,sparse=T)

BUBBS.Outlet.10487.trt.2011<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.10487.trt.2011')
i.BUBBS.Outlet.10487.trt.2011<-ToIgraph(BUBBS.Outlet.10487.trt.2011)
plot.igraph(i.BUBBS.Outlet.10487.trt.2011)
a.BUBBS.Outlet.10487.trt.2011<-as_adjacency_matrix(i.BUBBS.Outlet.10487.trt.2011,sparse=T)

BUBBS.Outlet.11007.fishless.2003<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.11007.fishless.2003')
i.BUBBS.Outlet.11007.fishless.2003<-ToIgraph(BUBBS.Outlet.11007.fishless.2003)
plot.igraph(i.BUBBS.Outlet.11007.fishless.2003)
a.BUBBS.Outlet.11007.fishless.2003<-as_adjacency_matrix(i.BUBBS.Outlet.11007.fishless.2003,sparse=T)

BUBBS.Outlet.11007.fishless.2004<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.11007.fishless.2004')
i.BUBBS.Outlet.11007.fishless.2004<-ToIgraph(BUBBS.Outlet.11007.fishless.2004)
plot.igraph(i.BUBBS.Outlet.11007.fishless.2004)
a.BUBBS.Outlet.11007.fishless.2004<-as_adjacency_matrix(i.BUBBS.Outlet.11007.fishless.2004,sparse=T)

BUBBS.Outlet.11007.fishless.2011<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.11007.fishless.2011')
i.BUBBS.Outlet.11007.fishless.2011<-ToIgraph(BUBBS.Outlet.11007.fishless.2011)
plot.igraph(i.BUBBS.Outlet.11007.fishless.2011)
a.BUBBS.Outlet.11007.fishless.2011<-as_adjacency_matrix(i.BUBBS.Outlet.11007.fishless.2011,sparse=T)

Bubbs.Outlet.10494.trt.2012<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.10494.trt.2012')
i.Bubbs.Outlet.10494.trt.2012<-ToIgraph(Bubbs.Outlet.10494.trt.2012)
plot.igraph(i.Bubbs.Outlet.10494.trt.2012)
a.Bubbs.Outlet.10494.trt.2012<-as_adjacency_matrix(i.Bubbs.Outlet.10494.trt.2012,sparse=T)

Bubbs.Outlet.Vidette.below.2003<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.Vidette.below.2003')
i.Bubbs.Outlet.Vidette.below.2003<-ToIgraph(Bubbs.Outlet.Vidette.below.2003)
plot.igraph(i.Bubbs.Outlet.Vidette.below.2003)
a.Bubbs.Outlet.Vidette.below.2003<-as_adjacency_matrix(i.Bubbs.Outlet.Vidette.below.2003,sparse=T)

Bubbs.Outlet.Vidette.below.2004<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.Vidette.below.2004')
i.Bubbs.Outlet.Vidette.below.2004<-ToIgraph(Bubbs.Outlet.Vidette.below.2004)
plot.igraph(i.Bubbs.Outlet.Vidette.below.2004)
a.Bubbs.Outlet.Vidette.below.2004<-as_adjacency_matrix(i.Bubbs.Outlet.Vidette.below.2004,sparse=T)

Bubbs.Outlet.Vidette.below.2011<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.Vidette.below.2011')
i.Bubbs.Outlet.Vidette.below.2011<-ToIgraph(Bubbs.Outlet.Vidette.below.2011)
plot.igraph(i.Bubbs.Outlet.Vidette.below.2011)
a.Bubbs.Outlet.Vidette.below.2011<-as_adjacency_matrix(i.Bubbs.Outlet.Vidette.below.2011,sparse=T)

Bubbs.Outlet.Vidette.below.2012<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/communities/Outlet.Vidette.below.2012')
i.Bubbs.Outlet.Vidette.below.2012<-ToIgraph(Bubbs.Outlet.Vidette.below.2012)
plot.igraph(i.Bubbs.Outlet.Vidette.below.2012)
a.Bubbs.Outlet.Vidette.below.2012<-as_adjacency_matrix(i.Bubbs.Outlet.Vidette.below.2012,sparse=T)


#KERN
KERN.10029<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10029')
i.KERN.10029<-ToIgraph(KERN.10029)
plot.igraph(i.KERN.10029)
a.KERN.10029<-as_adjacency_matrix(i.KERN.10029,sparse=T)

KERN.10030<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10030')
i.KERN.10030<-ToIgraph(KERN.10030)
plot.igraph(i.KERN.10030)
a.KERN.10030<-as_adjacency_matrix(i.KERN.10030,sparse=T)

KERN.10031<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10031')
i.KERN.10031<-ToIgraph(KERN.10031)
plot.igraph(i.KERN.10031)
a.KERN.10031<-as_adjacency_matrix(i.KERN.10031,sparse=T)

KERN.10032<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10032')
i.KERN.10032<-ToIgraph(KERN.10032)
plot.igraph(i.KERN.10032)
a.KERN.10032<-as_adjacency_matrix(i.KERN.10032,sparse=T)

KERN.10033<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10033')
i.KERN.10033<-ToIgraph(KERN.10033)
plot.igraph(i.KERN.10033)
a.KERN.10033<-as_adjacency_matrix(i.KERN.10033,sparse=T)

KERN.10034<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10034')
i.KERN.10034<-ToIgraph(KERN.10034)
plot.igraph(i.KERN.10034)
a.KERN.10034<-as_adjacency_matrix(i.KERN.10034,sparse=T)

KERN.10035<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10035')
i.KERN.10035<-ToIgraph(KERN.10035)
plot.igraph(i.KERN.10035)
a.KERN.10035<-as_adjacency_matrix(i.KERN.10035,sparse=T)

KERN.10036<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10036')
i.KERN.10036<-ToIgraph(KERN.10036)
plot.igraph(i.KERN.10036)
a.KERN.10036<-as_adjacency_matrix(i.KERN.10036,sparse=T)

KERN.10037<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10037')
i.KERN.10037<-ToIgraph(KERN.10037)
plot.igraph(i.KERN.10037)
a.KERN.10037<-as_adjacency_matrix(i.KERN.10037,sparse=T)

KERN.10038<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10038')
i.KERN.10038<-ToIgraph(KERN.10038)
plot.igraph(i.KERN.10038)
a.KERN.10038<-as_adjacency_matrix(i.KERN.10038,sparse=T)

KERN.10039<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10039')
i.KERN.10039<-ToIgraph(KERN.10039)
plot.igraph(i.KERN.10039)
a.KERN.10039<-as_adjacency_matrix(i.KERN.10039,sparse=T)

KERN.10040<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10040')
i.KERN.10040<-ToIgraph(KERN.10040)
plot.igraph(i.KERN.10040)
a.KERN.10040<-as_adjacency_matrix(i.KERN.10040,sparse=T)

KERN.10041<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10041')
i.KERN.10041<-ToIgraph(KERN.10041)
plot.igraph(i.KERN.10041)
a.KERN.10041<-as_adjacency_matrix(i.KERN.10041,sparse=T)

KERN.10042<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10042')
i.KERN.10042<-ToIgraph(KERN.10042)
plot.igraph(i.KERN.10042)
a.KERN.10042<-as_adjacency_matrix(i.KERN.10042,sparse=T)

KERN.10044<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10044')
i.KERN.10044<-ToIgraph(KERN.10044)
plot.igraph(i.KERN.10044)
a.KERN.10044<-as_adjacency_matrix(i.KERN.10044,sparse=T)

KERN.10046<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10046')
i.KERN.10046<-ToIgraph(KERN.10046)
plot.igraph(i.KERN.10046)
a.KERN.10046<-as_adjacency_matrix(i.KERN.10046,sparse=T)

KERN.10047<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10047')
i.KERN.10047<-ToIgraph(KERN.10047)
plot.igraph(i.KERN.10047)
a.KERN.10047<-as_adjacency_matrix(i.KERN.10047,sparse=T)

KERN.10048<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10048')
i.KERN.10048<-ToIgraph(KERN.10048)
plot.igraph(i.KERN.10048)
a.KERN.10048<-as_adjacency_matrix(i.KERN.10048,sparse=T)

KERN.10049<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10049')
i.KERN.10049<-ToIgraph(KERN.10049)
plot.igraph(i.KERN.10049)
a.KERN.10049<-as_adjacency_matrix(i.KERN.10049,sparse=T)

KERN.10052<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10052')
i.KERN.10052<-ToIgraph(KERN.10052)
plot.igraph(i.KERN.10052)
a.KERN.10052<-as_adjacency_matrix(i.KERN.10052,sparse=T)

KERN.10053<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10053')
i.KERN.10053<-ToIgraph(KERN.10053)
plot.igraph(i.KERN.10053)
a.KERN.10053<-as_adjacency_matrix(i.KERN.10053,sparse=T)

KERN.10054<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10054')
i.KERN.10054<-ToIgraph(KERN.10054)
plot.igraph(i.KERN.10054)
a.KERN.10054<-as_adjacency_matrix(i.KERN.10054,sparse=T)

KERN.10055<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10055')
i.KERN.10055<-ToIgraph(KERN.10055)
plot.igraph(i.KERN.10055)
a.KERN.10055<-as_adjacency_matrix(i.KERN.10055,sparse=T)

KERN.10056<-LoadCommunity('~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/communities/10056')
i.KERN.10056<-ToIgraph(KERN.10056)
plot.igraph(i.KERN.10056)
a.KERN.10056<-as_adjacency_matrix(i.KERN.10056,sparse=T)



#Calcualte betas

############
N<-list(i.ROCK.RCLS1.1,i.ROCK.RCLS2.1,i.ROCK.RCLS2.2,i.ROCK.RCLS2.3,i.ROCK.RCLS2.4,i.ROCK.RCLS2.5,i.ROCK.RCLS2.6,i.ROCK.RCLS3.1,i.ROCK.RCLS3.2,i.ROCK.RCLS3.3,i.ROCK.RCLS3.4,i.ROCK.RCLS3.5,i.ROCK.RCLS4.1,i.ROCK.RCLS4.2,i.ROCK.RCLS4.3,i.ROCK.RCLS5.1,i.ROCK.RCLS5.2,i.ROCK.RCLS5.3,i.ROCK.RCLS6.1,i.ROCK.RCLS6.2,i.ROCK.RCLS6.3,i.ROCK.RCLS6.4,i.ROCK.RCLS6.5,i.ROCK.RCLS7.1,i.ROCK.RCLS7.2,i.ROCK.RCLS7.3,i.ROCK.RCLS7.4,i.ROCK.RCLS7.5,i.ROCK.RCLS7.6,i.ROCK.RCLS7.7,i.ROCK.RCLS8.1,i.ROCK.RCLS8.2,i.ROCK.RCLS8.3,i.ROCK.RCLS8.4,i.ROCK.RCLS8.5,i.ROCK.RCLS8.7,
        i.KERN.10029,i.KERN.10030,i.KERN.10031,i.KERN.10032,i.KERN.10033,i.KERN.10034,i.KERN.10035,i.KERN.10036,i.KERN.10037,i.KERN.10038,i.KERN.10039,i.KERN.10040,i.KERN.10041,i.KERN.10042,i.KERN.10044,i.KERN.10046,i.KERN.10047,i.KERN.10048,i.KERN.10049,i.KERN.10052,i.KERN.10053,i.KERN.10054,i.KERN.10055,i.KERN.10056,
        i.Cascade.CLS1_1,i.Cascade.CLS1_3,i.Cascade.CLS2_1,i.Cascade.CLS2_3,i.Cascade.CLS3_1,i.Cascade.CLS3_2,i.Cascade.CLS3_3,i.Cascade.CLS4_1,i.Cascade.CLS4_2,i.Cascade.CLS4_3,i.Cascade.CLS5_1,
        i.EVO.ELS2_1,i.EVO.ELS2_2,i.EVO.ELS2_3,i.EVO.ELS3_1,i.EVO.ELS3_2,i.EVO.ELS3_3,i.EVO.ELS4_1,i.EVO.ELS4_2,i.EVO.ELS4_3,i.EVO.ELS5_1,i.EVO.ELS5_2,i.EVO.ELS5_3,i.EVO.ELS6_1,i.EVO.ELS6_2,i.EVO.ELS6_3,i.EVO.ELS7_1,i.EVO.ELS7_2,i.EVO.ELS7_3,i.EVO.ELS8_1,i.EVO.ELS8_2,i.EVO.ELS8_3,
        i.BUBBS.Outlet.10477.trt.2003,i.BUBBS.Outlet.10477.trt.2004,i.BUBBS.Outlet.10477.trt.2011,i.BUBBS.Outlet.10487.trt.2003,i.BUBBS.Outlet.10487.trt.2004,i.BUBBS.Outlet.10487.trt.2011,i.Bubbs.Outlet.10494.trt.2012,i.BUBBS.Outlet.11007.fishless.2003,i.BUBBS.Outlet.11007.fishless.2004,i.BUBBS.Outlet.11007.fishless.2011,i.Bubbs.Outlet.Vidette.below.2003,i.Bubbs.Outlet.Vidette.below.2004,i.Bubbs.Outlet.Vidette.below.2011,i.Bubbs.Outlet.Vidette.below.2012,
        i.RAE.RLS1_3,i.RAE.RLS2_1,i.RAE.RLS3_1,i.RAE.RLS3_2,i.RAE.RLS4_1,i.RAE.RLS4_2,i.RAE.RLS4_3,i.RAE.RLS5_1,i.RAE.RLS5_2,i.RAE.RLS5_3,i.RAE.RLS6_1,i.RAE.RLS6_2)

names(N)<-c("RCLS_2_1","RCLS_2_2","RCLS_2_3","RCLS_2_4","RCLS_2_5","RCLS_2_6","RCLS_3_1",
            "RCLS_3_2","RCLS_3_3","RCLS_3_4","RCLS_3_5","RCLS_4_1","RCLS_4_2","RCLS_4_3","RCLS_5_1","RCLS_5_2",
            "RCLS_5_3","RCLS_6_1","RCLS_6_2","RCLS_6_3","RCLS_6_4","RCLS_6_5","RCLS_7_1","RCLS_7_2","RCLS_7_3","RCLS_7_4",
            "RCLS_7_5","RCLS_7_6","RCLS_7_7","RCLS_8_1","RCLS_8_2","RCLS_8_3","RCLS_8_4","RCLS_8_5","RCLS_8_7","10029",	"10030"	,"10031"	,"10032",	"10033",	"10034"	,"10035"	,"10036"	,"10037",	"10038",	"10039"	,"10040"	,"10041",	"10042",	"10044",	"10046"	,"10047"	,"10048",	"10049"	,"10052",	"10053",	"10054",	"10055",	"10056",
            "CLS1_1",	"CLS1_3",	"CLS2_1",	"CLS2_3",	"CLS3_1",	"CLS3_2",	"CLS3_3",	"CLS4_1",	"CLS4_2",	"CLS4_3",	"CLS5_1",
            "ELS2_1","ELS2_2","ELS2_3","ELS3_1","ELS3_2","ELS3_3","ELS4_1","ELS4_2","ELS4_3","ELS5_1","ELS5_2","ELS5_3","ELS6_1","ELS6_2","ELS6_3","ELS7_1","ELS7_2","ELS7_3","ELS8_1","ELS8_2","ELS8_3",
            "Outlet.10477.trt.2003",	"Outlet.10477.trt.2004",	"Outlet.10477.trt.2011",	"Outlet.10487.trt.2003",	"Outlet.10487.trt.2004",	"Outlet.10487.trt.2011",	"Outlet.10494.trt.2012",	"Outlet.11007.fishless.2003",	"Outlet.11007.fishless.2004",	"Outlet.11007.fishless.2011",	"Outlet.Vidette.below.2003",	"Outlet.Vidette.below.2004",	"Outlet.Vidette.below.2011",	"Outlet.Vidette.below.2012",
            "RLS1_3","RLS2_1",	"RLS3_1",	"RLS3_2",	"RLS4_1",	"RLS4_2",	"RLS4_3",	"RLS5_1",	"RLS5_2",	"RLS5_3",	"RLS6_1",	"RLS6_2")


N.rock<-list(i.ROCK.RCLS1.1,i.ROCK.RCLS2.1,i.ROCK.RCLS2.2,i.ROCK.RCLS2.3,i.ROCK.RCLS2.4,i.ROCK.RCLS2.5,i.ROCK.RCLS2.6,i.ROCK.RCLS3.1,i.ROCK.RCLS3.2,i.ROCK.RCLS3.3,i.ROCK.RCLS3.4,i.ROCK.RCLS3.5,i.ROCK.RCLS4.1,i.ROCK.RCLS4.2,i.ROCK.RCLS4.3,i.ROCK.RCLS5.1,i.ROCK.RCLS5.2,i.ROCK.RCLS5.3,i.ROCK.RCLS6.1,i.ROCK.RCLS6.2,i.ROCK.RCLS6.3,i.ROCK.RCLS6.4,i.ROCK.RCLS6.5,i.ROCK.RCLS7.1,i.ROCK.RCLS7.2,i.ROCK.RCLS7.3,i.ROCK.RCLS7.4,i.ROCK.RCLS7.5,i.ROCK.RCLS7.6,i.ROCK.RCLS7.7,i.ROCK.RCLS8.1,i.ROCK.RCLS8.2,i.ROCK.RCLS8.3,i.ROCK.RCLS8.4,i.ROCK.RCLS8.5,i.ROCK.RCLS8.7)
names(N.rock)<-c("RCLS_2_1","RCLS_2_2","RCLS_2_3","RCLS_2_4","RCLS_2_5","RCLS_2_6","RCLS_3_1","RCLS_3_2","RCLS_3_3","RCLS_3_4","RCLS_3_5","RCLS_4_1","RCLS_4_2","RCLS_4_3","RCLS_5_1","RCLS_5_2",
                 "RCLS_5_3","RCLS_6_1","RCLS_6_2","RCLS_6_3","RCLS_6_4","RCLS_6_5","RCLS_7_1","RCLS_7_2","RCLS_7_3","RCLS_7_4","RCLS_7_5","RCLS_7_6","RCLS_7_7","RCLS_8_1","RCLS_8_2","RCLS_8_3","RCLS_8_4","RCLS_8_5","RCLS_8_7")

N.kern<-list(i.KERN.10029,i.KERN.10030,i.KERN.10031,i.KERN.10032,i.KERN.10033,i.KERN.10034,i.KERN.10035,i.KERN.10036,i.KERN.10037,i.KERN.10038,i.KERN.10039,i.KERN.10040,i.KERN.10041,i.KERN.10042,i.KERN.10044,i.KERN.10046,i.KERN.10047,i.KERN.10048,i.KERN.10049,i.KERN.10052,i.KERN.10053,i.KERN.10054,i.KERN.10055,i.KERN.10056)
names(N.kern)<-c("10029",	"10030"	,"10031"	,"10032",	"10033",	"10034"	,"10035"	,"10036"	,"10037",	"10038",	"10039"	,"10040"	,"10041",	"10042",	"10044",	"10046"	,"10047"	,"10048",	"10049"	,"10052",	"10053",	"10054",	"10055",	"10056")

N.casc<-list(i.Cascade.CLS1_1,i.Cascade.CLS1_3,i.Cascade.CLS2_1,i.Cascade.CLS2_3,i.Cascade.CLS3_1,i.Cascade.CLS3_2,i.Cascade.CLS3_3,i.Cascade.CLS4_1,i.Cascade.CLS4_2,i.Cascade.CLS4_3,i.Cascade.CLS5_1)
names(N.casc)<-c("CLS1_1",	"CLS1_3",	"CLS2_1",	"CLS2_3",	"CLS3_1",	"CLS3_2",	"CLS3_3",	"CLS4_1",	"CLS4_2",	"CLS4_3",	"CLS5_1")

N.evo<-list(i.EVO.ELS2_1,i.EVO.ELS2_2,i.EVO.ELS2_3,i.EVO.ELS3_1,i.EVO.ELS3_2,i.EVO.ELS3_3,i.EVO.ELS4_1,i.EVO.ELS4_2,i.EVO.ELS4_3,i.EVO.ELS5_1,i.EVO.ELS5_2,i.EVO.ELS5_3,i.EVO.ELS6_1,i.EVO.ELS6_2,i.EVO.ELS6_3,i.EVO.ELS7_1,i.EVO.ELS7_2,i.EVO.ELS7_3,i.EVO.ELS8_1,i.EVO.ELS8_2,i.EVO.ELS8_3)
names(N.evo)<-c("ELS2_1","ELS2_2","ELS2_3","ELS3_1","ELS3_2","ELS3_3","ELS4_1","ELS4_2","ELS4_3","ELS5_1","ELS5_2","ELS5_3","ELS6_1","ELS6_2","ELS6_3","ELS7_1","ELS7_2","ELS7_3","ELS8_1","ELS8_2","ELS8_3")

N.bubbs<-list(i.BUBBS.Outlet.10477.trt.2003,i.BUBBS.Outlet.10477.trt.2004,i.BUBBS.Outlet.10477.trt.2011,i.BUBBS.Outlet.10487.trt.2003,i.BUBBS.Outlet.10487.trt.2004,i.BUBBS.Outlet.10487.trt.2011,i.Bubbs.Outlet.10494.trt.2012,i.BUBBS.Outlet.11007.fishless.2003,i.BUBBS.Outlet.11007.fishless.2004,i.BUBBS.Outlet.11007.fishless.2011,i.Bubbs.Outlet.Vidette.below.2003,i.Bubbs.Outlet.Vidette.below.2004,i.Bubbs.Outlet.Vidette.below.2011,i.Bubbs.Outlet.Vidette.below.2012)
names(N.bubbs)<-c( "Outlet.10477.trt.2003",	"Outlet.10477.trt.2004",	"Outlet.10477.trt.2011",	"Outlet.10487.trt.2003",	"Outlet.10487.trt.2004",	"Outlet.10487.trt.2011",	"Outlet.10494.trt.2012",	"Outlet.11007.fishless.2003",	"Outlet.11007.fishless.2004",	"Outlet.11007.fishless.2011",	"Outlet.Vidette.below.2003",	"Outlet.Vidette.below.2004",	"Outlet.Vidette.below.2011",	"Outlet.Vidette.below.2012")

N.rae<-list(i.RAE.RLS1_3,i.RAE.RLS2_1,i.RAE.RLS3_1,i.RAE.RLS3_2,i.RAE.RLS4_1,i.RAE.RLS4_2,i.RAE.RLS4_3,i.RAE.RLS5_1,i.RAE.RLS5_2,i.RAE.RLS5_3,i.RAE.RLS6_1,i.RAE.RLS6_2)
names(N.rae)<-c("RLS1_3","RLS2_1",	"RLS3_1",	"RLS3_2",	"RLS4_1",	"RLS4_2",	"RLS4_3",	"RLS5_1",	"RLS5_2",	"RLS5_3",	"RLS6_1",	"RLS6_2")


#Calc beta each ent
rock.beta <- network_betadiversity(N.rock)
kern.beta <- network_betadiversity(N.kern)
casc.beta <- network_betadiversity(N.casc)
evo.beta <- network_betadiversity(N.evo)
bubbs.beta <- network_betadiversity(N.bubbs)
rae.beta <- network_betadiversity(N.rae)


#Geo.dist all sites
evoenv<-env%>%dplyr::filter(O.NET=="ROCK")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.rock <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.rock <- as.dist(GeoDist.rock)

evoenv<-env%>%dplyr::filter(O.NET=="KERN")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.kern <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.kern <- as.dist(GeoDist.kern)

evoenv<-env%>%dplyr::filter(Network=="CASCADE")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.casc <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.casc <- as.dist(GeoDist.casc)

evoenv<-env%>%dplyr::filter(Network=="EVO")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.evo <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.evo <- as.dist(GeoDist.evo)

evoenv<-env%>%dplyr::filter(Network=="BUBBS")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.bubbs <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.bubbs <- as.dist(GeoDist.bubbs)

evoenv<-env%>%dplyr::filter(Network=="RAE")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.rae <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.rae <- as.dist(GeoDist.rae)


###Put it togerther

rock.beta$GEO<-GeoDist.rock
kern.beta$GEO<-GeoDist.kern
casc.beta$GEO<-GeoDist.casc
evo.beta$GEO<-GeoDist.evo
bubbs.beta$GEO<-GeoDist.bubbs
rae.beta$GEO<-GeoDist.rae

rock.beta$Network<-c("ROCK")
kern.beta$Network<-c("KERN")
casc.beta$Network<-c("CASCADE")
evo.beta$Network<-c("EVO")
bubbs.beta$Network<-c("BUBBS")
rae.beta$Network<-c("RAE")


alls<-rbind(rock.beta,kern.beta,casc.beta,evo.beta,bubbs.beta,rae.beta)
alls<-alls%>%mutate(ST.WN=ST/WN)

alls%>%ggplot( aes(x=ST,y=S,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed", size=1.5)+ylim(0,1)+xlim(0,1)+
  facet_grid(~Network)+
  theme_bw()

#Dissim in species comp
alls%>%ggplot( aes(x=GEO,y=S,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()
#Dissimilarity of interactions due to species turnover
alls%>%ggplot( aes(x=GEO,y=ST,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()

#Dissimialirty of all interctions
alls%>%ggplot( aes(x=GEO,y=WN,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()


#Dissimilarity of interactions established between species common to both realisations

alls%>%ggplot( aes(x=GEO,y=OS,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()

#Contribution of species dissimilarity to network dissimilarity

alls%>%ggplot( aes(x=GEO,y=ST.WN,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()





evoenv<-env%>%dplyr::filter(Network!="YOUNG")%>%dplyr::select(Site,Elevation)%>%column_to_rownames(var="Site")

net.dist <- dist(as.matrix(evoenv, latlon=TRUE))
#colnames(net.dist) <- rownames(GeoDist) <- rownames(evoenv)
GeoDist <- as.dist(net.dist)


evoenv<-env%>%dplyr::filter(Network!="YOUNG")%>%dplyr::select(Site,River.dist.lake)%>%column_to_rownames(var="Site")

river.dist <- dist(as.matrix(evoenv))
colnames(river.dist) <- rownames(GeoDist) <- rownames(evoenv)
GeoDist <- as.dist(net.dist)

evoenv<-env%>%dplyr::filter(Network!="YOUNG")%>%dplyr::select(Site,SHRUB_SCRUB,Up.Lake.area,Head.river.dist,River.dist.lake,Elevation)%>%column_to_rownames(var="Site")

env.dist<-dist(evoenv)
metaweb<-metaweb(N)
net.beta <- network_betadiversity(N)
net.beta$GEO<-GeoDist
net.beta$netdist<-net.dist
net.beta$env<-env.dist
net.beta$riverdist<-river.dist


evoenv<-env%>%dplyr::filter(Network!="YOUNG")
net.beta$Network<-evo$env$Network

ggplot(net.beta, aes(x=ST,y=S))+geom_point()+geom_smooth(method = "lm")+geom_abline(intercept = 0, slope = 1, color="red", 
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



