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


############

N<-list(i.ROCK.RCLS1.1,i.ROCK.RCLS2.1,i.ROCK.RCLS2.2,i.ROCK.RCLS2.3,i.ROCK.RCLS2.4,i.ROCK.RCLS2.5,i.ROCK.RCLS2.6,i.ROCK.RCLS3.1,i.ROCK.RCLS3.2,i.ROCK.RCLS3.3,i.ROCK.RCLS3.4,i.ROCK.RCLS3.5,i.ROCK.RCLS4.1,i.ROCK.RCLS4.2,i.ROCK.RCLS4.3,i.ROCK.RCLS5.1,i.ROCK.RCLS5.2,i.ROCK.RCLS5.3,i.ROCK.RCLS6.1,i.ROCK.RCLS6.2,i.ROCK.RCLS6.3,i.ROCK.RCLS6.4,i.ROCK.RCLS6.5,i.ROCK.RCLS7.1,i.ROCK.RCLS7.2,i.ROCK.RCLS7.3,i.ROCK.RCLS7.4,i.ROCK.RCLS7.5,i.ROCK.RCLS7.6,i.ROCK.RCLS7.7,i.ROCK.RCLS8.1,i.ROCK.RCLS8.2,i.ROCK.RCLS8.3,i.ROCK.RCLS8.4,i.ROCK.RCLS8.5,i.ROCK.RCLS8.7)

names(N)<-c("RCLS_2_1","RCLS_2_2","RCLS_2_3","RCLS_2_4","RCLS_2_5","RCLS_2_6","RCLS_3_1",
            "RCLS_3_2","RCLS_3_3","RCLS_3_4","RCLS_3_5","RCLS_4_1","RCLS_4_2","RCLS_4_3","RCLS_5_1","RCLS_5_2",
            "RCLS_5_3","RCLS_6_1","RCLS_6_2","RCLS_6_3","RCLS_6_4","RCLS_6_5","RCLS_7_1","RCLS_7_2","RCLS_7_3","RCLS_7_4",
            "RCLS_7_5","RCLS_7_6","RCLS_7_7","RCLS_8_1","RCLS_8_2","RCLS_8_3","RCLS_8_4","RCLS_8_5","RCLS_8_7")

evoenv<-env%>%dplyr::filter(O.NET=="ROCK")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")


GeoDist <- spDists(as.matrix(evoenv, latlon=TRUE))
colnames(GeoDist) <- rownames(GeoDist) <- rownames(evoenv)
GeoDist <- as.dist(GeoDist)

evoenv<-env%>%dplyr::filter(O.NET=="ROCK")%>%dplyr::select(Site,Elevation)%>%column_to_rownames(var="Site")

net.dist <- dist(as.matrix(evoenv, latlon=TRUE))
colnames(net.dist) <-  rownames(GeoDist)
GeoDist <- as.dist(net.dist)


evoenv<-env%>%dplyr::filter(O.NET=="ROCK")%>%dplyr::select(Site,Head.river.dist)%>%column_to_rownames(var="Site")

river.dist <- dist(as.matrix(evoenv))
colnames(river.dist) <- rownames(GeoDist) 
GeoDist <- as.dist(net.dist)
evoenv<-env%>%dplyr::filter(O.NET=="ROCK")%>%dplyr::select(Site,SHRUB_SCRUB,Up.Lake.area,Head.river.dist,River.dist.lake,Elevation)%>%column_to_rownames(var="Site")

env.dist<-dist(evoenv)
metaweb<-metaweb(N)
net.beta <- network_betadiversity(N)
net.beta$GEO<-GeoDist
net.beta$netdist<-net.dist
net.beta$env<-env.dist
net.beta$riverdist<-river.dist

pairs(net.betas[,7:10])

net.betas<-net.beta%>%
  separate("i", sep="_" ,into=c("net1", "stream1", "position1"))%>%
  separate("j", sep="_" ,into=c("net2", "stream2", "position2"))

net.betas$Varaition<-if_else(net.betas$stream1== "8" & net.betas$stream2== "8", 
                             "within",if_else(net.betas$stream1== "7" & net.betas$stream2== "7", 
                                              "within",if_else(net.betas$stream1== "6" & net.betas$stream2== "6",
                                                               "within",if_else(net.betas$stream1== "5" & net.betas$stream2== "5",
                                                                                "within",if_else(net.betas$stream1== "4" & net.betas$stream2== "4",
                                                                                                 "within",if_else(net.betas$stream1== "3" & net.betas$stream2== "3",
                                                                                                                  "within",if_else(net.betas$stream1== "2" & net.betas$stream2== "2",
                                                                                                                                   "within", "among")))))))


net.betas<-net.betas%>%
  mutate(BST_WN=ST/WN)

net.betas%>%
  dplyr::filter(Varaition != "NA")%>%
  ggplot(aes(x=log(GEO)+1,y=ST,colour=Varaition))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~Varaition)+
  scale_color_viridis(discrete = TRUE)+
  labs(x="Spatial Distance",y="Dissimilarity of interactions due to species turnover")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

net.betas%>%
  dplyr::filter(Varaition != "NA")%>%
  dplyr::filter(WN < 1)%>%
  ggplot(aes(x=S,y=WN,colour=GEO))+
  geom_point()+
  #geom_smooth(method = "lm")+
  scale_color_viridis(discrete = F)+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  labs(x="Dissimilarity of species composition",y="Dissimilarity of species interactions")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

net.betas%>%
  dplyr::filter(Varaition != "NA")%>%
  dplyr::filter(OS < 1)%>%
  ggplot(aes(x=S,y=OS,colour=GEO))+
  geom_point()+
  #geom_smooth(method = "lm")+
  scale_color_viridis(discrete = F)+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  labs(x="Dissimilarity of species composition",y="Dissimilarity of species interactions")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())


dog<-lm(S~GEO*Varaition,data=net.betas)
summary(dog)

ggplot(net.betas, aes(x=as.factor(stream2),y=S,fill=Varaition))+geom_boxplot()
ggplot(net.betas, aes(x=as.factor(stream2),y=ST,fill=Varaition))+geom_boxplot()
ggplot(net.betas, aes(x=as.factor(stream2),y=OS,fill=Varaition))+geom_boxplot()
ggplot(net.betas, aes(x=as.factor(stream2),y=WN,fill=Varaition))+geom_boxplot()

net.betas%>%
  filter(stream1=='2' & stream2!="2")%>%
  ggplot(aes(x=S,y=ST))+
  geom_point()+geom_smooth(method = "lm")+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+ylim(0,1)+xlim(0,1)


ggplot(net.beta, aes(x=S,y=ST, colour=netdist))+geom_point()+geom_smooth(method = "lm")+geom_abline(intercept = 0, slope = 1, color="red", 
                                                                                                    linetype="dashed", size=1.5)+ylim(0,1)+xlim(0,1)
ggplot(net.beta, aes(x=S,y=OS))+geom_point()+geom_smooth(method = "lm")+geom_abline(intercept = 0, slope = 1, color="red", 
                                                                                    linetype="dashed", size=1.5)
ggplot(net.beta, aes(x=S,y=WN))+geom_point()+geom_smooth(method = "lm")+geom_abline(intercept = 0, slope = 1, color="red", 
                                                                                    linetype="dashed", size=1.5)
ggplot(net.betas, aes(x=GEO,y=S,colour=Varaition))+
  geom_point()+geom_smooth(method = "lm")+facet_grid(~Varaition)

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