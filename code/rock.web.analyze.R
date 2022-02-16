library(cowplot)
library(viridis)
library(ggplot2)
library(reshape2)
library(metR)
library(igraph)
library(ggbiplot)
library(cheddar)
library(tidyverse)


#rock.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Rock/New_res_fam_con_genus/"
rock.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/New_family/"
#rock.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Rock/Family/"
Rock.web<-LoadCollection(rock.dir)

Rock.data<-CollectionCPS(Rock.web, 
                         c('title',
                           S='NumberOfNodes',
                           L='NumberOfTrophicLinks',
                           L.S='LinkageDensity',
                           C='DirectedConnectance',
                           B='FractionBasalNodes',
                           I='FractionIntermediateNodes',
                           T='FractionTopLevelNodes',
                           N='FractionNonTopLevelNodes',
                           Isolated='FractionIsolatedNodes',
                           Can='FractionCannibalistic',
                           Omn='FractionOmnivorous',
                           Sim.mean= 'MeanMaximumTrophicSimilarity',
                           SumDietGaps='SumDietGaps',
                           SumConsumerGaps='SumConsumerGaps',
                           #NormalisedTrophicGenerality='NormalisedTrophicGenerality',
                           #Ratio='Ratio resources:consumers',
                           #Consumers='NumberOfConsumers',
                           #Resources='NumberOfResources',
                           #TrophicSimilarity='TrophicSimilarity',
                           #ShortestPaths='ShortestPaths',
                           #Flow='FlowBasedTrophicLevel',
                           #Trophic.Height='TrophicHeight',
                           #Chain.mean='ChainAveragedTrophicLevel',
                           #Prey.mean='PreyAveragedTrophicLevel',
                           #Top.nodes='TopLevelNodes',
                           #Max.trophic.posit= 'MeanMaximumTrophicLevel',
                           #Sim.max='MaximumTrophicSimilarity',
                           #Vul='NormalisedTrophicVulnerability',
                           #Gen='NormalisedTrophicGenerality',
                           Path='CharacteristicPathLength'
                           #nT <- 'TopLevelNodes'
                           #nI <- length(IntermediateNodes(community)),
                           #nB <- length(BasalNodes(community)),
                           #B='SumBiomassByClass'
                           #V='Vulnerability'
                           #PATL='PreyAveragedTrophicLevRCLS',
                           #CATL='ChainAveragedTrophicLevRCLS',
                           #DD='DegreeDistribution'
                           ))

all.webs<-( Rock.data)
all.webs<-all.webs%>%tibble::rownames_to_column()%>%rename(Site = title) 

envs <-read.csv("Data/dave.matt.env.full.12.29.19.csv")

env<-envs%>%filter(O.NET=="ROCK")


env<-env%>%mutate(Euc.dist.lake=log(1+Euc.dist.lake),River.dist.lake=log(1+River.dist.lake),Head.river.dist=log(1+Head.river.dist))
  #mutate(Size.net.dist=Head.river.dist*Up.Lake.area,Size.river.dist=River.dist.lake*Up.Lake.area,RCLSev.dist=River.dist.lake/RCLSevation)
env.webs<-dplyr::left_join(env,all.webs, by="Site")#%>%filter(Head.river.dist>3.5)

#PCA
spatials<-env.webs%>%dplyr::select(c(Head.river.dist, River.dist.lake, Up.Lake.area, Elevation))
envs<-env.webs%>%dplyr::select(c(Temp,Chlorophyll.mean,Conductivity,DO,pH,Discharge.Mean,SHRUB_SCRUB))

Epca = prcomp(envs, scale.=TRUE)
biplot(Epca)
summary(Epca)
Epca$rotation
ggbiplot(Epca)
ggbiplot(Epca, labels=rownames(all$O.NET), groups=interaction(all$O.NET), ellipse=TRUE)

Epca$x
env_pc_scores <- data.frame(Epca$x[,1:6])
colnames(env_pc_scores)<-c("E_PC1","E_PC2","E_PC3","E_PC4","E_PC5","E_PC6")
rownames(env_pc_scores)<-env.webs$Site

#Space
Epca = prcomp(spatials, scale.=TRUE)
biplot(Epca)
summary(Epca)
Epca$rotation


Epca$x
spatial_pc_scores <- data.frame(Epca$x[,1:4])
colnames(spatial_pc_scores)<-c("S_PC1","S_PC2","S_PC3","S_PC4")
rownames(spatial_pc_scores)<-env.webs$Site

################
spatial_pc_scores<-spatial_pc_scores%>%rownames_to_column("Site")
env_pc_scores<-env_pc_scores%>%rownames_to_column("Site")
#env.webs<-env.webs%>%rownames_to_column("Site")

env.webz<-left_join(env.webs,spatial_pc_scores, by="Site")
env.webzz<-left_join(env.webz,env_pc_scores, by="Site")
################################################################################################################################

env.webzz%>%
  filter(Head.river.dist>3.5)%>%
  gather(S,L,L.S,C,Omn,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = E_PC1, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme_bw()

env.webs%>%
  filter(Head.river.dist>3.5)%>%
  gather(S,L,L.S,C,B,I,Sim.mean,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = Head.river.dist, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme_bw()

env.webs%>%
  filter(Head.river.dist>3.5)%>%
  gather(S,L,L.S,C,B,I,Isolated,Can,Omn,Sim.mean,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(Fish), y = value, fill=as.factor(Fish))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  facet_wrap(~var, scales = "free") +
  theme_bw()

env.webzz%>%
  filter(Head.river.dist>3.5)%>%
  gather(E_PC1,E_PC2,E_PC3,E_PC4,S_PC1,S_PC2,S_PC3,Chlorophyll.mean, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = C)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme_bw()

varlist<-names(env.webs)[c(28:33,36:40)]

modRCLS <- lapply(varlist, function(y) {
  lm(substitute(i~$Elevation, list(i = as.name(y))), data = env.webs)
})
lapply(modRCLS, summary)

models <- lapply(varlist, function(y) {
  lm(substitute( i ~ env.webs$Head.river.dist, list(i = as.name(y))), data = env.webs)
})

lapply(models, summary)
## look at the first element of the list, model 1
models[[1]]
#Heat map of both head and River dist metrics
#Beta
lm.mod <- lm(C ~ S_PC1*E_PC3, data = env.webzz)
summary(lm.mod)

range(env.webs$Head.river.dist)
lake<-specie%>%drop_na()
range(env.webs$River.dist.lake)

prepplot1 <- as.data.frame(matrix(ncol = 3, nrow = 10000))
colnames(prepplot1) <- c("River.dist.lake", "Head.river.dist", "est.C")

prepplot1$Head.river.dist <- rep(seq(6.761405 ,10.063097, length.out = 100), 100)
prepplot1 <- prepplot1[order(prepplot1$Head.river.dist),]
prepplot1$River.dist.lake <- rep(seq(0.7409909, 9.6235643, length.out = 100), 100)
#prepplot1$est.C <- -4.99626     +0.21383 *prepplot1$Head.river.dist +0.75407 *prepplot1$River.dist.lake + 
#  -0.09176 *prepplot1$Head.river.dist*prepplot1$River.dist.lake
prepplot1$est.C<-predict(lm.mod,prepplot1, type="response")

st.bd<-ggplot(prepplot1) +
  geom_tile(aes(River.dist.lake, Head.river.dist, fill = est.C))+
  #ggtitle("e)") +
  scale_fill_viridis_d()+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))+
  ggplot2::xlab("Log Distance from Upstream Lakes (m)") + ylab("Log Distance from Headwaters (m)") 
  #ggplot2::labs(fill = "C")


lm.mod <- lm(B ~ Head.river.dist, data = env.webs)
summary(lm.mod)


##### Lavaan modRCLSing
library(lavaan)
library(semPlot)

smod1 = '
         C~Head.river.dist+ River.dist.lake +Chlorophyll.mean'

#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=env.webs)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.labRCLS.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)


#BETA


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

#ROCK
i.ROCK.RCLS1.1<-ToIgraph(ROCK.RCLS1.1)
plot.igraph(i.ROCK.RCLS1.1)
a.ROCK.RCLS1.1<-as_adjacency_matrix(i.ROCK.RCLS1.1,sparse=T)

i.ROCK.RCLS2.1<-ToIgraph(ROCK.RCLS2.1)
plot.igraph(i.ROCK.RCLS2.1)
a.ROCK.RCLS2.1<-as_adjacency_matrix(i.ROCK.RCLS2.1,sparse=T)

i.ROCK.RCLS2.2<-ToIgraph(ROCK.RCLS2.2)
plot.igraph(i.ROCK.RCLS2.2)
a.ROCK.RCLS2.2<-as_adjacency_matrix(i.ROCK.RCLS2.2,sparse=T)

i.ROCK.RCLS2.3<-ToIgraph(ROCK.RCLS2.3)
plot.igraph(i.ROCK.RCLS2.3)
a.ROCK.RCLS2.3<-as_adjacency_matrix(i.ROCK.RCLS2.3,sparse=T)

i.ROCK.RCLS2.4<-ToIgraph(ROCK.RCLS2.4)
plot.igraph(i.ROCK.RCLS2.4)
a.ROCK.RCLS2.4<-as_adjacency_matrix(i.ROCK.RCLS2.4,sparse=T)

i.ROCK.RCLS2.5<-ToIgraph(ROCK.RCLS2.5)
plot.igraph(i.ROCK.RCLS2.5)
a.ROCK.RCLS2.5<-as_adjacency_matrix(i.ROCK.RCLS2.5,sparse=T)

i.ROCK.RCLS2.6<-ToIgraph(ROCK.RCLS2.6)
plot.igraph(i.ROCK.RCLS2.6)
a.ROCK.RCLS2.6<-as_adjacency_matrix(i.ROCK.RCLS2.6,sparse=T)

i.ROCK.RCLS3.1<-ToIgraph(ROCK.RCLS3.1)
plot.igraph(i.ROCK.RCLS3.1)
a.ROCK.RCLS3.1<-as_adjacency_matrix(i.ROCK.RCLS3.1,sparse=T)

i.ROCK.RCLS3.2<-ToIgraph(ROCK.RCLS3.2)
plot.igraph(i.ROCK.RCLS3.2)
a.ROCK.RCLS3.2<-as_adjacency_matrix(i.ROCK.RCLS3.2,sparse=T)

i.ROCK.RCLS3.3<-ToIgraph(ROCK.RCLS3.3)
plot.igraph(i.ROCK.RCLS3.3)
a.ROCK.RCLS3.3<-as_adjacency_matrix(i.ROCK.RCLS3.3,sparse=T)

i.ROCK.RCLS3.4<-ToIgraph(ROCK.RCLS3.4)
plot.igraph(i.ROCK.RCLS3.4)
a.ROCK.RCLS3.4<-as_adjacency_matrix(i.ROCK.RCLS3.4,sparse=T)

i.ROCK.RCLS3.5<-ToIgraph(ROCK.RCLS3.5)
plot.igraph(i.ROCK.RCLS3.5)
a.ROCK.RCLS3.5<-as_adjacency_matrix(i.ROCK.RCLS3.5,sparse=T)

i.ROCK.RCLS4.1<-ToIgraph(ROCK.RCLS4.1)
plot.igraph(i.ROCK.RCLS4.1)
a.ROCK.RCLS4.1<-as_adjacency_matrix(i.ROCK.RCLS4.1,sparse=T,type="both")
aa.ROCK.RCLS4.1<-as.matrix(a.ROCK.RCLS4.1)

i.ROCK.RCLS4.2<-ToIgraph(ROCK.RCLS4.2)
plot.igraph(i.ROCK.RCLS4.2)
a.ROCK.RCLS4.2<-as_adjacency_matrix(i.ROCK.RCLS4.2,sparse=T)
aa.ROCK.RCLS4.2<-as.matrix(a.ROCK.RCLS4.2)

i.ROCK.RCLS4.3<-ToIgraph(ROCK.RCLS4.3)
plot.igraph(i.ROCK.RCLS4.3)
a.ROCK.RCLS4.3<-as_adjacency_matrix(i.ROCK.RCLS4.3,sparse=T)
aa.ROCK.RCLS4.3<-as.matrix(a.ROCK.RCLS4.3)

i.ROCK.RCLS5.1<-ToIgraph(ROCK.RCLS5.1)
plot.igraph(i.ROCK.RCLS5.1)
a.ROCK.RCLS5.1<-as_adjacency_matrix(i.ROCK.RCLS5.1,sparse=T)

i.ROCK.RCLS5.2<-ToIgraph(ROCK.RCLS5.2)
plot.igraph(i.ROCK.RCLS5.2)
a.ROCK.RCLS5.2<-as_adjacency_matrix(i.ROCK.RCLS5.2,sparse=T)

i.ROCK.RCLS5.3<-ToIgraph(ROCK.RCLS5.3)
plot.igraph(i.ROCK.RCLS5.3)
a.ROCK.RCLS5.3<-as_adjacency_matrix(i.ROCK.RCLS5.3,sparse=T)


i.ROCK.RCLS6.1<-ToIgraph(ROCK.RCLS6.1)
plot.igraph(i.ROCK.RCLS6.1)
a.ROCK.RCLS6.1<-as_adjacency_matrix(i.ROCK.RCLS6.1,sparse=T)

i.ROCK.RCLS6.2<-ToIgraph(ROCK.RCLS6.2)
plot.igraph(i.ROCK.RCLS6.2)
a.ROCK.RCLS6.2<-as_adjacency_matrix(i.ROCK.RCLS6.2,sparse=T)

i.ROCK.RCLS6.3<-ToIgraph(ROCK.RCLS6.3)
plot.igraph(i.ROCK.RCLS6.3)
a.ROCK.RCLS6.3<-as_adjacency_matrix(i.ROCK.RCLS6.3,sparse=T)


i.ROCK.RCLS6.4<-ToIgraph(ROCK.RCLS6.4)
plot.igraph(i.ROCK.RCLS6.4)
a.ROCK.RCLS6.4<-as_adjacency_matrix(i.ROCK.RCLS6.4,sparse=T)

i.ROCK.RCLS6.5<-ToIgraph(ROCK.RCLS6.5)
plot.igraph(i.ROCK.RCLS6.5)
a.ROCK.RCLS6.5<-as_adjacency_matrix(i.ROCK.RCLS6.5,sparse=T)

i.ROCK.RCLS7.1<-ToIgraph(ROCK.RCLS7.1)
plot.igraph(i.ROCK.RCLS7.1)
a.ROCK.RCLS7.1<-as_adjacency_matrix(i.ROCK.RCLS7.1,sparse=T)

i.ROCK.RCLS7.2<-ToIgraph(ROCK.RCLS7.2)
plot.igraph(i.ROCK.RCLS7.2)
a.ROCK.RCLS7.2<-as_adjacency_matrix(i.ROCK.RCLS7.2,sparse=T)

i.ROCK.RCLS7.3<-ToIgraph(ROCK.RCLS7.3)
plot.igraph(i.ROCK.RCLS7.3)
a.ROCK.RCLS7.3<-as_adjacency_matrix(i.ROCK.RCLS7.3,sparse=T)

i.ROCK.RCLS7.4<-ToIgraph(ROCK.RCLS7.4)
plot.igraph(i.ROCK.RCLS7.4)
a.ROCK.RCLS7.4<-as_adjacency_matrix(i.ROCK.RCLS7.4,sparse=T)

i.ROCK.RCLS7.5<-ToIgraph(ROCK.RCLS7.5)
plot.igraph(i.ROCK.RCLS7.5)
a.ROCK.RCLS7.5<-as_adjacency_matrix(i.ROCK.RCLS7.5,sparse=T)

i.ROCK.RCLS7.6<-ToIgraph(ROCK.RCLS7.6)
plot.igraph(i.ROCK.RCLS7.6)
a.ROCK.RCLS7.6<-as_adjacency_matrix(i.ROCK.RCLS7.6,sparse=T)

i.ROCK.RCLS7.7<-ToIgraph(ROCK.RCLS7.7)
plot.igraph(i.ROCK.RCLS7.1)
a.ROCK.RCLS7.7<-as_adjacency_matrix(i.ROCK.RCLS7.7,sparse=T)

i.ROCK.RCLS8.1<-ToIgraph(ROCK.RCLS8.1)
plot.igraph(i.ROCK.RCLS6.4)
a.ROCK.RCLS8.1<-as_adjacency_matrix(i.ROCK.RCLS8.1,sparse=T)

i.ROCK.RCLS8.2<-ToIgraph(ROCK.RCLS8.2)
plot.igraph(i.ROCK.RCLS8.2)
a.ROCK.RCLS8.2<-as_adjacency_matrix(i.ROCK.RCLS8.2,sparse=T)

i.ROCK.RCLS8.3<-ToIgraph(ROCK.RCLS8.3)
plot.igraph(i.ROCK.RCLS8.3)
a.ROCK.RCLS8.3<-as_adjacency_matrix(i.ROCK.RCLS8.3,sparse=T)

i.ROCK.RCLS8.4<-ToIgraph(ROCK.RCLS8.4)
plot.igraph(i.ROCK.RCLS8.4)
a.ROCK.RCLS8.4<-as_adjacency_matrix(i.ROCK.RCLS8.4,sparse=T)

i.ROCK.RCLS8.5<-ToIgraph(ROCK.RCLS8.5)
plot.igraph(i.ROCK.RCLS8.5)
a.ROCK.RCLS8.5<-as_adjacency_matrix(i.ROCK.RCLS8.5,sparse=T)

i.ROCK.RCLS8.7<-ToIgraph(ROCK.RCLS8.7)
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


