#File: Organizing environmental, spatial, and local food web metrics

#Libraries
library(cowplot)
library(viridis)
library(ggplot2)
library(reshape2)
library(metR)
library(igraph)
library(ggbiplot)
library(cheddar)
library(tidyverse)

##################################################################################################################################################################
#1) Load Food webs by Network

#cascade.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site.Genus/"
#cascade.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Cascade_Sites_Order_Level_02_12_20/"
cascade.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site.Family/"
Cascade.web<-LoadCollection(cascade.dir)

#evo.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/EVO/Site.Genus/"
#evo.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/EVO/Site.Order/"
evo.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/EVO/Site.Family/"
Evo.web<-LoadCollection(evo.dir)

#rae.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site.Genus/"
#rae.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Rae_site_order_02_12_20/"
rae.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site.Family/"
Rae.web<-LoadCollection(rae.dir)

#kern.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/KERN/Site.Genus/"
#kern.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/KERN/Site.Order/"
kern.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/KERN/Site.Family/"
Kern.web<-LoadCollection(kern.dir)

#bubbs.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site.Genus/"
#bubbs.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site.Order/"
bubbs.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site.Family/"
Bubbs.web<-LoadCollection(bubbs.dir)

#rock.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Rock/Genus/"
#rock.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Rock/Family/"
rock.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Rock/Family/"
Rock.web<-LoadCollection(rock.dir)

Rae.data<-CollectionCPS(Rae.web, 
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
                          #Sim.max='MaximumTrophicSimilarity',
                          #Vul='NormalisedTrophicVulnerability',
                          #Gen='NormalisedTrophicGenerality',
                          Path='CharacteristicPathLength'
                        ))

Evo.data<-CollectionCPS(Evo.web, 
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
                          #Sim.max='MaximumTrophicSimilarity',
                          #Vul='NormalisedTrophicVulnerability',
                          #Gen='NormalisedTrophicGenerality',
                          Path='CharacteristicPathLength'
                        ))

Cascade.data<-CollectionCPS(Cascade.web, 
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
                              #Sim.max='MaximumTrophicSimilarity',
                              #Vul='NormalisedTrophicVulnerability',
                              #Gen='NormalisedTrophicGenerality',
                              Path='CharacteristicPathLength'
                            ))

Kern.data<-CollectionCPS(Kern.web, 
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
                           #Sim.max='MaximumTrophicSimilarity',
                           #Vul='NormalisedTrophicVulnerability',
                           #Gen='NormalisedTrophicGenerality',
                           Path='CharacteristicPathLength'
                         ))

Bubbs.data<-CollectionCPS(Bubbs.web, 
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
                            #Sim.max='MaximumTrophicSimilarity',
                            #Vul='NormalisedTrophicVulnerability',
                            #Gen='NormalisedTrophicGenerality',
                            Path='CharacteristicPathLength'
                          ))

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
                           #Sim.max='MaximumTrophicSimilarity',
                           #Vul='NormalisedTrophicVulnerability',
                           #Gen='NormalisedTrophicGenerality',
                           Path='CharacteristicPathLength'
                         ))

all.webs<-rbind(Kern.data,Cascade.data,Evo.data,Bubbs.data,Rae.data, Rock.data)
all.webs<-all.webs%>%tibble::rownames_to_column()%>%rename(Site = title) 
#setwd("~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web")

##################################################################################################################################################################
#2) Load Environmental and Spatial Data
envs <-read.csv("dave.matt.env.full.12.29.19.csv")
env<-envs%>%filter(Network == "RAE" | Network=="EVO" | Network =="CASCADE"| O.NET =="KERN" | Network=="BUBBS" | Network=="ROCK")


env<-env%>%mutate(Euc.dist.lake=log(1+Euc.dist.lake),River.dist.lake=log(1+River.dist.lake),Head.river.dist=log(1+Head.river.dist))%>%
  mutate(Size.net.dist=Head.river.dist*Up.Lake.area,Size.river.dist=River.dist.lake*Up.Lake.area,Elev.dist=River.dist.lake/Elevation)

env.webs<-dplyr::left_join(env,all.webs, by="Site")

##################################################################################################################################################################

