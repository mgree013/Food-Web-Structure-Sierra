#Project:The Geography of High Elevation Benthic Food Webs
#Date: 03/21/19-Present
#Authors:Matthew D. Green, David Herbst, and Kurt E. Anderson

#Data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.2fqz612qw
################################################################################################################################
#File: Organizing environmental, spatial, and local food web metrics

#Load Packages
Packages <- c("tidyverse", "ggplot2", "igraph", "reshape2","reshape", "viridis", "ggbiplot", "metR", "cheddar", "cowplot", "vegan","glmmTMB", "performance")
lapply(Packages, library, character.only = TRUE)

##################################################################################################################################################################
#1) Load Food webs by Network


#cascade.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Final_family"
cascade.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Family_genus"
Cascade.web<-LoadCollection(cascade.dir)

#evo.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Final_family/"
evo.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/Family_genus/"
Evo.web<-LoadCollection(evo.dir)

#rae.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Final_family/"
rae.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE/Family_genus/"
Rae.web<-LoadCollection(rae.dir)

#kern.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Final_family/"
kern.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Family_genus/"
Kern.web<-LoadCollection(kern.dir)

#bubbs.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Final_family/"
bubbs.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Family_genus/"
Bubbs.web<-LoadCollection(bubbs.dir)

#rock.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Final_family/"
rock.dir="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Family_genus/"
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
                          Path='CharacteristicPathLength',
                          Slope='NvMSlope'
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
                          Path='CharacteristicPathLength',
                          Slope='NvMSlope'
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
                              Path='CharacteristicPathLength',
                              Slope='NvMSlope'
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
                           Path='CharacteristicPathLength',
                           Slope='NvMSlope'
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
                            Path='CharacteristicPathLength',
                            Slope='NvMSlope'
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
                           Path='CharacteristicPathLength',
                           Slope='NvMSlope'
                         ))

all.webs<-rbind(Kern.data,Cascade.data,Evo.data,Bubbs.data,Rae.data, Rock.data)
all.webs<-all.webs%>%tibble::rownames_to_column()%>%dplyr::rename(Site = title) 
#setwd("~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web")

##################################################################################################################################################################
#2) Load Environmental and Spatial Data
envs <-read.csv("dave.matt.env.full.12.29.19.csv")
env<-envs%>%filter(Network == "RAE" | Network=="EVO" | Network =="CASCADE"| O.NET =="KERN" | Network=="BUBBS" | Network=="ROCK")


env<-env%>%mutate(Euc.dist.lake=log(1+Euc.dist.lake),River.dist.lake=log(1+River.dist.lake),Head.river.dist=log(1+Head.river.dist))%>%
  mutate(Size.net.dist=Head.river.dist*Up.Lake.area,Size.river.dist=River.dist.lake*Up.Lake.area,Elev.dist=River.dist.lake/Elevation)

env.webs<-dplyr::left_join(env,all.webs, by="Site")

##################################################################################################################################################################

