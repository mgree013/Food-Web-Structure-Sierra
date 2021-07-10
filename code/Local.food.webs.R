#Local.Food.Webs Sierra Nevada
library(tidyverse )
library(cheddar)
##################################################################################################################################################################################################################
#setwd("~/Users/matthewdouglasgreen/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web.Web/Sierra.Nevada.LS.Food.Web")
registry<-read.csv('new.registry.csv')

food.web<-read.csv("dog.csv")

#Cascade
Cascade<-food.web%>%
  filter(Network=="CASCADE")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web<-food.web%>%
  filter(Network=="CASCADE")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades<-left_join(Cascade,Cascade.food.web)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades)<-Cascade$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "order"
minimum.con.method <-  "order"

nodes<-cbind(Cascades,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc<-list(title= "Cascade", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade<-Community(Cascades,Properties.Casc, trophic.links=links)
SaveCommunity(Cascade, dir ="~/Dropbox/Sierra Lake Project/Food.Webs//Joining.the.dots.Web/ScienceDirect_files_21Oct2019_15-14-40.23/Sites/Cascade",fn='write.csv' )

Cascade.LS <- Community(properties = CPS(Cascade),
                              nodes = Cascades,
                              trophic.links = links)

PlotWebByLevel(Cascade, level='ChainAveragedTrophicLevel')

######################################################################################################################################################################################



#EVo
Evo<-food.web%>%
  filter(Network=="EVO")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Evo.food.web<-food.web%>%
  filter(Network=="EVO")%>%
  dplyr::group_by(Taxonomic_name)%>%
  summarize(density=mean(value))

Evos<-left_join(Evo,Evo.food.web)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Evos)<-Evo$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method <-  "family"

nodes<-cbind(Evos,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Evo<-list(title= "Evo", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Evo<-Community(Evos,Properties.Evo, trophic.links=links)
SaveCommunity(Evo, dir ="~/Dropbox/Sierra Lake Project/Food.Webs//Joining.the.dots.Web/ScienceDirect_files_21Oct2019_15-14-40.23/Sites/Evo",fn='write.csv' )

Evo.LS <- Community(properties = CPS(Evo),
                              nodes = Evos,
                              trophic.links = links)

PlotWebByLevel(Evo, level='ChainAveragedTrophicLevel')

################################################################################################################################################################################################################

#Rae
Rae<-food.web%>%
  filter(Network=="RAE")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Rae.food.web<-food.web%>%
  filter(Network=="RAE")%>%
  dplyr::group_by(Taxonomic_name)%>%
  summarize(density=mean(value))

Raes<-left_join(Rae,Rae.food.web)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Raes)<-Rae$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method <-  "family"

nodes<-cbind(Raes,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Rae<-list(title= "Rae", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Rae<-Community(Raes,Properties.Rae, trophic.links=links)
SaveCommunity(Rae, dir ="~/Dropbox/Sierra Lake Project/Food.Webs//Joining.the.dots.Web/ScienceDirect_files_21Oct2019_15-14-40.23/Sites/Rae",fn='write.csv' )

Rae.LS <- Community(properties = CPS(Rae),
                              nodes = Raes,
                              trophic.links = links)

PlotWebByLevel(Rae.LS, level='ChainAveragedTrophicLevel')




################################################################################################################################################################################################################
#COmbine three local food webs into communtiy colelction

Nets<-CommunityCollection(list(Rae.LS,Evo.LS,Cascade.LS))

CollectionCPS(Nets,  properties=NULL)
                    
CollectionCPS(Nets, c('title',
                      S='NumberOfNodes',
                      L='NumberOfTrophicLinks',
                      'L/S'='LinkageDensity',
                      C='DirectedConnectance',
                      #Slope='NvMSlope',
                      B='FractionBasalNodes',
                      I='FractionIntermediateNodes',
                      T='FractionTopLevelNodes'))


CollectionNPS(Nets, properties=NULL)

CollectionTLPS(Nets, node.properties=NULL, link.properties=NULL)



################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################
################################################################################################################################################################################################################
################################################################################################################################################################################################################
#Cascade Sites Individually
setwd("~/Users/matthewdouglasgreen/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/")

#Cascade CLS1.1
Cascade.CLS1.1<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS1_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS1.1<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS1_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS1.1<-left_join(Cascade.CLS1.1,Cascade.food.web.CLS1.1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS1.1)<-Cascade.CLS1.1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS1.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS1.1<-list(title= "CLS1_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS1.1<-Community(Cascades.CLS1.1,Properties.Casc.CLS1.1, trophic.links=links)
SaveCommunity(Cascade.CLS1.1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS1.1/",fn='write.csv' )

Cascades.LS.CLS1.1 <- Community(properties = CPS(Cascade.CLS1.1),
                        nodes = Cascades.CLS1.1,
                        trophic.links = links)

PlotWebByLevel(Cascade.CLS1.1, level='ChainAveragedTrophicLevel')

#CLS1.3
Cascade.CLS1.3<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS1_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS1.3<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS1_3")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS1.3<-left_join(Cascade.CLS1.3,Cascade.food.web.CLS1.3)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS1.3)<-Cascade.CLS1.3$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS1.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS1.3<-list(title= "CLS1_3", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS1.3<-Community(Cascades.CLS1.3,Properties.Casc.CLS1.3, trophic.links=links)
SaveCommunity(Cascade.CLS1.3, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS1.3/",fn='write.csv' )

Cascades.LS.CLS1.3 <- Community(properties = CPS(Cascade.CLS1.3),
                                nodes = Cascades.CLS1.3,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS1.3, level='ChainAveragedTrophicLevel')


#CLS2.1
Cascade.CLS2.1<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS2_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS2.1<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS2_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS2.1<-left_join(Cascade.CLS2.1,Cascade.food.web.CLS2.1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS2.1)<-Cascade.CLS2.1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS2.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS2.1<-list(title= "CLS2_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS2.1<-Community(Cascades.CLS2.1,Properties.Casc.CLS2.1, trophic.links=links)
SaveCommunity(Cascade.CLS2.1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS2.1/",fn='write.csv' )

Cascades.LS.CLS2.1 <- Community(properties = CPS(Cascade.CLS2.1),
                                nodes = Cascades.CLS2.1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS2.1, level='ChainAveragedTrophicLevel')

#CLS2.3
Cascade.CLS2.3<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS2_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS2.3<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS2_3")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS2.3<-left_join(Cascade.CLS2.3,Cascade.food.web.CLS2.3)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS2.3)<-Cascade.CLS2.3$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS2.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS2.3<-list(title= "CLS2_3", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS2.3<-Community(Cascades.CLS2.3,Properties.Casc.CLS2.3, trophic.links=links)
SaveCommunity(Cascade.CLS2.3, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS2.3/",fn='write.csv' )

Cascades.LS.CLS2.3 <- Community(properties = CPS(Cascade.CLS2.3),
                                nodes = Cascades.CLS2.3,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS2.3, level='ChainAveragedTrophicLevel')


#CLS3.1
Cascade.CLS3.1<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS3_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS3.1<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS3_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS3.1<-left_join(Cascade.CLS3.1,Cascade.food.web.CLS3.1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS3.1)<-Cascade.CLS3.1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS3.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS3.1<-list(title= "CLS3_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS3.1<-Community(Cascades.CLS3.1,Properties.Casc.CLS3.1, trophic.links=links)
SaveCommunity(Cascade.CLS3.1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS3.1/",fn='write.csv' )

Cascades.LS.CLS3.1 <- Community(properties = CPS(Cascade.CLS3.1),
                                nodes = Cascades.CLS3.1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS3.1, level='ChainAveragedTrophicLevel')

#CLS3.2
Cascade.CLS3.2<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS3_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS3.2<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS3_2")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS3.2<-left_join(Cascade.CLS3.2,Cascade.food.web.CLS3.2)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS3.2)<-Cascade.CLS3.2$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS3.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS3.2<-list(title= "CLS3_2", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS3.2<-Community(Cascades.CLS3.2,Properties.Casc.CLS3.2, trophic.links=links)
SaveCommunity(Cascade.CLS3.2, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS3.2/",fn='write.csv' )

Cascades.LS.CLS3.2 <- Community(properties = CPS(Cascade.CLS3.2),
                                nodes = Cascades.CLS3.2,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS3.2, level='ChainAveragedTrophicLevel')


#CLS3.3
Cascade.CLS3.3<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS3_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS3.3<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS3_3")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS3.3<-left_join(Cascade.CLS3.3,Cascade.food.web.CLS3.3)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS3.3)<-Cascade.CLS3.3$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS3.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS3.3<-list(title= "CLS3_3", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS3.3<-Community(Cascades.CLS3.3,Properties.Casc.CLS3.3, trophic.links=links)
SaveCommunity(Cascade.CLS3.3, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS3.3/",fn='write.csv' )

Cascades.LS.CLS3.3 <- Community(properties = CPS(Cascade.CLS3.3),
                                nodes = Cascades.CLS3.3,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS3.3, level='ChainAveragedTrophicLevel')

#CLS4.1
Cascade.CLS4.1<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS4_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS4.1<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS4_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS4.1<-left_join(Cascade.CLS4.1,Cascade.food.web.CLS4.1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS4.1)<-Cascade.CLS4.1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS4.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS4.1<-list(title= "CLS4_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS4.1<-Community(Cascades.CLS4.1,Properties.Casc.CLS4.1, trophic.links=links)
SaveCommunity(Cascade.CLS4.1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS4.1/",fn='write.csv' )

Cascades.LS.CLS4.1 <- Community(properties = CPS(Cascade.CLS4.1),
                                nodes = Cascades.CLS4.1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS4.1, level='ChainAveragedTrophicLevel')

#CLS4.2
Cascade.CLS4.2<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS4_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS4.2<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS4_2")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS4.2<-left_join(Cascade.CLS4.2,Cascade.food.web.CLS4.2)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS4.2)<-Cascade.CLS4.2$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS4.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS4.2<-list(title= "CLS4_2", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS4.2<-Community(Cascades.CLS4.2,Properties.Casc.CLS4.2, trophic.links=links)
SaveCommunity(Cascade.CLS4.2, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS4.2/",fn='write.csv' )

Cascades.LS.CLS4.2 <- Community(properties = CPS(Cascade.CLS4.2),
                                nodes = Cascades.CLS4.2,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS4.2, level='ChainAveragedTrophicLevel')

#CLS4.3
Cascade.CLS4.3<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS4_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS4.3<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS4_3")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS4.3<-left_join(Cascade.CLS4.3,Cascade.food.web.CLS4.3)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS4.3)<-Cascade.CLS4.3$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS4.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS4.3<-list(title= "CLS4_3", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS4.3<-Community(Cascades.CLS4.3,Properties.Casc.CLS4.3, trophic.links=links)
SaveCommunity(Cascade.CLS4.3, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS4.3/",fn='write.csv' )

Cascades.LS.CLS4.3 <- Community(properties = CPS(Cascade.CLS4.3),
                                nodes = Cascades.CLS4.3,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS4.3, level='ChainAveragedTrophicLevel')

#CLS5.1
Cascade.CLS5.1<-food.web%>%
  filter(Network=="CASCADE" & Site=="CLS5_1 ")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS5.1<-food.web%>%
  filter(Network=="CASCADE"& Site=="CLS5_1 ")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Cascades.CLS5.1<-left_join(Cascade.CLS5.1,Cascade.food.web.CLS5.1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(Cascades.CLS5.1)<-Cascade.CLS5.1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "genus"
minimum.con.method <-  "genus"

nodes<-cbind(Cascades.CLS5.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS5.1<-list(title= "CLS5_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Cascade.CLS5.1<-Community(Cascades.CLS5.1,Properties.Casc.CLS5.1, trophic.links=links)
SaveCommunity(Cascade.CLS5.1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site/CLS5.1/",fn='write.csv' )

Cascades.LS.CLS5.1 <- Community(properties = CPS(Cascade.CLS5.1),
                                nodes = Cascades.CLS5.1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS5.1, level='ChainAveragedTrophicLevel')


#Done with Cascade LS food webs wohooooo!
################################################################################################################################################################################################
#COmbine three local food webs into communtiy colelction

Cascading<-CommunityCollection(list(Cascades.LS.CLS1.1,Cascades.LS.CLS1.3,Cascades.LS.CLS2.1,Cascades.LS.CLS2.3,Cascades.LS.CLS3.1,Cascades.LS.CLS3.2,Cascades.LS.CLS3.3,Cascades.LS.CLS4.1,Cascades.LS.CLS4.2,Cascades.LS.CLS4.3,Cascades.LS.CLS5.1))

CollectionCPS(Cascading,  properties=NULL)

dads<-CollectionCPS(Cascading, 
                    c('title',
                    Species='NumberofSpecies',
                    O='LevelofOmnivory',
                    S='NumberOfNodes',
                    L='NumberOfTrophicLinks',
                    'L/S'='LinkageDensity',
                    C='DirectedConnectance',
                    #Slope='NvMSlope',
                    B='FractionBasalNodes',
                    I='FractionIntermediateNodes',
                    T='FractionTopLevelNodes',
                    Isolated='FractionIsolatedNodes',
                    Prey.per.pred='PreyperPredator',
                    Pred.per.Prey='PredatorPerPrey'))

envs <-read.csv("stream.env.csv")
cascade.env<-envs%>%filter(Network=="CASCADE")

cascade.all<-cbind(cascade.env,dads)

dev.off()
par(mfrow=c(4,6))
envlist=names(cascade.env)

for(i in c(2:11,15:17,19:25,27,30:32)) # this creates a loop of 23 iterations
{
  plot(cascade.env[,i],dads$C , xlab=envlist[c(i)],ylab="Connectance") 
  test2 = lm(dads$C ~cascade.env[,i])
  abline(test2)
}
varlist<-names(cascade.env)[c(2:11,15:17,19:25,27,30:32)]

models <- lapply(varlist, function(y) {
  lm(substitute(dads$C~i, list(i = as.name(y))), data = cascade.env)
})
lapply(models, summary)





CollectionNPS(Nets, properties=NULL)

CollectionTLPS(Nets, node.properties=NULL, link.properties=NULL)

################################################################################################################################################################################################
