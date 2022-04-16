#Local.Food.Webs Sierra Nevada
library(tidyverse )
library(cheddar)
##################################################################################################################################################################################################################
setwd("~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/data")
registry<-read.csv('newest.registry.csv')

food.web<-read.csv("pivoted.sp.data.csv")
fn.trait<-read.csv("Full_fn_trait.csv")
summary(food.web)
fn.traits<-fn.trait%>%select(c(Taxonomic_name,M))#%>%rename("Taxonomic_name"="Full_List_Taxa")
food.webz<-left_join(fn.traits,food.web, by="Taxonomic_name")

################################################################################################################################################################################################################
#RAE Sites Individually
setwd("~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/RAE")

#RAE RLS1_3
RAE.RLS1_3<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS1_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS1_3<-RAE.RLS1_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS1_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS1_3<-list(title= "RLS1_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS1_3<-Community(RAE.food.web.RLS1_3,Properties.Casc.RLS1_3, trophic.links=links)
SaveCommunity(RAE.RLS1_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS1_3/",fn='write.csv' )

RAEs.LS.RLS1_3 <- Community(properties = CPS(RAE.RLS1_3),
                                nodes = RAE.food.web.RLS1_3,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS1_3, level='ChainAveragedTrophicLevel')

#RLS2_1
RAE.RLS2_1<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS2_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS2_1<-RAE.RLS2_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS2_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS2_1<-list(title= "RLS2_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS2_1<-Community(RAE.food.web.RLS2_1,Properties.Casc.RLS2_1, trophic.links=links)
SaveCommunity(RAE.RLS2_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS2_1/",fn='write.csv' )

RAEs.LS.RLS2_1 <- Community(properties = CPS(RAE.RLS2_1),
                                nodes = RAE.food.web.RLS2_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS2_1, level='ChainAveragedTrophicLevel')


#RLS3_1
RAE.RLS3_1<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS3_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS3_1<-RAE.RLS3_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS3_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS3_1<-list(title= "RLS3_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS3_1<-Community(RAE.food.web.RLS3_1,Properties.Casc.RLS3_1, trophic.links=links)
SaveCommunity(RAE.RLS3_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS3_1/",fn='write.csv' )

RAEs.LS.RLS3_1 <- Community(properties = CPS(RAE.RLS3_1),
                                nodes = RAE.food.web.RLS3_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS3_1, level='ChainAveragedTrophicLevel')

#RLS3_2
RAE.RLS3_2<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS3_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS3_2<-RAE.RLS3_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS3_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS3_2<-list(title= "RLS3_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS3_2<-Community(RAE.food.web.RLS3_2,Properties.Casc.RLS3_2, trophic.links=links)
SaveCommunity(RAE.RLS3_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS3_2/",fn='write.csv' )

RAEs.LS.RLS3_2 <- Community(properties = CPS(RAE.RLS3_2),
                                nodes = RAE.food.web.RLS3_2,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS3_2, level='ChainAveragedTrophicLevel')


#RLS4_1
RAE.RLS4_1<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS4_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS4_1<-RAE.RLS4_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS4_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS4_1<-list(title= "RLS4_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS4_1<-Community(RAE.food.web.RLS4_1,Properties.Casc.RLS4_1, trophic.links=links)
SaveCommunity(RAE.RLS4_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS4_1/",fn='write.csv' )

RAEs.LS.RLS4_1 <- Community(properties = CPS(RAE.RLS4_1),
                                nodes = RAE.food.web.RLS4_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS4_1, level='ChainAveragedTrophicLevel')

#RLS4_2
RAE.RLS4_2<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS4_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS4_2<-RAE.RLS4_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS4_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS4_2<-list(title= "RLS4_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS4_2<-Community(RAE.food.web.RLS4_2,Properties.Casc.RLS4_2, trophic.links=links)
SaveCommunity(RAE.RLS4_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS4_2/",fn='write.csv' )

RAEs.LS.RLS4_2 <- Community(properties = CPS(RAE.RLS4_2),
                                nodes = RAE.food.web.RLS4_2,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS4_2, level='ChainAveragedTrophicLevel')


#RLS4_3
RAE.RLS4_3<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS4_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS4_3<-RAE.RLS4_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS4_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS4_3<-list(title= "RLS4_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS4_3<-Community(RAE.food.web.RLS4_3,Properties.Casc.RLS4_3, trophic.links=links)
SaveCommunity(RAE.RLS4_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS4_3/",fn='write.csv' )

RAEs.LS.RLS4_3 <- Community(properties = CPS(RAE.RLS4_3),
                                nodes = RAE.food.web.RLS4_3,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS4_3, level='ChainAveragedTrophicLevel')

#RLS5_1
RAE.RLS5_1<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS5_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS5_1<-RAE.RLS5_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS5_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS5_1<-list(title= "RLS5_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS5_1<-Community(RAE.food.web.RLS5_1,Properties.Casc.RLS5_1, trophic.links=links)
SaveCommunity(RAE.RLS5_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS5_1/",fn='write.csv' )

RAEs.LS.RLS5_1 <- Community(properties = CPS(RAE.RLS5_1),
                                nodes = RAE.food.web.RLS5_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS5_1, level='ChainAveragedTrophicLevel')

#RLS5_2
RAE.RLS5_2<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS5_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS5_2<-RAE.RLS5_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS5_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS5_2<-list(title= "RLS5_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS5_2<-Community(RAE.food.web.RLS5_2,Properties.Casc.RLS5_2, trophic.links=links)
SaveCommunity(RAE.RLS5_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS5_2/",fn='write.csv' )

RAEs.LS.RLS5_2 <- Community(properties = CPS(RAE.RLS5_2),
                                nodes = RAE.food.web.RLS5_2,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS5_2, level='ChainAveragedTrophicLevel')

#RLS5_3
RAE.RLS5_3<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS5_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS5_3<-RAE.RLS5_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS5_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS5_3<-list(title= "RLS5_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS5_3<-Community(RAE.food.web.RLS5_3,Properties.Casc.RLS5_3, trophic.links=links)
SaveCommunity(RAE.RLS5_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS5_3/",fn='write.csv' )

RAEs.LS.RLS5_3 <- Community(properties = CPS(RAE.RLS5_3),
                                nodes = RAE.food.web.RLS5_3,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS5_3, level='ChainAveragedTrophicLevel')

#RLS6_1
RAE.RLS6_1<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS6_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS6_1<-RAE.RLS6_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS6_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS6_1<-list(title= "RLS6_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS6_1<-Community(RAE.food.web.RLS6_1,Properties.Casc.RLS6_1, trophic.links=links)
SaveCommunity(RAE.RLS6_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS6_1/",fn='write.csv' )

RAEs.LS.RLS6_1 <- Community(properties = CPS(RAE.RLS6_1),
                                nodes = RAE.food.web.RLS6_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS6_1, level='ChainAveragedTrophicLevel')

#RLS6_2
RAE.RLS6_2<-food.webz%>%
  filter(Network=="RAE" & Site=="RLS6_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS6_2<-RAE.RLS6_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAE.food.web.RLS6_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS6_2<-list(title= "RLS6_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
RAE.RLS6_2<-Community(RAE.food.web.RLS6_2,Properties.Casc.RLS6_2, trophic.links=links)
SaveCommunity(RAE.RLS6_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rae/RLS6_2/",fn='write.csv' )

RAEs.LS.RLS6_2 <- Community(properties = CPS(RAE.RLS6_2),
                                nodes = RAE.food.web.RLS6_2,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS6_2, level='ChainAveragedTrophicLevel')
#Done with RAE LS food webs wohooooo!
################################################################################################################################################################################################
#COmbine three local food webs into communtiy colelction

Rae.order<-CommunityCollection(list(RAEs.LS.RLS1_3,RAEs.LS.RLS2_1,RAEs.LS.RLS3_1,RAEs.LS.RLS3_2,RAEs.LS.RLS4_1,RAEs.LS.RLS4_2,RAEs.LS.RLS4_3,RAEs.LS.RLS5_1,RAEs.LS.RLS5_2,RAEs.LS.RLS5_3,RAEs.LS.RLS6_1,RAEs.LS.RLS6_2))

CollectionCPS(Rae.order,  properties=NULL)

Rae.data<-CollectionCPS(Rae.order, 
                    c('title',
                      S='NumberOfNodes',
                      L='NumberOfTrophicLinks',
                      'L/S'='LinkageDensity',
                      C='DirectedConnectance',
                      #Slope='NvMSlope',
                      B='FractionBasalNodes',
                      I='FractionIntermediateNodes',
                      T='FractionTopLevelNodes',
                      Isolated='FractionIsolatedNodes'
                      ))

###################################################################