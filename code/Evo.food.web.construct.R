#EVO food webs
library(tidyverse)
library(cheddar)
source(WebBuilder)

setwd("~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/data")

registry<-read.csv('newest.registry.csv')

food.web<-read.csv("pivoted.sp.data.csv")
fn.trait<-read.csv("Full_fn_trait.csv")
summary(food.web)
fn.traits<-fn.trait%>%select(c(Taxonomic_name,M))#%>%rename("Taxonomic_name"="Full_List_Taxa")
food.webz<-left_join(fn.traits,food.web, by="Taxonomic_name")
################################################################################################################################################################################################################
#EVO Sites Individually

#EVO ELS2_1
EVO.ELS2_1<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS2_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS2_1<-EVO.ELS2_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS2_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS2_1<-list(title= "ELS2_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS2_1<-Community(EVO.food.web.ELS2_1,Properties.EVO.ELS2_1, trophic.links=links)
SaveCommunity(EVO.ELS2_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS2_1/",fn='write.csv' )

EVOs.LS.ELS2_1 <- Community(properties = CPS(EVO.ELS2_1),
                                nodes = EVO.food.web.ELS2_1,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS2_1, level='ChainAveragedTrophicLevel')

#ELS2_2
EVO.ELS2_2<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS2_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS2_2<-EVO.ELS2_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS2_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))


Properties.EVO.ELS2_2<-list(title= "ELS2_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS2_2<-Community(EVO.food.web.ELS2_2,Properties.EVO.ELS2_2, trophic.links=links)
SaveCommunity(EVO.ELS2_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS2_2/",fn='write.csv' )

EVOs.LS.ELS2_2 <- Community(properties = CPS(EVO.ELS2_2),
                                nodes = EVO.food.web.ELS2_2,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS2_2, level='ChainAveragedTrophicLevel')


#ELS2_3
EVO.ELS2_3<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS2_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS2_3<-EVO.ELS2_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS2_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS2_3<-list(title= "ELS2_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS2_3<-Community(EVO.food.web.ELS2_3,Properties.EVO.ELS2_3, trophic.links=links)
SaveCommunity(EVO.ELS2_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS2_3/",fn='write.csv' )

EVOs.LS.ELS2_3 <- Community(properties = CPS(EVO.ELS2_3),
                                nodes = EVO.food.web.ELS2_3,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS2_3, level='ChainAveragedTrophicLevel')

#ELS3_1
EVO.ELS3_1<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS3_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS3_1<-EVO.ELS3_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS3_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS3_1<-list(title= "ELS3_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS3_1<-Community(EVO.food.web.ELS3_1,Properties.EVO.ELS3_1, trophic.links=links)
SaveCommunity(EVO.ELS3_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS3_1/",fn='write.csv' )

EVOs.LS.ELS3_1 <- Community(properties = CPS(EVO.ELS3_1),
                                nodes = EVO.food.web.ELS3_1,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS3_1, level='ChainAveragedTrophicLevel')


#ELS3_2
EVO.ELS3_2<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS3_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS3_2<-EVO.ELS3_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS3_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS3_2<-list(title= "ELS3_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS3_2<-Community(EVO.food.web.ELS3_2,Properties.EVO.ELS3_2, trophic.links=links)
SaveCommunity(EVO.ELS3_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS3_2/",fn='write.csv' )

EVOs.LS.ELS3_2 <- Community(properties = CPS(EVO.ELS3_2),
                                nodes = EVO.food.web.ELS3_2,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS3_2, level='ChainAveragedTrophicLevel')




#ELS3_3
EVO.ELS3_3<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS3_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS3_3<-EVO.ELS3_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS3_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS3_3<-list(title= "ELS3_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS3_3<-Community(EVO.food.web.ELS3_3,Properties.EVO.ELS3_3, trophic.links=links)
SaveCommunity(EVO.ELS3_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS3_3/",fn='write.csv' )

EVOs.LS.ELS3_3 <- Community(properties = CPS(EVO.ELS3_3),
                                nodes = EVO.food.web.ELS3_3,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS3_3, level='ChainAveragedTrophicLevel')

#ELS4_1
EVO.ELS4_1<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS4_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS4_1<-EVO.ELS4_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS4_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS4_1<-list(title= "ELS4_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS4_1<-Community(EVO.food.web.ELS4_1,Properties.EVO.ELS4_1, trophic.links=links)
SaveCommunity(EVO.ELS4_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS4_1/",fn='write.csv' )

EVOs.LS.ELS4_1 <- Community(properties = CPS(EVO.ELS4_1),
                                nodes = EVO.food.web.ELS4_1,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS4_1, level='ChainAveragedTrophicLevel')

#ELS4_2
EVO.ELS4_2<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS4_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS4_2<-EVO.ELS4_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS4_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS4_2<-list(title= "ELS4_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS4_2<-Community(EVO.food.web.ELS4_2,Properties.EVO.ELS4_2, trophic.links=links)
SaveCommunity(EVO.ELS4_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS4_2/",fn='write.csv' )

EVOs.LS.ELS4_2 <- Community(properties = CPS(EVO.ELS4_2),
                                nodes = EVO.food.web.ELS4_2,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS4_2, level='ChainAveragedTrophicLevel')

#ELS4_3
EVO.ELS4_3<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS4_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS4_3<-EVO.ELS4_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS4_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS4_3<-list(title= "ELS4_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS4_3<-Community(EVO.food.web.ELS4_3,Properties.EVO.ELS4_3, trophic.links=links)
SaveCommunity(EVO.ELS4_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS4_3/",fn='write.csv' )

EVOs.LS.ELS4_3 <- Community(properties = CPS(EVO.ELS4_3),
                                nodes = EVO.food.web.ELS4_3,
                                trophic.links = links)

PlotWebByLevel(EVO.ELS4_3, level='ChainAveragedTrophicLevel')

#ELS5_1
EVO.ELS5_1<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS5_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS5_1<-EVO.ELS5_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS5_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS5_1<-list(title= "ELS5_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS5_1<-Community(EVO.food.web.ELS5_1,Properties.EVO.ELS5_1, trophic.links=links)
SaveCommunity(EVO.ELS5_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS5_1/",fn='write.csv' )

EVOs.LS.ELS5_1 <- Community(properties = CPS(EVO.ELS5_1),
                                nodes = EVO.food.web.ELS5_1,
                                trophic.links = links)
EVOs.LS.ELS5_1
PlotWebByLevel(EVO.ELS5_1, level='ChainAveragedTrophicLevel')


#ELS5_2
EVO.ELS5_2<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS5_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS5_2<-EVO.ELS5_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS5_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS5_2<-list(title= "ELS5_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS5_2<-Community(EVO.food.web.ELS5_2,Properties.EVO.ELS5_2, trophic.links=links)
SaveCommunity(EVO.ELS5_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS5_2/",fn='write.csv' )

EVOs.LS.ELS5_2 <- Community(properties = CPS(EVO.ELS5_2),
                            nodes = EVO.food.web.ELS5_2,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS5_2, level='ChainAveragedTrophicLevel')


#ELS5_3
EVO.ELS5_3<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS5_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS5_3<-EVO.ELS5_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS5_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS5_3<-list(title= "ELS5_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS5_3<-Community(EVO.food.web.ELS5_3,Properties.EVO.ELS5_3, trophic.links=links)
SaveCommunity(EVO.ELS5_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS5_3/",fn='write.csv' )

EVOs.LS.ELS5_3 <- Community(properties = CPS(EVO.ELS5_3),
                            nodes = EVO.food.web.ELS5_3,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS5_3, level='ChainAveragedTrophicLevel')



#ELS6_1
EVO.ELS6_1<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS6_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS6_1<-EVO.ELS6_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS6_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.EVO.ELS6_1<-list(title= "ELS6_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS6_1<-Community(EVO.food.web.ELS6_1,Properties.EVO.ELS6_1, trophic.links=links)
SaveCommunity(EVO.ELS6_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS6_1/",fn='write.csv' )

EVOs.LS.ELS6_1 <- Community(properties = CPS(EVO.ELS6_1),
                            nodes = EVO.food.web.ELS6_1,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS6_1, level='ChainAveragedTrophicLevel')


#ELS6_2
EVO.ELS6_2<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS6_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS6_2<-EVO.ELS6_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS6_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS6_2<-list(title= "ELS6_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS6_2<-Community(EVO.food.web.ELS6_2,Properties.EVO.ELS6_2, trophic.links=links)
SaveCommunity(EVO.ELS6_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS6_2/",fn='write.csv' )

EVOs.LS.ELS6_2 <- Community(properties = CPS(EVO.ELS6_2),
                            nodes = EVO.food.web.ELS6_2,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS6_2, level='ChainAveragedTrophicLevel')



#ELS6_3
EVO.ELS6_3<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS6_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS6_3<-EVO.ELS6_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS6_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS6_3<-list(title= "ELS6_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS6_3<-Community(EVO.food.web.ELS6_3,Properties.EVO.ELS6_3, trophic.links=links)
SaveCommunity(EVO.ELS6_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS6_3/",fn='write.csv' )

EVOs.LS.ELS6_3 <- Community(properties = CPS(EVO.ELS6_3),
                            nodes = EVO.food.web.ELS6_3,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS6_3, level='ChainAveragedTrophicLevel')



#ELS7_1
EVO.ELS7_1<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS7_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS7_1<-EVO.ELS7_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS7_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS7_1<-list(title= "ELS7_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS7_1<-Community(EVO.food.web.ELS7_1,Properties.EVO.ELS7_1, trophic.links=links)
SaveCommunity(EVO.ELS7_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS7_1/",fn='write.csv' )

EVOs.LS.ELS7_1 <- Community(properties = CPS(EVO.ELS7_1),
                            nodes = EVO.food.web.ELS7_1,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS7_1, level='ChainAveragedTrophicLevel')



#ELS7_2
EVO.ELS7_2<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS7_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS7_2<-EVO.ELS7_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS7_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS7_2<-list(title= "ELS7_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS7_2<-Community(EVO.food.web.ELS7_2,Properties.EVO.ELS7_2, trophic.links=links)
SaveCommunity(EVO.ELS7_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS7_2/",fn='write.csv' )

EVOs.LS.ELS7_2 <- Community(properties = CPS(EVO.ELS7_2),
                            nodes = EVO.food.web.ELS7_2,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS7_2, level='ChainAveragedTrophicLevel')



#ELS7_3
EVO.ELS7_3<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS7_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS7_3<-EVO.ELS7_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS7_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS7_3<-list(title= "ELS7_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS7_3<-Community(EVO.food.web.ELS7_3,Properties.EVO.ELS7_3, trophic.links=links)
SaveCommunity(EVO.ELS7_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS7_3/",fn='write.csv' )

EVOs.LS.ELS7_3 <- Community(properties = CPS(EVO.ELS7_3),
                            nodes = EVO.food.web.ELS7_3,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS7_3, level='ChainAveragedTrophicLevel')



#ELS8_1
EVO.ELS8_1<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS8_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS8_1<-EVO.ELS8_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS8_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS8_1<-list(title= "ELS8_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS8_1<-Community(EVO.food.web.ELS8_1,Properties.EVO.ELS8_1, trophic.links=links)
SaveCommunity(EVO.ELS8_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS8_1/",fn='write.csv' )

EVOs.LS.ELS8_1 <- Community(properties = CPS(EVO.ELS8_1),
                            nodes = EVO.food.web.ELS8_1,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS8_1, level='ChainAveragedTrophicLevel')



#ELS8_2
EVO.ELS8_2<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS8_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS8_2<-EVO.ELS8_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS8_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS8_2<-list(title= "ELS8_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS8_2<-Community(EVO.food.web.ELS8_2,Properties.EVO.ELS8_2, trophic.links=links)
SaveCommunity(EVO.ELS8_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS8_2/",fn='write.csv' )

EVOs.LS.ELS8_2 <- Community(properties = CPS(EVO.ELS8_2),
                            nodes = EVO.food.web.ELS8_2,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS8_2, level='ChainAveragedTrophicLevel')
PlotWebByLevel(EVO.ELS7_3, show.nodes.as='labels',
               node.labels=EVO.ELS7_3$nodes$node,
               main='Level plot')
PlotWebByLevel(EVO.ELS7_3, main='PlotWebByLevel', 
               level=ChainAveragedTrophicLevel, x.layout='skinny', 
               ylab='Trophic level', show.level.labels=TRUE)


#ELS8_3
EVO.ELS8_3<-food.webz%>%
  filter(Network=="EVO" & Site=="ELS8_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

EVO.food.web.ELS8_3<-EVO.ELS8_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(EVO.food.web.ELS8_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.EVO.ELS8_3<-list(title= "ELS8_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
EVO.ELS8_3<-Community(EVO.food.web.ELS8_3,Properties.EVO.ELS8_3, trophic.links=links)
SaveCommunity(EVO.ELS8_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/EVO/ELS8_3/",fn='write.csv' )

EVOs.LS.ELS8_3 <- Community(properties = CPS(EVO.ELS8_3),
                            nodes = EVO.food.web.ELS8_3,
                            trophic.links = links)

PlotWebByLevel(EVO.ELS8_3, level='ChainAveragedTrophicLevel',show.level.labels=T,frame.plot=T)
PlotWebByLevel(EVO.ELS8_3)
#Done with EVO LS food webs wohooooo!
#########################################################


#COmbine three local food webs into communtiy colelction

Evo.order<-CommunityCollection(list(EVOs.LS.ELS2_1,EVOs.LS.ELS2_2,EVOs.LS.ELS2_3,EVOs.LS.ELS3_1,EVOs.LS.ELS3_2,EVOs.LS.ELS3_3,EVOs.LS.ELS4_1,EVOs.LS.ELS4_2,EVOs.LS.ELS4_3,EVOs.LS.ELS5_1,EVOs.LS.ELS5_2,EVOs.LS.ELS5_3,EVOs.LS.ELS6_1,EVOs.LS.ELS6_2,EVOs.LS.ELS6_3,EVOs.LS.ELS7_1,EVOs.LS.ELS7_2,EVOs.LS.ELS7_3,EVOs.LS.ELS8_1,EVOs.LS.ELS8_2,EVOs.LS.ELS8_3))

CollectionCPS(Evoing,  properties=NULL)

dads<-CollectionCPS(Evoing, 
                    c('title',
                      Species='NumberofSpecies',
                      O='DirectOmnivory',
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

temp.path <-"~/Users/matthewdouglasgreen/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/EVO/Evo.site.order/"
SaveCollection(Evo.order, temp.path)
Evo.order.loaded<-LoadCollection(temp.path)



envs <-read.csv("stream.env.csv")
evo.env<-envs%>%filter(Network=="EVO")

evo.all<-cbind(evo.env,dads)

dev.off()
par(mfrow=c(4,6))
envlist=names(evo.env)

for(i in c(2:11,15:17,19:25,27,30:32)) # this creates a loop of 23 iterations
{
  plot(evo.env[,i],dads$L , xlab=envlist[c(i)],ylab="Connectance") 
  test2 = lm(dads$L ~evo.env[,i])
  abline(test2)
}
varlist<-names(evo.env)[c(2:11,15:17,19:25,27,30:32)]

models <- lapply(varlist, function(y) {
  lm(substitute(dads$L~i, list(i = as.name(y))), data = evo.env)
})
lapply(models, summary)


ggplot(evo.all, aes(x=as.factor(Stream.Number), y=L))+geom_boxplot()


CollectionNPS(Nets, properties=NULL)

CollectionTLPS(Nets, node.properties=NULL, link.properties=NULL)

#################################################################################