#Local.Food.Webs Sierra Nevada
library(tidyverse)
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
#BUBBS Sites Individually

#BUBBS Outlet.10477.trt.2003
BUBBS.Outlet.10477.trt.2003<-food.webz%>%
  filter(Network=="BUBBS" & Site=="Outlet.10477.trt.2003")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10477.trt.2003<-BUBBS.Outlet.10477.trt.2003%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.Outlet.10477.trt.2003,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10477.trt.2003<-list(title= "Outlet.10477.trt.2003", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.10477.trt.2003<-Community(BUBBS.food.web.Outlet.10477.trt.2003,Properties.Bubbs.Outlet.10477.trt.2003, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10477.trt.2003, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.10477.trt.2003/",fn='write.csv' )

BUBBSs.LS.Outlet.10477.trt.2003 <- Community(properties = CPS(BUBBS.Outlet.10477.trt.2003),
                                nodes = BUBBS.food.web.Outlet.10477.trt.2003,
                                trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10477.trt.2003, level='ChainAveragedTrophicLevel')

#Outlet.10477.trt.2004
BUBBS.Outlet.10477.trt.2004<-food.webz%>%
  filter(Network=="BUBBS" & Site=="Outlet.10477.trt.2004")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10477.trt.2004<-BUBBS.Outlet.10477.trt.2004%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.Outlet.10477.trt.2004,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10477.trt.2004<-list(title= "Outlet.10477.trt.2004", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.10477.trt.2004<-Community(BUBBS.food.web.Outlet.10477.trt.2004,Properties.Bubbs.Outlet.10477.trt.2004, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10477.trt.2004, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.10477.trt.2004/",fn='write.csv' )

BUBBSs.LS.Outlet.10477.trt.2004 <- Community(properties = CPS(BUBBS.Outlet.10477.trt.2004),
                                nodes = BUBBS.food.web.Outlet.10477.trt.2004,
                                trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10477.trt.2004, level='ChainAveragedTrophicLevel')


#Outlet.10477.trt.2011
BUBBS.Outlet.10477.trt.2011<-food.webz%>%
  filter(Network=="BUBBS" & Site=="Outlet.10477.trt.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10477.trt.2011<-BUBBS.Outlet.10477.trt.2011%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.Outlet.10477.trt.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10477.trt.2011<-list(title= "Outlet.10477.trt.2011", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.10477.trt.2011<-Community(BUBBS.food.web.Outlet.10477.trt.2011,Properties.Bubbs.Outlet.10477.trt.2011, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10477.trt.2011, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.10477.trt.2011/",fn='write.csv' )

BUBBSs.LS.Outlet.10477.trt.2011 <- Community(properties = CPS(BUBBS.Outlet.10477.trt.2011),
                                nodes = BUBBS.food.web.Outlet.10477.trt.2011,
                                trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10477.trt.2011, level='ChainAveragedTrophicLevel')

#BUBBS Outlet.10477.trt.2003
BUBBS.Outlet.10487.trt.2003<-food.webz%>%
  filter(Network=="BUBBS" & Site=="Outlet.10487.trt.2003")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10487.trt.2003<-BUBBS.Outlet.10487.trt.2003%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.Outlet.10487.trt.2003,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10487.trt.2003<-list(title= "Outlet.10487.trt.2003", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.10487.trt.2003<-Community(BUBBS.food.web.Outlet.10487.trt.2003,Properties.Bubbs.Outlet.10487.trt.2003, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10487.trt.2003, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.10487.trt.2003/",fn='write.csv' )

BUBBSs.LS.Outlet.10487.trt.2003 <- Community(properties = CPS(BUBBS.Outlet.10487.trt.2003),
                                             nodes = BUBBS.food.web.Outlet.10487.trt.2003,
                                             trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10487.trt.2003, level='ChainAveragedTrophicLevel')

#Outlet.10487.trt.2004
BUBBS.Outlet.10487.trt.2004<-food.webz%>%
  filter(Network=="BUBBS" & Site=="Outlet.10487.trt.2004")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10487.trt.2004<-BUBBS.Outlet.10487.trt.2004%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.Outlet.10487.trt.2004,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10487.trt.2004<-list(title= "Outlet.10487.trt.2004", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.10487.trt.2004<-Community(BUBBS.food.web.Outlet.10487.trt.2004,Properties.Bubbs.Outlet.10487.trt.2004, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10487.trt.2004, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.10487.trt.2004/",fn='write.csv' )

BUBBSs.LS.Outlet.10487.trt.2004 <- Community(properties = CPS(BUBBS.Outlet.10487.trt.2004),
                                             nodes = BUBBS.food.web.Outlet.10487.trt.2004,
                                             trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10487.trt.2004, level='ChainAveragedTrophicLevel')


#Outlet.10487.trt.2011
Outlet.10487.trt.2011<-food.webz%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.10487.trt.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.100487.trt.2011<-Outlet.10487.trt.2011%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.100487.trt.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10487.trt.2011<-list(title= "Outlet.10487.trt.2011", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.10487.trt.2011<-Community(BUBBS.food.web.100487.trt.2011,Properties.Bubbs.Outlet.10487.trt.2011, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10487.trt.2011, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.10487.trt.2011/",fn='write.csv' )

BUBBSs.LS.Outlet.10487.trt.2011 <- Community(properties = CPS(BUBBS.Outlet.10487.trt.2011),
                                             nodes = BUBBS.food.web.100487.trt.2011,
                                             trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10487.trt.2011, level='ChainAveragedTrophicLevel')


#Outlet.11007.fishless.2011
BUBBS.Outlet.11007.fishless.2011<-food.webz%>%
  filter(Network=="BUBBS" & Site=="Outlet.11007.fishless.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.11007.fishless.2011<-BUBBS.Outlet.11007.fishless.2011%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.Outlet.11007.fishless.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.11007.fishless.2011<-list(title= "Outlet.11007.fishless.2011", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.11007.fishless.2011<-Community(BUBBS.food.web.Outlet.11007.fishless.2011,Properties.Bubbs.Outlet.11007.fishless.2011, trophic.links=links)
SaveCommunity(BUBBS.Outlet.11007.fishless.2011, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.11007.fishless.2011/",fn='write.csv' )

BUBBSs.LS.Outlet.11007.fishless.2011 <- Community(properties = CPS(BUBBS.Outlet.11007.fishless.2011),
                                nodes = BUBBS.food.web.Outlet.11007.fishless.2011,
                                trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.11007.fishless.2011, level='ChainAveragedTrophicLevel')

###
Bubbs.Outlet.10494.trt.2012<-food.webz%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.10494.trt.2012")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.10494.trt.2012<-Bubbs.Outlet.10494.trt.2012%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(Bubbs.food.web.Outlet.10494.trt.2012,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10494.trt.2012<-list(title= "Outlet.10494.trt.2012", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Bubbs.Outlet.10494.trt.2012<-Community(Bubbs.food.web.Outlet.10494.trt.2012,Properties.Bubbs.Outlet.10494.trt.2012, trophic.links=links)
SaveCommunity(Bubbs.Outlet.10494.trt.2012, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Outlet.10494.trt.2012/",fn='write.csv' )

Bubbs.LS.Outlet.10494.trt.2012 <- Community(properties = CPS(Bubbs.Outlet.10494.trt.2012),
                                            nodes = Bubbs.food.web.Outlet.10494.trt.2012,
                                            trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.10494.trt.2012, level='ChainAveragedTrophicLevel')


#Outlet.11007.fishless.2003
BUBBS.Outlet.11007.fishless.2003<-food.webz%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.11007.fishless.2003")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.11007.fishless.2003<-BUBBS.Outlet.11007.fishless.2003%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.Outlet.11007.fishless.2003,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.BUBBS.Outlet.11007.fishless.2003<-list(title= "Outlet.11007.fishless.2003", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.11007.fishless.2003<-Community(BUBBS.food.web.Outlet.11007.fishless.2003,Properties.BUBBS.Outlet.11007.fishless.2003, trophic.links=links)
SaveCommunity(BUBBS.Outlet.11007.fishless.2003, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.11007.fishless.2003/",fn='write.csv' )

BUBBSs.LS.Outlet.11007.fishless.2003 <- Community(properties = CPS(BUBBS.Outlet.11007.fishless.2003),
                            nodes = BUBBS.food.web.Outlet.11007.fishless.2003,
                            trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.11007.fishless.2003, level='ChainAveragedTrophicLevel')

#Outlet.11007.fishless.2004
BUBBS.Outlet.11007.fishless.2004<-food.webz%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.11007.fishless.2004")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.11007.fishless.2004<-BUBBS.Outlet.11007.fishless.2004%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(BUBBS.food.web.Outlet.11007.fishless.2004,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.BUBBS.Outlet.11007.fishless.2004<-list(title= "Outlet.11007.fishless.2004", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
BUBBS.Outlet.11007.fishless.2004<-Community(BUBBS.food.web.Outlet.11007.fishless.2004,Properties.BUBBS.Outlet.11007.fishless.2004, trophic.links=links)
SaveCommunity(BUBBS.Outlet.11007.fishless.2004, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/Outlet.11007.fishless.2004/",fn='write.csv' )

BUBBSs.LS.Outlet.11007.fishless.2004 <- Community(properties = CPS(BUBBS.Outlet.11007.fishless.2004),
                                                  nodes = BUBBS.food.web.Outlet.11007.fishless.2004,
                                                  trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.11007.fishless.2004, level='ChainAveragedTrophicLevel')


#Outlet.Vidette.below.2003
Bubbs.Outlet.Vidette.below.2003<-food.webz%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.Vidette.below.2003")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.Vidette.below.2003<-Bubbs.Outlet.Vidette.below.2003%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(Bubbs.food.web.Outlet.Vidette.below.2003,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.Vidette.below.2003<-list(title= "Outlet.Vidette.below.2003", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Bubbs.Outlet.Vidette.below.2003<-Community(Bubbs.food.web.Outlet.Vidette.below.2003,Properties.Bubbs.Outlet.Vidette.below.2003, trophic.links=links)
SaveCommunity(Bubbs.Outlet.Vidette.below.2003, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Outlet.Vidette.below.2003/",fn='write.csv' )

Bubbs.LS.Outlet.Vidette.below.2003 <- Community(properties = CPS(Bubbs.Outlet.Vidette.below.2003),
                           nodes = Bubbs.food.web.Outlet.Vidette.below.2003,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.Vidette.below.2003, level='ChainAveragedTrophicLevel')

#Outlet.Vidette.below.2004
Bubbs.Outlet.Vidette.below.2004<-food.webz%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.Vidette.below.2004")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.Vidette.below.2004<-Bubbs.Outlet.Vidette.below.2004%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(Bubbs.food.web.Outlet.Vidette.below.2004,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.Vidette.below.2004<-list(title= "Outlet.Vidette.below.2004", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Bubbs.Outlet.Vidette.below.2004<-Community(Bubbs.food.web.Outlet.Vidette.below.2004,Properties.Bubbs.Outlet.Vidette.below.2004, trophic.links=links)
SaveCommunity(Bubbs.Outlet.Vidette.below.2004, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Outlet.Vidette.below.2004/",fn='write.csv' )

Bubbs.LS.Outlet.Vidette.below.2004 <- Community(properties = CPS(Bubbs.Outlet.Vidette.below.2004),
                           nodes = Bubbs.food.web.Outlet.Vidette.below.2004,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.Vidette.below.2004, level='ChainAveragedTrophicLevel')

#Outlet.Vidette.below.2011
Bubbs.Outlet.Vidette.below.2011<-food.webz%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.Vidette.below.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.Vidette.below.2011<-Bubbs.Outlet.Vidette.below.2011%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"
Bubbs.food.web.Outlet.Vidette.below.2011
nodes<-cbind(Bubbs.Outlet.Vidette.below.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.Vidette.below.2011<-list(title= "Outlet.Vidette.below.2011", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Bubbs.Outlet.Vidette.below.2011<-Community(Bubbs.food.web.Outlet.Vidette.below.2011,Properties.Bubbs.Outlet.Vidette.below.2011, trophic.links=links)
SaveCommunity(Bubbs.Outlet.Vidette.below.2011, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Outlet.Vidette.below.2011/",fn='write.csv' )

Bubbs.LS.Outlet.Vidette.below.2011 <- Community(properties = CPS(Bubbs.Outlet.Vidette.below.2011),
                           nodes = Bubbs.food.web.Outlet.Vidette.below.2011,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.Vidette.below.2011, level='ChainAveragedTrophicLevel')

#Outlet.Vidette.below.2012
Bubbs.Outlet.Vidette.below.2012<-food.webz%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.Vidette.below.2012")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.Vidette.below.2012<-Bubbs.Outlet.Vidette.below.2012%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "genus"

nodes<-cbind(Bubbs.food.web.Outlet.Vidette.below.2012,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.Vidette.below.2012<-list(title= "Outlet.Vidette.below.2012", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Bubbs.Outlet.Vidette.below.2012<-Community(Bubbs.food.web.Outlet.Vidette.below.2012,Properties.Bubbs.Outlet.Vidette.below.2012, trophic.links=links)
SaveCommunity(Bubbs.Outlet.Vidette.below.2012, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Bubbs/Outlet.Vidette.below.2012/",fn='write.csv' )

Bubbs.LS.Outlet.Vidette.below.2012 <- Community(properties = CPS(Bubbs.Outlet.Vidette.below.2012),
                           nodes = Bubbs.food.web.Outlet.Vidette.below.2012,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.Vidette.below.2012, level='ChainAveragedTrophicLevel')




#Done with BUBBS LS food webs wohooooo!
################################################################################################################################################################################################
#COmbine three local food webs into communtiy colelction

BUBBS.order<-CommunityCollection(list(BUBBSs.LS.Outlet.10477.trt.2003,BUBBSs.LS.Outlet.10477.trt.2004,BUBBSs.LS.Outlet.10477.trt.2011,BUBBSs.LS.Outlet.10487.trt.2003,BUBBSs.LS.Outlet.10487.trt.2004,BUBBSs.LS.Outlet.10487.trt.2011,BUBBSs.LS.Outlet.10494.trt.2012,BUBBSs.LS.Outlet.11007.fishless.2003,BUBBSs.LS.Outlet.11007.fishless.2004,BUBBSs.LS.Outlet.11007.fishless.2011,BUBBSs.LS.Outlet.Vidette.below.2003))

CollectionCPS(Bubbsading,  properties=NULL)

dads<-CollectionCPS(Bubbsading, 
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

temp.path <-"~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/BUBBS/BUBBS.sites.order/"
SaveCollection(BUBBS.order, temp.path)
BUBBS.order.loaded<-LoadCollection(temp.path)

envs <-read.csv("stream.env.csv")
BUBBS.env<-envs%>%filter(Network=="BUBBS")

BUBBS.all<-cbind(BUBBS.env,dads)

dev.off()
par(mfrow=c(4,6))
envlist=names(BUBBS.env)

for(i in c(2:11,15:17,19:25,27,30:32)) # this creates a loop of 23 iterations
{
  plot(BUBBS.env[,i],dads$C , xlab=envlist[c(i)],ylab="Connectance") 
  test2 = lm(dads$C ~BUBBS.env[,i])
  abline(test2)
}
varlist<-names(BUBBS.env)[c(2:11,15:17,19:25,27,30:32)]

models <- lapply(varlist, function(y) {
  lm(substitute(dads$T~i, list(i = as.name(y))), data = BUBBS.env)
})
lapply(models, summary)





CollectionNPS(Nets, properties=NULL)

CollectionTLPS(Nets, node.properties=NULL, link.properties=NULL)

###################################################################