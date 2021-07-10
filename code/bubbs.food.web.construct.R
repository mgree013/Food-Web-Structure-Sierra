#Local.Food.Webs Sierra Nevada
library(tidyverse)
library(cheddar)
##################################################################################################################################################################################################################
setwd("~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web")
registry<-read.csv('new.registry.csv')

food.web<-read.csv("pivoted.sp.data.csv")

################################################################################################################################################################################################################
#BUBBS Sites Individually
setwd("~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs")

#BUBBS Outlet.10477.trt.2003
BUBBS.Outlet.10477.trt.2003<-food.web%>%
  filter(Network=="BUBBS" & Site=="Outlet.10477.trt.2003")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10477.trt.2003<-food.web%>%
  filter(Network=="BUBBS"& Site=="Outlet.10477.trt.2003")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBSs.Outlet.10477.trt.2003<-left_join(BUBBS.Outlet.10477.trt.2003,BUBBS.food.web.Outlet.10477.trt.2003)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(BUBBSs.Outlet.10477.trt.2003)<-BUBBS.Outlet.10477.trt.2003$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBSs.Outlet.10477.trt.2003,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10477.trt.2003<-list(title= "Outlet.10477.trt.2003", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.10477.trt.2003<-Community(BUBBSs.Outlet.10477.trt.2003,Properties.Bubbs.Outlet.10477.trt.2003, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10477.trt.2003, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.10477.trt.2003/",fn='write.csv' )

BUBBSs.LS.Outlet.10477.trt.2003 <- Community(properties = CPS(BUBBS.Outlet.10477.trt.2003),
                                nodes = BUBBSs.Outlet.10477.trt.2003,
                                trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10477.trt.2003, level='ChainAveragedTrophicLevel')

#Outlet.10477.trt.2004
BUBBS.Outlet.10477.trt.2004<-food.web%>%
  filter(Network=="BUBBS" & Site=="Outlet.10477.trt.2004")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10477.trt.2004<-food.web%>%
  filter(Network=="BUBBS"& Site=="Outlet.10477.trt.2004")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBSs.Outlet.10477.trt.2004<-left_join(BUBBS.Outlet.10477.trt.2004,BUBBS.food.web.Outlet.10477.trt.2004)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(BUBBSs.Outlet.10477.trt.2004)<-BUBBS.Outlet.10477.trt.2004$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBSs.Outlet.10477.trt.2004,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10477.trt.2004<-list(title= "Outlet.10477.trt.2004", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.10477.trt.2004<-Community(BUBBSs.Outlet.10477.trt.2004,Properties.Bubbs.Outlet.10477.trt.2004, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10477.trt.2004, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.10477.trt.2004/",fn='write.csv' )

BUBBSs.LS.Outlet.10477.trt.2004 <- Community(properties = CPS(BUBBS.Outlet.10477.trt.2004),
                                nodes = BUBBSs.Outlet.10477.trt.2004,
                                trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10477.trt.2004, level='ChainAveragedTrophicLevel')


#Outlet.10477.trt.2011
BUBBS.Outlet.10477.trt.2011<-food.web%>%
  filter(Network=="BUBBS" & Site=="Outlet.10477.trt.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10477.trt.2011<-food.web%>%
  filter(Network=="BUBBS"& Site=="Outlet.10477.trt.2011")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBSs.Outlet.10477.trt.2011<-left_join(BUBBS.Outlet.10477.trt.2011,BUBBS.food.web.Outlet.10477.trt.2011)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(BUBBSs.Outlet.10477.trt.2011)<-BUBBS.Outlet.10477.trt.2011$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBSs.Outlet.10477.trt.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10477.trt.2011<-list(title= "Outlet.10477.trt.2011", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.10477.trt.2011<-Community(BUBBSs.Outlet.10477.trt.2011,Properties.Bubbs.Outlet.10477.trt.2011, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10477.trt.2011, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.10477.trt.2011/",fn='write.csv' )

BUBBSs.LS.Outlet.10477.trt.2011 <- Community(properties = CPS(BUBBS.Outlet.10477.trt.2011),
                                nodes = BUBBSs.Outlet.10477.trt.2011,
                                trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10477.trt.2011, level='ChainAveragedTrophicLevel')

#BUBBS Outlet.10477.trt.2003
BUBBS.Outlet.10487.trt.2003<-food.web%>%
  filter(Network=="BUBBS" & Site=="Outlet.10487.trt.2003")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10487.trt.2003<-food.web%>%
  filter(Network=="BUBBS"& Site=="Outlet.10487.trt.2003")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBSs.Outlet.10487.trt.2003<-left_join(BUBBS.Outlet.10487.trt.2003,BUBBS.food.web.Outlet.10487.trt.2003)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(BUBBSs.Outlet.10487.trt.2003)<-BUBBS.Outlet.10487.trt.2003$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBSs.Outlet.10487.trt.2003,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10487.trt.2003<-list(title= "Outlet.10487.trt.2003", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.10487.trt.2003<-Community(BUBBSs.Outlet.10487.trt.2003,Properties.Bubbs.Outlet.10487.trt.2003, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10487.trt.2003, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.10487.trt.2003/",fn='write.csv' )

BUBBSs.LS.Outlet.10487.trt.2003 <- Community(properties = CPS(BUBBS.Outlet.10487.trt.2003),
                                             nodes = BUBBSs.Outlet.10487.trt.2003,
                                             trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10487.trt.2003, level='ChainAveragedTrophicLevel')

#Outlet.10487.trt.2004
BUBBS.Outlet.10487.trt.2004<-food.web%>%
  filter(Network=="BUBBS" & Site=="Outlet.10487.trt.2004")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.10487.trt.2004<-food.web%>%
  filter(Network=="BUBBS"& Site=="Outlet.10487.trt.2004")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBSs.Outlet.10487.trt.2004<-left_join(BUBBS.Outlet.10487.trt.2004,BUBBS.food.web.Outlet.10487.trt.2004)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(BUBBSs.Outlet.10487.trt.2004)<-BUBBS.Outlet.10487.trt.2004$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBSs.Outlet.10487.trt.2004,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10487.trt.2004<-list(title= "Outlet.10487.trt.2004", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.10487.trt.2004<-Community(BUBBSs.Outlet.10487.trt.2004,Properties.Bubbs.Outlet.10487.trt.2004, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10487.trt.2004, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.10487.trt.2004/",fn='write.csv' )

BUBBSs.LS.Outlet.10487.trt.2004 <- Community(properties = CPS(BUBBS.Outlet.10487.trt.2004),
                                             nodes = BUBBSs.Outlet.10487.trt.2004,
                                             trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10487.trt.2004, level='ChainAveragedTrophicLevel')


#Outlet.10487.trt.2011
Outlet.10487.trt.2011.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.10487.trt.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.10052<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.10487.trt.2011")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Outlet.10487.trt.2011.n<-left_join(Outlet.10487.trt.2011.o,BUBBS.food.web.10052)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

Outlet.10487.trt.2011.l<-left_join(Outlet.10487.trt.2011.o,BUBBS.food.web.10052)%>%
  group_by(node)%>%
  summarise(density=sum(value))

Outlet.10487.trt.2011<-left_join(Outlet.10487.trt.2011.l,Outlet.10487.trt.2011.n)%>%
  select(density, phylum ,class, order, family, genus, node)

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Outlet.10487.trt.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10487.trt.2011<-list(title= "Outlet.10487.trt.2011", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.10487.trt.2011<-Community(Outlet.10487.trt.2011,Properties.Bubbs.Outlet.10487.trt.2011, trophic.links=links)
SaveCommunity(BUBBS.Outlet.10487.trt.2011, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.10487.trt.2011/",fn='write.csv' )

BUBBSs.LS.Outlet.10487.trt.2011 <- Community(properties = CPS(BUBBS.Outlet.10487.trt.2011),
                                             nodes = BUBBSs.Outlet.10487.trt.2011,
                                             trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.10487.trt.2011, level='ChainAveragedTrophicLevel')


#Outlet.11007.fishless.2011
BUBBS.Outlet.11007.fishless.2011<-food.web%>%
  filter(Network=="BUBBS" & Site=="Outlet.11007.fishless.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.11007.fishless.2011<-food.web%>%
  filter(Network=="BUBBS"& Site=="Outlet.11007.fishless.2011")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBSs.Outlet.11007.fishless.2011<-left_join(BUBBS.Outlet.11007.fishless.2011,BUBBS.food.web.Outlet.11007.fishless.2011)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(BUBBSs.Outlet.11007.fishless.2011)<-BUBBS.Outlet.11007.fishless.2011$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBSs.Outlet.11007.fishless.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.11007.fishless.2011<-list(title= "Outlet.11007.fishless.2011", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.11007.fishless.2011<-Community(BUBBSs.Outlet.11007.fishless.2011,Properties.Bubbs.Outlet.11007.fishless.2011, trophic.links=links)
SaveCommunity(BUBBS.Outlet.11007.fishless.2011, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site.Family/communities/Outlet.11007.fishless.2011/",fn='write.csv' )

BUBBSs.LS.Outlet.11007.fishless.2011 <- Community(properties = CPS(BUBBS.Outlet.11007.fishless.2011),
                                nodes = BUBBSs.Outlet.11007.fishless.2011,
                                trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.11007.fishless.2011, level='ChainAveragedTrophicLevel')

###
Bubbs.Outlet.10494.trt.2012.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.10494.trt.2012")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.10494.trt.2012<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.10494.trt.2012")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Bubbs.Outlet.10494.trt.2012.n<-left_join(Bubbs.Outlet.10494.trt.2012.o,Bubbs.food.web.Outlet.10494.trt.2012)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

Bubbs.Outlet.10494.trt.2012.l<-left_join(Bubbs.Outlet.10494.trt.2012.o,Bubbs.food.web.Outlet.10494.trt.2012)%>%
  group_by(node)%>%
  summarise(density=sum(value))

Bubbs.Outlet.10494.trt.2012<-left_join(Bubbs.Outlet.10494.trt.2012.l,Bubbs.Outlet.10494.trt.2012.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Bubbs.Outlet.10494.trt.2012,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10494.trt.2012<-list(title= "Outlet.10494.trt.2012", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Bubbs.Outlet.10494.trt.2012<-Community(Bubbs.Outlet.10494.trt.2012,Properties.Bubbs.Outlet.10494.trt.2012, trophic.links=links)
SaveCommunity(Bubbs.Outlet.10494.trt.2012, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site/Outlet.10494.trt.2012/",fn='write.csv' )

Bubbs.LS.Outlet.10494.trt.2012 <- Community(properties = CPS(Bubbs.Outlet.10494.trt.2012),
                                            nodes = Bubbs.Outlet.10494.trt.2012,
                                            trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.10494.trt.2012, level='ChainAveragedTrophicLevel')


#Outlet.10494.trt.2012
#not working
Bubbs.Outlet.10494.trt.2012.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.10494.trt.2012")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.10494.trt.2012<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.10494.trt.2012")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Bubbs.Outlet.10494.trt.2012.n<-left_join(Bubbs.Outlet.10494.trt.2012.o,Bubbs.food.web.Outlet.10494.trt.2012)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

Bubbs.Outlet.10494.trt.2012.l<-left_join(Bubbs.Outlet.10494.trt.2012.o,Bubbs.food.web.Outlet.10494.trt.2012)%>%
  group_by(node)%>%
  summarise(density=sum(value))

Bubbs.Outlet.10494.trt.2012<-left_join(Bubbs.Outlet.10494.trt.2012.l,Bubbs.Outlet.10494.trt.2012.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Bubbs.Outlet.10494.trt.2012,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10494.trt.2012<-list(title= "Outlet.10494.trt.2012", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Bubbs.Outlet.10494.trt.2012<-Community(Bubbs.Outlet.10494.trt.2012,Properties.Bubbs.Outlet.10494.trt.2012, trophic.links=links)
SaveCommunity(Bubbs.Outlet.10494.trt.2012, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site/Outlet.10494.trt.2012/",fn='write.csv' )

Bubbs.LS.Outlet.10494.trt.2012 <- Community(properties = CPS(Bubbs.Outlet.10494.trt.2012),
                           nodes = Bubbs.Outlet.10494.trt.2012,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.10494.trt.2012, level='ChainAveragedTrophicLevel')

#Outlet.11007.fishless.2003
BUBBS.Outlet.11007.fishless.2003.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.11007.fishless.2003")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.11007.fishless.2003<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.11007.fishless.2003")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBS.Outlet.11007.fishless.2003.n<-left_join(BUBBS.Outlet.11007.fishless.2003.o,BUBBS.food.web.Outlet.11007.fishless.2003)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

BUBBS.Outlet.11007.fishless.2003.l<-left_join(BUBBS.Outlet.11007.fishless.2003.o,BUBBS.food.web.Outlet.11007.fishless.2003)%>%
  group_by(node)%>%
  summarise(density=sum(value))

BUBBS.Outlet.11007.fishless.2003<-left_join(BUBBS.Outlet.11007.fishless.2003.l,BUBBS.Outlet.11007.fishless.2003.n)%>%
  select(density, phylum ,class, order, family, genus, node)

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBS.Outlet.11007.fishless.2003,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.BUBBS.Outlet.11007.fishless.2003<-list(title= "Outlet.11007.fishless.2003", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.11007.fishless.2003<-Community(BUBBS.Outlet.11007.fishless.2003,Properties.BUBBS.Outlet.11007.fishless.2003, trophic.links=links)
SaveCommunity(BUBBS.Outlet.11007.fishless.2003, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.11007.fishless.2003/",fn='write.csv' )

BUBBSs.LS.Outlet.11007.fishless.2003 <- Community(properties = CPS(BUBBS.Outlet.11007.fishless.2003),
                            nodes = BUBBS.Outlet.11007.fishless.2003,
                            trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.11007.fishless.2003, level='ChainAveragedTrophicLevel')

#Outlet.11007.fishless.2004
BUBBS.Outlet.11007.fishless.2004.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.11007.fishless.2004")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.11007.fishless.2004<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.11007.fishless.2004")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBS.Outlet.11007.fishless.2004.n<-left_join(BUBBS.Outlet.11007.fishless.2004.o,BUBBS.food.web.Outlet.11007.fishless.2004)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

BUBBS.Outlet.11007.fishless.2004.l<-left_join(BUBBS.Outlet.11007.fishless.2004.o,BUBBS.food.web.Outlet.11007.fishless.2004)%>%
  group_by(node)%>%
  summarise(density=sum(value))

BUBBS.Outlet.11007.fishless.2004<-left_join(BUBBS.Outlet.11007.fishless.2004.l,BUBBS.Outlet.11007.fishless.2004.n)%>%
  select(density, phylum ,class, order, family, genus, node)

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBS.Outlet.11007.fishless.2004,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.BUBBS.Outlet.11007.fishless.2004<-list(title= "Outlet.11007.fishless.2004", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.11007.fishless.2004<-Community(BUBBS.Outlet.11007.fishless.2004,Properties.BUBBS.Outlet.11007.fishless.2004, trophic.links=links)
SaveCommunity(BUBBS.Outlet.11007.fishless.2004, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.11007.fishless.2004/",fn='write.csv' )

BUBBSs.LS.Outlet.11007.fishless.2004 <- Community(properties = CPS(BUBBS.Outlet.11007.fishless.2004),
                                                  nodes = BUBBS.Outlet.11007.fishless.2004,
                                                  trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.11007.fishless.2004, level='ChainAveragedTrophicLevel')

#Outlet.11007.fishless.2011
BUBBS.Outlet.11007.fishless.2011.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.11007.fishless.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

BUBBS.food.web.Outlet.11007.fishless.2011<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.11007.fishless.2011")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

BUBBS.Outlet.11007.fishless.2011.n<-left_join(BUBBS.Outlet.11007.fishless.2011.o,BUBBS.food.web.Outlet.11007.fishless.2011)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

BUBBS.Outlet.11007.fishless.2011.l<-left_join(BUBBS.Outlet.11007.fishless.2011.o,BUBBS.food.web.Outlet.11007.fishless.2011)%>%
  group_by(node)%>%
  summarise(density=sum(value))

BUBBS.Outlet.11007.fishless.2011<-left_join(BUBBS.Outlet.11007.fishless.2011.l,BUBBS.Outlet.11007.fishless.2011.n)%>%
  select(density, phylum ,class, order, family, genus, node)

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(BUBBS.Outlet.11007.fishless.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.BUBBS.Outlet.11007.fishless.2011<-list(title= "Outlet.11007.fishless.2011", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
BUBBS.Outlet.11007.fishless.2011<-Community(BUBBS.Outlet.11007.fishless.2011,Properties.BUBBS.Outlet.11007.fishless.2011, trophic.links=links)
SaveCommunity(BUBBS.Outlet.11007.fishless.2011, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/Site/Outlet.11007.fishless.2011/",fn='write.csv' )

BUBBSs.LS.Outlet.11007.fishless.2011 <- Community(properties = CPS(BUBBS.Outlet.11007.fishless.2011),
                                                  nodes = BUBBS.Outlet.11007.fishless.2011,
                                                  trophic.links = links)

PlotWebByLevel(BUBBS.Outlet.11007.fishless.2011, level='ChainAveragedTrophicLevel')
#Outlet.Vidette.below.2003
Bubbs.Outlet.Vidette.below.2003.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.Vidette.below.2003")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.Vidette.below.2003<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.Vidette.below.2003")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Bubbs.Outlet.Vidette.below.2003.n<-left_join(Bubbs.Outlet.Vidette.below.2003.o,Bubbs.food.web.Outlet.Vidette.below.2003)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

Bubbs.Outlet.Vidette.below.2003.l<-left_join(Bubbs.Outlet.Vidette.below.2003.o,Bubbs.food.web.Outlet.Vidette.below.2003)%>%
  group_by(node)%>%
  summarise(density=sum(value))

Bubbs.Outlet.Vidette.below.2003<-left_join(Bubbs.Outlet.Vidette.below.2003.l,Bubbs.Outlet.Vidette.below.2003.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Bubbs.Outlet.Vidette.below.2003,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.Vidette.below.2003<-list(title= "Outlet.Vidette.below.2003", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Bubbs.Outlet.Vidette.below.2003<-Community(Bubbs.Outlet.Vidette.below.2003,Properties.Bubbs.Outlet.Vidette.below.2003, trophic.links=links)
SaveCommunity(Bubbs.Outlet.Vidette.below.2003, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site/Outlet.Vidette.below.2003/",fn='write.csv' )

Bubbs.LS.Outlet.Vidette.below.2003 <- Community(properties = CPS(Bubbs.Outlet.Vidette.below.2003),
                           nodes = Bubbs.Outlet.Vidette.below.2003,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.Vidette.below.2003, level='ChainAveragedTrophicLevel')

#Outlet.Vidette.below.2004
Bubbs.Outlet.Vidette.below.2004.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.Vidette.below.2004")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.Vidette.below.2004<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.Vidette.below.2004")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Bubbs.Outlet.Vidette.below.2004.n<-left_join(Bubbs.Outlet.Vidette.below.2004.o,Bubbs.food.web.Outlet.Vidette.below.2004)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

Bubbs.Outlet.Vidette.below.2004.l<-left_join(Bubbs.Outlet.Vidette.below.2004.o,Bubbs.food.web.Outlet.Vidette.below.2004)%>%
  group_by(node)%>%
  summarise(density=sum(value))

Bubbs.Outlet.Vidette.below.2004<-left_join(Bubbs.Outlet.Vidette.below.2004.l,Bubbs.Outlet.Vidette.below.2004.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Bubbs.Outlet.Vidette.below.2004,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.Vidette.below.2004<-list(title= "Outlet.Vidette.below.2004", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Bubbs.Outlet.Vidette.below.2004<-Community(Bubbs.Outlet.Vidette.below.2004,Properties.Bubbs.Outlet.Vidette.below.2004, trophic.links=links)
SaveCommunity(Bubbs.Outlet.Vidette.below.2004, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site/Outlet.Vidette.below.2004/",fn='write.csv' )

Bubbs.LS.Outlet.Vidette.below.2004 <- Community(properties = CPS(Bubbs.Outlet.Vidette.below.2004),
                           nodes = Bubbs.Outlet.Vidette.below.2004,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.Vidette.below.2004, level='ChainAveragedTrophicLevel')

#Outlet.Vidette.below.2011
Bubbs.Outlet.Vidette.below.2011.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.Vidette.below.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.Vidette.below.2011<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.Vidette.below.2011")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Bubbs.Outlet.Vidette.below.2011.n<-left_join(Bubbs.Outlet.Vidette.below.2011.o,Bubbs.food.web.Outlet.Vidette.below.2011)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

Bubbs.Outlet.Vidette.below.2011.l<-left_join(Bubbs.Outlet.Vidette.below.2011.o,Bubbs.food.web.Outlet.Vidette.below.2011)%>%
  group_by(node)%>%
  summarise(density=sum(value))

Bubbs.Outlet.Vidette.below.2011<-left_join(Bubbs.Outlet.Vidette.below.2011.l,Bubbs.Outlet.Vidette.below.2011.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Bubbs.Outlet.Vidette.below.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.Vidette.below.2011<-list(title= "Outlet.Vidette.below.2011", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Bubbs.Outlet.Vidette.below.2011<-Community(Bubbs.Outlet.Vidette.below.2011,Properties.Bubbs.Outlet.Vidette.below.2011, trophic.links=links)
SaveCommunity(Bubbs.Outlet.Vidette.below.2011, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site/Outlet.Vidette.below.2011/",fn='write.csv' )

Bubbs.LS.Outlet.Vidette.below.2011 <- Community(properties = CPS(Bubbs.Outlet.Vidette.below.2011),
                           nodes = Bubbs.Outlet.Vidette.below.2011,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.Vidette.below.2011, level='ChainAveragedTrophicLevel')

#Outlet.Vidette.below.2012
Bubbs.Outlet.Vidette.below.2012.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.Vidette.below.2012")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.Vidette.below.2012<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.Vidette.below.2012")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Bubbs.Outlet.Vidette.below.2012.n<-left_join(Bubbs.Outlet.Vidette.below.2012.o,Bubbs.food.web.Outlet.Vidette.below.2012)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

Bubbs.Outlet.Vidette.below.2012.l<-left_join(Bubbs.Outlet.Vidette.below.2012.o,Bubbs.food.web.Outlet.Vidette.below.2012)%>%
  group_by(node)%>%
  summarise(density=sum(value))

Bubbs.Outlet.Vidette.below.2012<-left_join(Bubbs.Outlet.Vidette.below.2012.l,Bubbs.Outlet.Vidette.below.2012.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Bubbs.Outlet.Vidette.below.2012,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.Vidette.below.2012<-list(title= "Outlet.Vidette.below.2012", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Bubbs.Outlet.Vidette.below.2012<-Community(Bubbs.Outlet.Vidette.below.2012,Properties.Bubbs.Outlet.Vidette.below.2012, trophic.links=links)
SaveCommunity(Bubbs.Outlet.Vidette.below.2012, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site/Outlet.Vidette.below.2012/",fn='write.csv' )

Bubbs.LS.Outlet.Vidette.below.2012 <- Community(properties = CPS(Bubbs.Outlet.Vidette.below.2012),
                           nodes = Bubbs.Outlet.Vidette.below.2012,
                           trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.Vidette.below.2012, level='ChainAveragedTrophicLevel')





Bubbs.Outlet.10487.trt.2011.o<-food.web%>%
  filter(O.NET=="BUBBS" & Site=="Outlet.10487.trt.2011")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Bubbs.food.web.Outlet.10487.trt.2011<-food.web%>%
  filter(O.NET=="BUBBS"& Site=="Outlet.10487.trt.2011")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

Bubbs.Outlet.10487.trt.2011.n<-left_join(Bubbs.Outlet.10487.trt.2011.o,Bubbs.food.web.Outlet.10487.trt.2011)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

Bubbs.Outlet.10487.trt.2011.l<-left_join(Bubbs.Outlet.10487.trt.2011.o,Bubbs.food.web.Outlet.10487.trt.2011)%>%
  group_by(node)%>%
  summarise(density=sum(value))

Bubbs.Outlet.10487.trt.2011<-left_join(Bubbs.Outlet.10487.trt.2011.l,Bubbs.Outlet.10487.trt.2011.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Bubbs.Outlet.10487.trt.2011,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Bubbs.Outlet.10487.trt.2011<-list(title= "Outlet.10487.trt.2011", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
Bubbs.Outlet.10487.trt.2011<-Community(Bubbs.Outlet.10487.trt.2011,Properties.Bubbs.Outlet.10487.trt.2011, trophic.links=links)
SaveCommunity(Bubbs.Outlet.10487.trt.2011, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site.Order/communities/Outlet.10487.trt.2011/",fn='write.csv' )

Bubbs.LS.Outlet.10487.trt.2011 <- Community(properties = CPS(Bubbs.Outlet.10487.trt.2011),
                                            nodes = Bubbs.Outlet.10487.trt.2011,
                                            trophic.links = links)

PlotWebByLevel(Bubbs.Outlet.10487.trt.2011, level='ChainAveragedTrophicLevel')

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

temp.path <-"~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/BUBBS/BUBBS.sites.order/"
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