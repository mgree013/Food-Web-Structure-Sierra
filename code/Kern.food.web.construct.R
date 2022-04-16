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
#KERN Sites Individually

#KERN 10029
KERN.10029<-food.web%>%
  filter(O.NET=="KERN" & Site=="10029")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10029<-KERN.10029%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10029,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10029<-list(title= "10029", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10029<-Community(KERN.10029,Properties.KERN.10029, trophic.links=links)
SaveCommunity(KERN.10029, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10029/",fn='write.csv' )
nodes<-as.matrix(nodes)
KERN.LS.10029 <- Community(properties = CPS(KERN.10029),
                                nodes = KERN.10029,
                                trophic.links = links)

PlotWebByLevel(KERN.10029, level='ChainAveragedTrophicLevel')

#10030
KERN.10030<-food.web%>%
  filter(O.NET=="KERN" & Site=="10030")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10030<-food.web%>%
  filter(O.NET=="KERN"& Site=="10030")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10030<-left_join(KERN.10030,KERN.food.web.10030)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERN.10030)<-KERN.10030$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10030,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))


nodes<-as.data.frame(nodes)
Properties.KERN.10030<-list(title= "10030", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10030<-Community(KERN.10030,Properties.KERN.10030, trophic.links=links)
SaveCommunity(KERN.10030, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10030/",fn='write.csv' )

KERN.LS.10030 <- Community(properties = CPS(KERN.10030),
                                nodes = KERN.10030,
                                trophic.links = links)

PlotWebByLevel(KERN.10030, level='ChainAveragedTrophicLevel')


#10031
KERN.10031<-food.web%>%
  filter(O.NET=="KERN" & Site=="10031")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10031<-food.web%>%
  filter(O.NET=="KERN"& Site=="10031")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10031<-left_join(KERN.10031,KERN.food.web.10031)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERN.10031)<-KERN.10031$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10031,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10031<-list(title= "10031", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10031<-Community(KERN.10031,Properties.KERN.10031, trophic.links=links)
SaveCommunity(KERN.10031, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10031/",fn='write.csv' )

KERN.LS.10031 <- Community(properties = CPS(KERN.10031),
                                nodes = KERN.10031,
                                trophic.links = links)

PlotWebByLevel(KERN.10031, level='ChainAveragedTrophicLevel')

#10032
KERN.10032<-food.web%>%
  filter(O.NET=="KERN" & Site=="10032")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10032<-food.web%>%
  filter(O.NET=="KERN"& Site=="10032")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10032<-left_join(KERN.10032,KERN.food.web.10032)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERN.10032)<-KERN.10032$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10032,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10032<-list(title= "10032", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10032<-Community(KERN.10032,Properties.KERN.10032, trophic.links=links)
SaveCommunity(KERN.10032, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10032/",fn='write.csv' )

KERN.LS.10032 <- Community(properties = CPS(KERN.10032),
                                nodes = KERN.10032,
                                trophic.links = links)

PlotWebByLevel(KERN.10032, level='ChainAveragedTrophicLevel')


#10033
KERN.10033<-food.web%>%
  filter(O.NET=="KERN" & Site=="10033")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10033<-food.web%>%
  filter(O.NET=="KERN"& Site=="10033")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10033<-left_join(KERN.10033,KERN.food.web.10033)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERN.10033)<-KERN.10033$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10033,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10033<-list(title= "10033", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10033<-Community(KERN.10033,Properties.KERN.10033, trophic.links=links)
SaveCommunity(KERN.10033, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10033/",fn='write.csv' )

KERN.LS.10033 <- Community(properties = CPS(KERN.10033),
                                nodes = KERN.10033,
                                trophic.links = links)

PlotWebByLevel(KERN.10033, level='ChainAveragedTrophicLevel')

#10034
KERN.10034<-food.web%>%
  filter(O.NET=="KERN" & Site=="10034")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10034<-food.web%>%
  filter(O.NET=="KERN"& Site=="10034")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10034<-left_join(KERN.10034,KERN.food.web.10034)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERN.10034)<-KERN.10034$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10034,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10034<-list(title= "10034", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10034<-Community(KERN.10034,Properties.KERN.10034, trophic.links=links)
SaveCommunity(KERN.10034, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10034/",fn='write.csv' )

KERN.LS.10034 <- Community(properties = CPS(KERN.10034),
                                nodes = KERN.10034,
                                trophic.links = links)

PlotWebByLevel(KERN.10034, level='ChainAveragedTrophicLevel')


#10035
KERN.10035<-food.web%>%
  filter(O.NET=="KERN" & Site=="10035")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10035<-food.web%>%
  filter(O.NET=="KERN"& Site=="10035")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10035<-left_join(KERN.10035,KERN.food.web.10035)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERN.10035)<-KERN.10035$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10035,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10035<-list(title= "10035", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10035<-Community(KERN.10035,Properties.KERN.10035, trophic.links=links)
SaveCommunity(KERN.10035, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10035/",fn='write.csv' )

KERN.LS.10035 <- Community(properties = CPS(KERN.10035),
                                nodes = KERN.10035,
                                trophic.links = links)

PlotWebByLevel(KERN.10035, level='ChainAveragedTrophicLevel')

#10036
KERN.10036<-food.web%>%
  filter(O.NET=="KERN" & Site=="10036")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10036<-food.web%>%
  filter(O.NET=="KERN"& Site=="10036")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10036<-left_join(KERN.10036,KERN.food.web.10036)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERN.10036)<-KERN.10036$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10036,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10036<-list(title= "10036", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10036<-Community(KERN.10036,Properties.KERN.10036, trophic.links=links)
SaveCommunity(KERN.10036, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10036/",fn='write.csv' )

KERN.LS.10036 <- Community(properties = CPS(KERN.10036),
                                nodes = KERN.10036,
                                trophic.links = links)

PlotWebByLevel(KERN.10036, level='ChainAveragedTrophicLevel')

#10037
KERN.10037.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10037")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10037<-food.web%>%
  filter(O.NET=="KERN"& Site=="10037")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10037.n<-left_join(KERN.10037.o,KERN.food.web.10037)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10037.l<-left_join(KERN.10037.o,KERN.food.web.10037)%>%
  group_by(node)%>%
  summarise(density=sum(value))

KERN.10037<-left_join(KERN.10037.l,KERN.10037.n)%>%
  select(density, phylum ,class, order, family, genus, node)

row_names(KERN.10037)<-KERN.10037.n$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10037,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10037<-list(title= "10037", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10037<-Community(KERN.10037,Properties.KERN.10037, trophic.links=links)
SaveCommunity(KERN.10037, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10037/",fn='write.csv' )

KERN.LS.10037 <- Community(properties = CPS(KERN.10037),
                           nodes = KERN.10037,
                           trophic.links = links)

PlotWebByLevel(KERN.10037, level='ChainAveragedTrophicLevel')


#10039
KERN.10039.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10039")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10039<-food.web%>%
  filter(O.NET=="KERN"& Site=="10039")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10039.n<-left_join(KERN.10039.o,KERN.food.web.10039)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10039.l<-left_join(KERN.10039.o,KERN.food.web.10039)%>%
  group_by(node)%>%
  summarise(density=sum(value))
        
KERN.10039<-left_join(KERN.10039.l,KERN.10039.n)%>%
  select(density, phylum ,class, order, family, genus, node)
  
row.names(KERN.10039)<-KERN.10039.n$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10039,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10039<-list(title= "10039", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10039<-Community(KERN.10039,Properties.KERN.10039, trophic.links=links)
SaveCommunity(KERN.10039, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10039/",fn='write.csv' )

KERN.LS.10039 <- Community(properties = CPS(KERN.10039),
                                nodes = KERN.10039,
                                trophic.links = links)

PlotWebByLevel(KERN.10039, level='ChainAveragedTrophicLevel')

#10038
KERN.10038.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10038")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10038<-food.web%>%
  filter(O.NET=="KERN"& Site=="10038")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10038.n<-left_join(KERN.10038.o,KERN.food.web.10038)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10038.l<-left_join(KERN.10038.o,KERN.food.web.10038)%>%
  group_by(node)%>%
  summarise(density=sum(value))

KERN.10038<-left_join(KERN.10038.l,KERN.10038.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10038,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10038<-list(title= "10038", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10038<-Community(KERN.10038,Properties.KERN.10038, trophic.links=links)
SaveCommunity(KERN.10038, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10038/",fn='write.csv' )

KERN.LS.10038 <- Community(properties = CPS(KERN.10038),
                                nodes = KERN.10038,
                                trophic.links = links)

PlotWebByLevel(KERN.10038, level='ChainAveragedTrophicLevel')

#10039
KERN.10039.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10039")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10039<-food.web%>%
  filter(O.NET=="KERN"& Site=="10039")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10039.n<-left_join(KERN.10039.o,KERN.food.web.10039)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10039.l<-left_join(KERN.10039.o,KERN.food.web.10039)%>%
  group_by(node)%>%
  summarise(density=sum(value))

KERN.10039<-left_join(KERN.10039.l,KERN.10039.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10039,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10039<-list(title= "10039", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10039<-Community(KERN.10039,Properties.KERN.10039, trophic.links=links)
SaveCommunity(KERN.10039, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10039/",fn='write.csv' )

KERN.LS.10039 <- Community(properties = CPS(KERN.10039),
                                nodes = KERN.10039,
                                trophic.links = links)

PlotWebByLevel(KERN.10039, level='ChainAveragedTrophicLevel')


#10040
KERN.10040<-food.web%>%
  filter(O.NET=="KERN" & Site=="10040")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10040<-food.web%>%
  filter(O.NET=="KERN"& Site=="10040")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10040<-left_join(KERN.10040,KERN.food.web.10040)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERN.10040)<-KERN.10040$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10040,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10040<-list(title= "10040", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10040<-Community(KERN.10040,Properties.KERN.10040, trophic.links=links)
SaveCommunity(KERN.10040, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10040/",fn='write.csv' )

KERN.LS.10040 <- Community(properties = CPS(KERN.10040),
                            nodes = KERN.10040,
                            trophic.links = links)

PlotWebByLevel(KERN.10040, level='ChainAveragedTrophicLevel')

#10041
KERN.10041<-food.web%>%
  filter(O.NET=="KERN" & Site=="10041")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10041<-food.web%>%
  filter(O.NET=="KERN"& Site=="10041")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERNs.10041<-left_join(KERN.10041,KERN.food.web.10041)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERNs.10041)<-KERN.10041$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERNs.10041,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10041<-list(title= "10041", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10041<-Community(KERNs.10041,Properties.KERN.10041, trophic.links=links)
SaveCommunity(KERN.10041, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10041/",fn='write.csv' )

KERNs.LS.10041 <- Community(properties = CPS(KERN.10041),
                            nodes = KERNs.10041,
                            trophic.links = links)

PlotWebByLevel(KERN.10041, level='ChainAveragedTrophicLevel')

#10042
KERN.10042.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10042")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10042<-food.web%>%
  filter(O.NET=="KERN"& Site=="10042")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10042.n<-left_join(KERN.10042.o,KERN.food.web.10042)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10042.l<-left_join(KERN.10042.o,KERN.food.web.10042)%>%
  group_by(node)%>%
  summarise(density=sum(value))

KERN.10042<-left_join(KERN.10042.l,KERN.10042.n)%>%
  select(density, phylum ,class, order, family, genus, node)

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10042,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10042<-list(title= "10042", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10042<-Community(KERN.10042,Properties.KERN.10042, trophic.links=links)
SaveCommunity(KERN.10042, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10042/",fn='write.csv' )

KERNs.LS.10042 <- Community(properties = CPS(KERN.10042),
                            nodes = KERN.10042,
                            trophic.links = links)

PlotWebByLevel(KERN.10042, level='ChainAveragedTrophicLevel')

#10044
KERN.10044.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10044")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10044<-food.web%>%
  filter(O.NET=="KERN"& Site=="10044")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10044.n<-left_join(KERN.10044.o,KERN.food.web.10044)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10044.l<-left_join(KERN.10044.o,KERN.food.web.10044)%>%
  group_by(node)%>%
  summarise(density=sum(value))

KERN.10044<-left_join(KERN.10044.l,KERN.10044.n)%>%
  select(density, phylum ,class, order, family, genus, node)

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10044,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10044<-list(title= "10044", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10044<-Community(KERN.10044,Properties.KERN.10044, trophic.links=links)
SaveCommunity(KERN.10044, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10044/",fn='write.csv' )

KERNs.LS.10044 <- Community(properties = CPS(KERN.10044),
                            nodes = KERN.10044,
                            trophic.links = links)

PlotWebByLevel(KERN.10044, level='ChainAveragedTrophicLevel')



#10046
KERN.10046<-food.web%>%
  filter(O.NET=="KERN" & Site=="10046")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10046<-food.web%>%
  filter(O.NET=="KERN"& Site=="10046")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERNs.10046<-left_join(KERN.10046,KERN.food.web.10046)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERNs.10046)<-KERN.10046$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERNs.10046,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10046<-list(title= "10046", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10046<-Community(KERNs.10046,Properties.KERN.10046, trophic.links=links)
SaveCommunity(KERN.10046, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10046/",fn='write.csv' )

KERNs.LS.10046 <- Community(properties = CPS(KERN.10046),
                            nodes = KERNs.10046,
                            trophic.links = links)

PlotWebByLevel(KERN.10046, level='ChainAveragedTrophicLevel')

#10047
KERN.10047<-food.web%>%
  filter(O.NET=="KERN" & Site=="10047")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10047<-food.web%>%
  filter(O.NET=="KERN"& Site=="10047")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERNs.10047<-left_join(KERN.10047,KERN.food.web.10047)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERNs.10047)<-KERN.10047$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERNs.10047,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10047<-list(title= "10047", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10047<-Community(KERNs.10047,Properties.KERN.10047, trophic.links=links)
SaveCommunity(KERN.10047, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10047/",fn='write.csv' )

KERNs.LS.10047 <- Community(properties = CPS(KERN.10047),
                            nodes = KERNs.10047,
                            trophic.links = links)

PlotWebByLevel(KERN.10047, level='ChainAveragedTrophicLevel')

#10048
KERN.10048.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10048")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10048<-food.web%>%
  filter(O.NET=="KERN"& Site=="10048")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10048.n<-left_join(KERN.10048.o,KERN.food.web.10048)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10048.l<-left_join(KERN.10048.o,KERN.food.web.10048)%>%
  group_by(node)%>%
  summarise(density=sum(value))

KERN.10048<-left_join(KERN.10048.l,KERN.10048.n)%>%
  select(density, phylum ,class, order, family, genus, node)

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10048,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10048<-list(title= "10048", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10048<-Community(KERN.10048,Properties.KERN.10048, trophic.links=links)
SaveCommunity(KERN.10048, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10048/",fn='write.csv' )

KERNs.LS.10048 <- Community(properties = CPS(KERN.10048),
                            nodes = KERN.10048,
                            trophic.links = links)

PlotWebByLevel(KERN.10048, level='ChainAveragedTrophicLevel')

#10049
KERN.10049.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10049")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10049<-food.web%>%
  filter(O.NET=="KERN"& Site=="10049")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10049.n<-left_join(KERN.10049.o,KERN.food.web.10049)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10049.l<-left_join(KERN.10049.o,KERN.food.web.10049)%>%
  group_by(node)%>%
  summarise(density=sum(value))

KERN.10049<-left_join(KERN.10049.l,KERN.10049.n)%>%
  select(density, phylum ,class, order, family, genus, node)
#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10049,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10049<-list(title= "10049", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10049<-Community(KERN.10049,Properties.KERN.10049, trophic.links=links)
SaveCommunity(KERN.10049, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10049/",fn='write.csv' )

KERNs.LS.10049 <- Community(properties = CPS(KERN.10049),
                            nodes = KERN.10049,
                            trophic.links = links)

PlotWebByLevel(KERN.10049, level='ChainAveragedTrophicLevel')


#10052
KERN.10052.o<-food.web%>%
  filter(O.NET=="KERN" & Site=="10052")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10052<-food.web%>%
  filter(O.NET=="KERN"& Site=="10052")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERN.10052.n<-left_join(KERN.10052.o,KERN.food.web.10052)%>%
  select(density, phylum ,class, order, family, genus, node,Taxonomic_name)%>%
  distinct(node, .keep_all = T)

KERN.10052.l<-left_join(KERN.10052.o,KERN.food.web.10052)%>%
  group_by(node)%>%
  summarise(density=sum(value))

KERN.10052<-left_join(KERN.10052.l,KERN.10052.n)%>%
  select(density, phylum ,class, order, family, genus, node)

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERN.10052,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10052<-list(title= "10052", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10052<-Community(KERN.10052,Properties.KERN.10052, trophic.links=links)
SaveCommunity(KERN.10052, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10052/",fn='write.csv' )

KERNs.LS.10052 <- Community(properties = CPS(KERN.10052),
                            nodes = KERN.10052,
                            trophic.links = links)

PlotWebByLevel(KERN.10052, level='ChainAveragedTrophicLevel')

#10053
KERN.10053<-food.web%>%
  filter(O.NET=="KERN" & Site=="10053")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10053<-food.web%>%
  filter(O.NET=="KERN"& Site=="10053")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERNs.10053<-left_join(KERN.10053,KERN.food.web.10053)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERNs.10053)<-KERN.10053$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERNs.10053,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10053<-list(title= "10053", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10053<-Community(KERNs.10053,Properties.KERN.10053, trophic.links=links)
SaveCommunity(KERN.10053, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10053/",fn='write.csv' )

KERNs.LS.10053 <- Community(properties = CPS(KERN.10053),
                            nodes = KERNs.10053,
                            trophic.links = links)

PlotWebByLevel(KERN.10053, level='ChainAveragedTrophicLevel')

#10054
KERN.10054<-food.web%>%
  filter(O.NET=="KERN" & Site=="10054")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10054<-food.web%>%
  filter(O.NET=="KERN"& Site=="10054")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERNs.10054<-left_join(KERN.10054,KERN.food.web.10054)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERNs.10054)<-KERN.10054$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERNs.10054,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10054<-list(title= "10054", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10054<-Community(KERNs.10054,Properties.KERN.10054, trophic.links=links)
SaveCommunity(KERN.10054, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10054/",fn='write.csv' )

KERNs.LS.10054 <- Community(properties = CPS(KERN.10054),
                            nodes = KERNs.10054,
                            trophic.links = links)

PlotWebByLevel(KERN.10054, level='ChainAveragedTrophicLevel')

#10055
KERN.10055<-food.web%>%
  filter(O.NET=="KERN" & Site=="10055")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10055<-food.web%>%
  filter(O.NET=="KERN"& Site=="10055")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERNs.10055<-left_join(KERN.10055,KERN.food.web.10055)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERNs.10055)<-KERN.10055$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERNs.10055,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10055<-list(title= "10055", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10055<-Community(KERNs.10055,Properties.KERN.10055, trophic.links=links)
SaveCommunity(KERN.10055, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10055/",fn='write.csv' )

KERNs.LS.10055 <- Community(properties = CPS(KERN.10055),
                            nodes = KERNs.10055,
                            trophic.links = links)

PlotWebByLevel(KERN.10055, level='ChainAveragedTrophicLevel')
PlotWebByLevel(KERN.10055, show.nodes.as='labels',
               node.labels=KERN.10055$nodes$node,
               main='Level plot')

#10056
KERN.10056<-food.web%>%
  filter(O.NET=="KERN" & Site=="10056")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

KERN.food.web.10056<-food.web%>%
  filter(O.NET=="KERN"& Site=="10056")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

KERNs.10056<-left_join(KERN.10056,KERN.food.web.10056)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(KERNs.10056)<-KERN.10056$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(KERNs.10056,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.KERN.10056<-list(title= "10056", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
KERN.10056<-Community(KERNs.10056,Properties.KERN.10056, trophic.links=links)
SaveCommunity(KERN.10056, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/Site/10056/",fn='write.csv' )

KERNs.LS.10056 <- Community(properties = CPS(KERN.10056),
                            nodes = KERNs.10056,
                            trophic.links = links)

PlotWebByLevel(KERN.10054, level='ChainAveragedTrophicLevel',colour.by='consumer.category',bg.by='consumer.category',
               symbol.by='consumer.category',pch=19, highlight.nodes=NULL)

PlotCircularWeb(KERN.10056, level='ChainAveragedTrophicLevel',frame.plot = T,colour.by='resolved.to')

PlotWebByLevel(KERN.10046, show.nodes.as='labels',
               node.labels=KERN.10046$nodes$node,
               main='Level plot',frame.plot = T)
#Done with KERN LS food webs wohooooo!
################################################################################################################################################################################################
#COmbine three local food webs into communtiy colelction


CollectionCPS(KERNading,  properties=NULL)

dads<-CollectionCPS(KERNading, 
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

temp.path <-"~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/KERN/KERN.sites.order/"
SaveCollection(KERN.order, temp.path)
KERN.order.loaded<-LoadCollection(temp.path)

envs <-read.csv("stream.env.csv")
KERN.env<-envs%>%filter(O.NET=="KERN")

KERN.all<-cbind(KERN.env,dads)

dev.off()
par(mfrow=c(4,6))
envlist=names(KERN.env)

for(i in c(2:11,15:17,19:25,27,30:32)) # this creates a loop of 23 iterations
{
  plot(KERN.env[,i],dads$C , xlab=envlist[c(i)],ylab="Connectance") 
  test2 = lm(dads$C ~KERN.env[,i])
  abline(test2)
}
varlist<-names(KERN.env)[c(2:11,15:17,19:25,27,30:32)]

models <- lapply(varlist, function(y) {
  lm(substitute(dads$T~i, list(i = as.name(y))), data = KERN.env)
})
lapply(models, summary)





CollectionNPS(Nets, properties=NULL)

CollectionTLPS(Nets, node.properties=NULL, link.properties=NULL)

###################################################################