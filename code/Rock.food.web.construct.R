#Local.Food.Webs Sierra Nevada
library(tidyverse)
library(cheddar)
##################################################################################################################################################################################################################
setwd("~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/data")

registry<-read.csv('newest.registry.csv')

food.web<-read.csv("pivoted.sp.data.csv")
summary(food.web)

fn.trait<-read.csv("Full_fn_trait.csv")
summary(food.web)
fn.traits<-fn.trait%>%select(c(Taxonomic_name,M))#%>%rename("Taxonomic_name"="Full_List_Taxa")
food.webz<-left_join(fn.traits,food.web, by="Taxonomic_name")
  
################################################################################################################################################################
#Rock RCLS1.1
ROCK.RCLS1.1<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS1.1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS1.1<-ROCK.RCLS1.1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS1.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS1.1<-list(title= "RCLS1.1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
ROCK.RCLS1.1<-Community(ROCKs.RCLS1.1,Properties.Casc.RCLS1.1, trophic.links=links)
SaveCommunity(ROCK.RCLS1.1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS1.1/",fn='write.csv' )

ROCKs.LS.RCLS1.1 <- Community(properties = CPS(ROCK.RCLS1.1),
                              nodes = ROCKs.RCLS1.1,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS1.1, level='ChainAveragedTrophicLevel')

#ROCK RCLS2.1
ROCK.RCLS2.1<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS2.1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS2.1<-ROCK.RCLS2.1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS2.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS2.1<-list(title= "RCLS2.1", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS2.1<-Community(ROCKs.RCLS2.1,Properties.Casc.RCLS2.1, trophic.links=links)
SaveCommunity(ROCK.RCLS2.1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS2.1/",fn='write.csv' )

ROCKs.LS.RCLS2.1 <- Community(properties = CPS(ROCK.RCLS2.1),
                              nodes = ROCKs.RCLS2.1,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS2.1, level='ChainAveragedTrophicLevel')


#ROCK RCLS2.2
ROCK.RCLS2.2<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS2.2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS2.2<-ROCK.RCLS2.2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS2.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS2.2<-list(title= "RCLS2.2", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS2.2<-Community(ROCKs.RCLS2.2,Properties.Casc.RCLS2.2, trophic.links=links)
SaveCommunity(ROCK.RCLS2.2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS2.2/",fn='write.csv' )

ROCKs.LS.RCLS2.2 <- Community(properties = CPS(ROCK.RCLS2.2),
                              nodes = ROCKs.RCLS2.2,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS2.2, level='ChainAveragedTrophicLevel')

#ROCK RCLS2.3
ROCK.RCLS2.3<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS2.3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS2.3<-ROCK.RCLS2.3%>%
  dplyr::select(c(N, M,phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS2.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS2.3<-list(title= "RCLS2.3", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS2.3<-Community(ROCKs.RCLS2.3,Properties.Casc.RCLS2.3, trophic.links=links)
SaveCommunity(ROCK.RCLS2.3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS2.3/",fn='write.csv' )

ROCKs.LS.RCLS2.3 <- Community(properties = CPS(ROCK.RCLS2.3),
                              nodes = ROCKs.RCLS2.3,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS2.3, level='ChainAveragedTrophicLevel')

#ROCK RCLS2.4
ROCK.RCLS2.4<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS2.4")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS2.4<-ROCK.RCLS2.4%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS2.4,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS2.4<-list(title= "RCLS2.4", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS2.4<-Community(ROCKs.RCLS2.4,Properties.Casc.RCLS2.4, trophic.links=links)
SaveCommunity(ROCK.RCLS2.4, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS2.4/",fn='write.csv' )

ROCKs.LS.RCLS2.4 <- Community(properties = CPS(ROCK.RCLS2.4),
                              nodes = ROCKs.RCLS2.4,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS2.4, level='ChainAveragedTrophicLevel')

#ROCK RCLS2.5
ROCK.RCLS2.5<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS2.5")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS2.5<-ROCK.RCLS2.5%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS2.5,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS2.5<-list(title= "RCLS2.5", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS2.5<-Community(ROCKs.RCLS2.5,Properties.Casc.RCLS2.5, trophic.links=links)
SaveCommunity(ROCK.RCLS2.5, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS2.5/",fn='write.csv' )

ROCKs.LS.RCLS2.5 <- Community(properties = CPS(ROCK.RCLS2.5),
                              nodes = ROCKs.RCLS2.5,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS2.5, level='ChainAveragedTrophicLevel')

#ROCK RCLS2.6
ROCK.RCLS2.6<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS2.6")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS2.6<-ROCK.RCLS2.6%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS2.6,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS2.6<-list(title= "RCLS2.6", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS2.6<-Community(ROCKs.RCLS2.6,Properties.Casc.RCLS2.6, trophic.links=links)
SaveCommunity(ROCK.RCLS2.6, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS2.6/",fn='write.csv' )

ROCKs.LS.RCLS2.6 <- Community(properties = CPS(ROCK.RCLS2.6),
                              nodes = ROCKs.RCLS2.6,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS2.6, level='ChainAveragedTrophicLevel')

#ROCK RCLS3.1
ROCK.RCLS3.1<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS3.1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS3.1<-ROCK.RCLS3.1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS3.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS3.1<-list(title= "RCLS3.1", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS3.1<-Community(ROCKs.RCLS3.1,Properties.Casc.RCLS3.1, trophic.links=links)
SaveCommunity(ROCK.RCLS3.1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS3.1/",fn='write.csv' )

ROCKs.LS.RCLS3.1 <- Community(properties = CPS(ROCK.RCLS3.1),
                              nodes = ROCKs.RCLS3.1,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS3.1, level='ChainAveragedTrophicLevel')

#ROCK RCLS3.2
ROCK.RCLS3.2<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS3.2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS3.2<-ROCK.RCLS3.2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS3.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS3.2<-list(title= "RCLS3.2", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS3.2<-Community(ROCKs.RCLS3.2,Properties.Casc.RCLS3.2, trophic.links=links)
SaveCommunity(ROCK.RCLS3.2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS3.2/",fn='write.csv' )

ROCKs.LS.RCLS3.2 <- Community(properties = CPS(ROCK.RCLS3.2),
                              nodes = ROCKs.RCLS3.2,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS3.2, level='ChainAveragedTrophicLevel')

#ROCK RCLS3.3
ROCK.RCLS3.3<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS3.3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS3.3<-ROCK.RCLS3.3%>%
  dplyr::select(c(N, M,phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS3.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS3.3<-list(title= "RCLS3.3", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS3.3<-Community(ROCKs.RCLS3.3,Properties.Casc.RCLS3.3, trophic.links=links)
SaveCommunity(ROCK.RCLS3.3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS3.3/",fn='write.csv' )

ROCKs.LS.RCLS3.3 <- Community(properties = CPS(ROCK.RCLS3.3),
                              nodes = ROCKs.RCLS3.3,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS3.3, level='ChainAveragedTrophicLevel')

#ROCK RCLS3.4
ROCK.RCLS3.4<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS3.4")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS3.4<-ROCK.RCLS3.4%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS3.4,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS3.4<-list(title= "RCLS3.4", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS3.4<-Community(ROCKs.RCLS3.4,Properties.Casc.RCLS3.4, trophic.links=links)
SaveCommunity(ROCK.RCLS3.4, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS3.4/",fn='write.csv' )

ROCKs.LS.RCLS3.4 <- Community(properties = CPS(ROCK.RCLS3.4),
                              nodes = ROCKs.RCLS3.4,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS3.4, level='ChainAveragedTrophicLevel')

#ROCK RCLS3.5
ROCK.RCLS3.5<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS3.5")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS3.5<-ROCK.RCLS3.5%>%
  dplyr::select(c(N, M,phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS3.5,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS3.5<-list(title= "RCLS3.5", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS3.5<-Community(ROCKs.RCLS3.5,Properties.Casc.RCLS3.5, trophic.links=links)
SaveCommunity(ROCK.RCLS3.5, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS3.5/",fn='write.csv' )

ROCKs.LS.RCLS3.5 <- Community(properties = CPS(ROCK.RCLS3.5),
                              nodes = ROCKs.RCLS3.5,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS3.5, level='ChainAveragedTrophicLevel')


#ROCK RCLS4.1
ROCK.RCLS4.1<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS4.1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS4.1<-ROCK.RCLS4.1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS4.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS4.1<-list(title= "RCLS4.1", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS4.1<-Community(ROCKs.RCLS4.1,Properties.Casc.RCLS4.1, trophic.links=links)
SaveCommunity(ROCK.RCLS4.1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS4.1/",fn='write.csv' )

ROCKs.LS.RCLS4.1 <- Community(properties = CPS(ROCK.RCLS4.1),
                              nodes = ROCKs.RCLS4.1,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS4.1, level='ChainAveragedTrophicLevel')


#ROCK RCLS4.2
ROCK.RCLS4.2<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS4.2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS4.2<-ROCK.RCLS4.2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS4.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS4.2<-list(title= "RCLS4.2", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS4.2<-Community(ROCKs.RCLS4.2,Properties.Casc.RCLS4.2, trophic.links=links)
SaveCommunity(ROCK.RCLS4.2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS4.2/",fn='write.csv' )

ROCKs.LS.RCLS4.2 <- Community(properties = CPS(ROCK.RCLS4.2),
                              nodes = ROCKs.RCLS4.2,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS4.2, level='ChainAveragedTrophicLevel')

#ROCK RCLS4.3
ROCK.RCLS4.3<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS4.3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS4.3<-ROCK.RCLS4.3%>%
  dplyr::select(c(N, M,phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS4.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS4.3<-list(title= "RCLS4.3", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS4.3<-Community(ROCKs.RCLS4.3,Properties.Casc.RCLS4.3, trophic.links=links)
SaveCommunity(ROCK.RCLS4.3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS4.3/",fn='write.csv' )

ROCKs.LS.RCLS4.3 <- Community(properties = CPS(ROCK.RCLS4.3),
                              nodes = ROCKs.RCLS4.3,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS4.3, level='ChainAveragedTrophicLevel')

#ROCK RCLS5.1
ROCK.RCLS5.1<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS5.1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS5.1<-ROCK.RCLS5.1%>%
  dplyr::select(c(N, M,phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS5.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS5.1<-list(title= "RCLS5.1", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS5.1<-Community(ROCKs.RCLS5.1,Properties.Casc.RCLS5.1, trophic.links=links)
SaveCommunity(ROCK.RCLS5.1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS5.1/",fn='write.csv' )

ROCKs.LS.RCLS5.1 <- Community(properties = CPS(ROCK.RCLS5.1),
                              nodes = ROCKs.RCLS5.1,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS5.1, level='ChainAveragedTrophicLevel')

#ROCK RCLS5.2
ROCK.RCLS5.2<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS5.2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS5.2<-ROCK.RCLS5.2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS5.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS5.2<-list(title= "RCLS5.2", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS5.2<-Community(ROCKs.RCLS5.2,Properties.Casc.RCLS5.2, trophic.links=links)
SaveCommunity(ROCK.RCLS5.2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS5.2/",fn='write.csv' )

ROCKs.LS.RCLS5.2 <- Community(properties = CPS(ROCK.RCLS5.2),
                              nodes = ROCKs.RCLS5.2,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS5.2, level='ChainAveragedTrophicLevel')

#ROCK RCLS5.3
ROCK.RCLS5.3<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS5.3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS5.3<-ROCK.RCLS5.3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS5.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS5.3<-list(title= "RCLS5.3", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS5.3<-Community(ROCKs.RCLS5.3,Properties.Casc.RCLS5.3, trophic.links=links)
SaveCommunity(ROCK.RCLS5.3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS5.3/",fn='write.csv' )

ROCKs.LS.RCLS5.3 <- Community(properties = CPS(ROCK.RCLS5.3),
                              nodes = ROCKs.RCLS5.3,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS5.3, level='ChainAveragedTrophicLevel')

#ROCK RCLS6.1
ROCK.RCLS6.1<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS6.1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS6.1<-ROCK.RCLS6.1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS6.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS6.1<-list(title= "RCLS6.1", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS6.1<-Community(ROCKs.RCLS6.1,Properties.Casc.RCLS6.1, trophic.links=links)
SaveCommunity(ROCK.RCLS6.1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS6.1/",fn='write.csv' )

ROCKs.LS.RCLS6.1 <- Community(properties = CPS(ROCK.RCLS6.1),
                              nodes = ROCKs.RCLS6.1,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS6.1, level='ChainAveragedTrophicLevel')

#ROCK RCLS6.2
ROCK.RCLS6.2<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS6.2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS6.2<-ROCK.RCLS6.2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS6.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS6.2<-list(title= "RCLS6.2", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS6.2<-Community(ROCKs.RCLS6.2,Properties.Casc.RCLS6.2, trophic.links=links)
SaveCommunity(ROCK.RCLS6.2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS6.2/",fn='write.csv' )

ROCKs.LS.RCLS6.2 <- Community(properties = CPS(ROCK.RCLS6.2),
                              nodes = ROCKs.RCLS6.2,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS6.2, level='ChainAveragedTrophicLevel')

#ROCK RCLS6.3
ROCK.RCLS6.3<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS6.3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS6.3<-ROCK.RCLS6.3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS6.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS6.3<-list(title= "RCLS6.3", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS6.3<-Community(ROCKs.RCLS6.3,Properties.Casc.RCLS6.3, trophic.links=links)
SaveCommunity(ROCK.RCLS6.3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS6.3/",fn='write.csv' )

ROCKs.LS.RCLS6.3 <- Community(properties = CPS(ROCK.RCLS6.3),
                              nodes = ROCKs.RCLS6.3,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS6.3, level='ChainAveragedTrophicLevel')

#ROCK RCLS6.4
ROCK.RCLS6.4<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS6.4")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS6.4<-ROCK.RCLS6.4%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS6.4,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS6.4<-list(title= "RCLS6.4", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS6.4<-Community(ROCKs.RCLS6.4,Properties.Casc.RCLS6.4, trophic.links=links)
SaveCommunity(ROCK.RCLS6.4, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS6.4/",fn='write.csv' )

ROCKs.LS.RCLS6.4 <- Community(properties = CPS(ROCK.RCLS6.4),
                              nodes = ROCKs.RCLS6.4,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS6.4, level='ChainAveragedTrophicLevel')

#ROCK RCLS6.5
ROCK.RCLS6.5<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS6.5")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS6.5<-ROCK.RCLS6.5%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS6.5,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS6.5<-list(title= "RCLS6.5", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS6.5<-Community(ROCKs.RCLS6.5,Properties.Casc.RCLS6.5, trophic.links=links)
SaveCommunity(ROCK.RCLS6.5, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS6.5/",fn='write.csv' )

ROCKs.LS.RCLS6.5 <- Community(properties = CPS(ROCK.RCLS6.5),
                              nodes = ROCKs.RCLS6.5,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS6.5, level='ChainAveragedTrophicLevel')


#ROCK RCLS7.1
ROCK.RCLS7.1<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS7.1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS7.1<-ROCK.RCLS7.1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS7.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS7.1<-list(title= "RCLS7.1", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS7.1<-Community(ROCKs.RCLS7.1,Properties.Casc.RCLS7.1, trophic.links=links)
SaveCommunity(ROCK.RCLS7.1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS7.1/",fn='write.csv' )

ROCKs.LS.RCLS7.1 <- Community(properties = CPS(ROCK.RCLS7.1),
                              nodes = ROCKs.RCLS7.1,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS7.1, level='ChainAveragedTrophicLevel')


#ROCK RCLS7.2
ROCK.RCLS7.2<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS7.2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS7.2<-ROCK.RCLS7.2%>%
  dplyr::select(c(N, M,phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS7.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS7.2<-list(title= "RCLS7.2", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS7.2<-Community(ROCKs.RCLS7.2,Properties.Casc.RCLS7.2, trophic.links=links)
SaveCommunity(ROCK.RCLS7.2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS7.2/",fn='write.csv' )

ROCKs.LS.RCLS7.2 <- Community(properties = CPS(ROCK.RCLS7.2),
                              nodes = ROCKs.RCLS7.2,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS7.2, level='ChainAveragedTrophicLevel')

#ROCK RCLS7.3
ROCK.RCLS7.3<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS7.3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS7.3<-ROCK.RCLS7.3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS7.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS7.3<-list(title= "RCLS7.3", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS7.3<-Community(ROCKs.RCLS7.3,Properties.Casc.RCLS7.3, trophic.links=links)
SaveCommunity(ROCK.RCLS7.3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS7.3/",fn='write.csv' )

ROCKs.LS.RCLS7.3 <- Community(properties = CPS(ROCK.RCLS7.3),
                              nodes = ROCKs.RCLS7.3,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS7.3, level='ChainAveragedTrophicLevel')

#ROCK RCLS7.4
ROCK.RCLS7.4<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS7.4")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS7.4<-ROCK.RCLS7.4%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS7.4,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS7.4<-list(title= "RCLS7.4", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS7.4<-Community(ROCKs.RCLS7.4,Properties.Casc.RCLS7.4, trophic.links=links)
SaveCommunity(ROCK.RCLS7.4, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS7.4/",fn='write.csv' )

ROCKs.LS.RCLS7.4 <- Community(properties = CPS(ROCK.RCLS7.4),
                              nodes = ROCKs.RCLS7.4,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS7.4, level='ChainAveragedTrophicLevel')

#ROCK RCLS7.5
ROCK.RCLS7.5<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS7.5")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS7.5<-ROCK.RCLS7.5%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS7.5,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS7.5<-list(title= "RCLS7.5", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS7.5<-Community(ROCKs.RCLS7.5,Properties.Casc.RCLS7.5, trophic.links=links)
SaveCommunity(ROCK.RCLS7.5, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS7.5/",fn='write.csv' )

ROCKs.LS.RCLS7.5 <- Community(properties = CPS(ROCK.RCLS7.5),
                              nodes = ROCKs.RCLS7.5,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS7.5, level='ChainAveragedTrophicLevel')

#ROCK RCLS7.6
ROCK.RCLS7.6<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS7.6")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS7.6<-ROCK.RCLS7.6%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS7.6,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS7.6<-list(title= "RCLS7.6", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS7.6<-Community(ROCKs.RCLS7.6,Properties.Casc.RCLS7.6, trophic.links=links)
SaveCommunity(ROCK.RCLS7.6, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS7.6/",fn='write.csv' )

ROCKs.LS.RCLS7.6 <- Community(properties = CPS(ROCK.RCLS7.6),
                              nodes = ROCKs.RCLS7.6,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS7.6, level='ChainAveragedTrophicLevel')

#ROCK RCLS7.7
ROCK.RCLS7.7<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS7.7")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS7.7<-ROCK.RCLS7.7%>%
  dplyr::select(c(N, M,phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS7.7,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS7.7<-list(title= "RCLS7.7", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS7.7<-Community(ROCKs.RCLS7.7,Properties.Casc.RCLS7.7, trophic.links=links)
SaveCommunity(ROCK.RCLS7.7, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS7.7/",fn='write.csv' )

ROCKs.LS.RCLS7.7 <- Community(properties = CPS(ROCK.RCLS7.7),
                              nodes = ROCKs.RCLS7.7,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS7.7, level='ChainAveragedTrophicLevel')

#ROCK RCLS8.1
ROCK.RCLS8.1<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS8.1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS8.1<-ROCK.RCLS8.1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS8.1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS8.1<-list(title= "RCLS8.1", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS8.1<-Community(ROCKs.RCLS8.1,Properties.Casc.RCLS8.1, trophic.links=links)
SaveCommunity(ROCK.RCLS8.1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS8.1/",fn='write.csv' )

ROCKs.LS.RCLS8.1 <- Community(properties = CPS(ROCK.RCLS8.1),
                              nodes = ROCKs.RCLS8.1,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS8.1, level='ChainAveragedTrophicLevel')

#ROCK RCLS8.2
ROCK.RCLS8.2<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS8.2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS8.2<-ROCK.RCLS8.2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS8.2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS8.2<-list(title= "RCLS8.2", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS8.2<-Community(ROCKs.RCLS8.2,Properties.Casc.RCLS8.2, trophic.links=links)
SaveCommunity(ROCK.RCLS8.2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS8.2/",fn='write.csv' )

ROCKs.LS.RCLS8.2 <- Community(properties = CPS(ROCK.RCLS8.2),
                              nodes = ROCKs.RCLS8.2,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS8.2, level='ChainAveragedTrophicLevel')

#ROCK RCLS8.3
ROCK.RCLS8.3<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS8.3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS8.3<-ROCK.RCLS8.3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS8.3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS8.3<-list(title= "RCLS8.3", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS8.3<-Community(ROCKs.RCLS8.3,Properties.Casc.RCLS8.3, trophic.links=links)
SaveCommunity(ROCK.RCLS8.3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS8.3/",fn='write.csv' )

ROCKs.LS.RCLS8.3 <- Community(properties = CPS(ROCK.RCLS8.3),
                              nodes = ROCKs.RCLS8.3,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS8.3, level='ChainAveragedTrophicLevel')

#ROCK RCLS8.4
ROCK.RCLS8.4<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS8.4")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS8.4<-ROCK.RCLS8.4%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family" 
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS8.4,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS8.4<-list(title= "RCLS8.4", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS8.4<-Community(ROCKs.RCLS8.4,Properties.Casc.RCLS8.4, trophic.links=links)
SaveCommunity(ROCK.RCLS8.4, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS8.4/",fn='write.csv' )

ROCKs.LS.RCLS8.4 <- Community(properties = CPS(ROCK.RCLS8.4),
                              nodes = ROCKs.RCLS8.4,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS8.4, level='ChainAveragedTrophicLevel')


#ROCK RCLS8.5
ROCK.RCLS8.5<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS8.5")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS8.5<-ROCK.RCLS8.5%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS8.5,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS8.5<-list(title= "RCLS8.5", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS8.5<-Community(ROCKs.RCLS8.5,Properties.Casc.RCLS8.5, trophic.links=links)
SaveCommunity(ROCK.RCLS8.5, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS8.5/",fn='write.csv' )

ROCKs.LS.RCLS8.5 <- Community(properties = CPS(ROCK.RCLS8.5),
                              nodes = ROCKs.RCLS8.5,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS8.5, level='ChainAveragedTrophicLevel')

#ROCK RCLS8.7
ROCK.RCLS8.7<-food.webz%>%
  filter(Network=="ROCK" & Site=="RCLS8.7")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

ROCKs.RCLS8.7<-ROCK.RCLS8.7%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(ROCKs.RCLS8.7,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Properties.Casc.RCLS8.7<-list(title= "RCLS8.7", project="Sierra Nevada Lake-Stream Nets",  M.units="m^2", N.units="mg")
ROCK.RCLS8.7<-Community(ROCKs.RCLS8.7,Properties.Casc.RCLS8.7, trophic.links=links)
SaveCommunity(ROCK.RCLS8.7, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/RCLS8.7/",fn='write.csv' )

ROCKs.LS.RCLS8.7 <- Community(properties = CPS(ROCK.RCLS8.7),
                              nodes = ROCKs.RCLS8.7,
                              trophic.links = links)

PlotWebByLevel(ROCK.RCLS8.7, level='ChainAveragedTrophicLevel')

#Done with ROCK LS food webs wohooooo!
################################################################################################################################################################################################
#COmbine three local food webs into communtiy colelction
PlotNPS(ROCKs.LS.RCLS8.1, 'Log10M', 'Log10N', show.web=FALSE, highlight.nodes=NULL)
PlotNPS(ROCKs.LS.RCLS8.3, 'M', 'ChainAveragedTrophicLevel')

par(mfrow=c(2,2))
PlotMvN(ROCKs.LS.RCLS8.1)
PlotNvM(ROCKs.LS.RCLS8.1)
PlotBvM(ROCKs.LS.RCLS8.1)
PlotMvB(ROCKs.LS.RCLS8.1)

PlotNPyramid(ROCKs.LS.RCLS8.1)
PlotNPyramid(ROCKs.LS.RCLS8.2)
PlotNPyramid(ROCKs.LS.RCLS8.5)
PlotNPyramid(ROCKs.LS.RCLS8.7)

PlotBPyramid(ROCKs.LS.RCLS8.1)
PlotBPyramid(ROCKs.LS.RCLS8.2)
PlotBPyramid(ROCKs.LS.RCLS8.5)
PlotBPyramid(ROCKs.LS.RCLS8.7)

PlotBPyramid(ROCKs.LS.RCLS2.7)
PlotBPyramid(ROCKs.LS.RCLS2.6)
PlotBPyramid(ROCKs.LS.RCLS2.3)
PlotBPyramid(ROCKs.LS.RCLS2.4)

ROCKing<-CommunityCollection(list(ROCK.RCLS1.1,ROCK.RCLS2.1,ROCK.RCLS2.2,ROCK.RCLS2.3,ROCK.RCLS2.4,ROCK.RCLS2.5,ROCK.RCLS2.6,ROCK.RCLS3.1,ROCK.RCLS3.2,ROCK.RCLS3.3,ROCK.RCLS3.4,ROCK.RCLS3.5,ROCK.RCLS4.1,ROCK.RCLS4.2,ROCK.RCLS4.3,ROCK.RCLS5.1,ROCK.RCLS5.2,ROCK.RCLS5.3,ROCK.RCLS6.1,ROCK.RCLS6.2,ROCK.RCLS6.3,ROCK.RCLS6.4,ROCK.RCLS6.5,ROCK.RCLS7.1,ROCK.RCLS7.2,ROCK.RCLS7.3,ROCK.RCLS7.4,ROCK.RCLS7.5,ROCK.RCLS7.6,ROCK.RCLS7.7,ROCK.RCLS8.1,ROCK.RCLS8.2,ROCK.RCLS8.3,ROCK.RCLS8.4,ROCK.RCLS8.5,ROCK.RCLS8.7))

CollectionCPS(ROCKing,  properties=NULL)

ROCK.data<-CollectionCPS(ROCKing, 
                         c('title',
                           S='NumberOfNodes',
                           L='NumberOfTrophicLinks',
                           'L/S'='LinkageN',
                           C='DirectedConnectance',
                           #Slope='NvMSlope',
                           B='FractionBasalNodes',
                           I='FractionIntermediateNodes',
                           T='FractionTopLevelNodes',
                           Isolated='FractionIsolatedNodes'
                         ))

temp.path <- tempfile("~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Rock/Community")
SaveCollection(ROCKing, temp.path)
ROCK.order.loaded<-LoadCollection(temp.path)
