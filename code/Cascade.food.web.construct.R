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
#Cascade Sites Individually

#Cascade CLS1_1
Cascade.CLS1_1<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS1_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS1_1<-Cascade.CLS1_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS1_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS1_1<-list(title= "CLS1_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS1_1<-Community(Cascade.food.web.CLS1_1,Properties.Casc.CLS1_1, trophic.links=links)
SaveCommunity(Cascade.CLS1_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS1_1/",fn='write.csv' )

Cascades.LS.CLS1_1 <- Community(properties = CPS(Cascade.CLS1_1),
                                nodes = Cascade.food.web.CLS1_1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS1_1, level='ChainAveragedTrophicLevel')

#CLS1_3
Cascade.CLS1_3<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS1_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS1_3<-Cascade.CLS1_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS1_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS1_3<-list(title= "CLS1_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS1_3<-Community(Cascade.food.web.CLS1_3,Properties.Casc.CLS1_3, trophic.links=links)
SaveCommunity(Cascade.CLS1_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS1_3/",fn='write.csv' )

Cascades.LS.CLS1_3 <- Community(properties = CPS(Cascade.CLS1_3),
                                nodes = Cascade.food.web.CLS1_3,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS1_3, level='ChainAveragedTrophicLevel')


#CLS2_1
Cascade.CLS2_1<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS2_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS2_1<-Cascade.CLS2_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS2_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS2_1<-list(title= "CLS2_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS2_1<-Community(Cascade.food.web.CLS2_1,Properties.Casc.CLS2_1, trophic.links=links)
SaveCommunity(Cascade.CLS2_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS2_1/",fn='write.csv' )

Cascades.LS.CLS2_1 <- Community(properties = CPS(Cascade.CLS2_1),
                                nodes = Cascade.food.web.CLS2_1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS2_1, level='ChainAveragedTrophicLevel')

#CLS2_3
Cascade.CLS2_3<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS2_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS2_3<-Cascade.CLS2_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS2_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS2_3<-list(title= "CLS2_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS2_3<-Community(Cascade.food.web.CLS2_3,Properties.Casc.CLS2_3, trophic.links=links)
SaveCommunity(Cascade.CLS2_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS2_3/",fn='write.csv' )

Cascades.LS.CLS2_3 <- Community(properties = CPS(Cascade.CLS2_3),
                                nodes = Cascade.food.web.CLS2_3,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS2_3, level='ChainAveragedTrophicLevel')


#CLS3_1
Cascade.CLS3_1<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS3_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS3_1<-Cascade.CLS3_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS3_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS3_1<-list(title= "CLS3_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS3_1<-Community(Cascade.food.web.CLS3_1,Properties.Casc.CLS3_1, trophic.links=links)
SaveCommunity(Cascade.CLS3_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS3_1/",fn='write.csv' )

Cascades.LS.CLS3_1 <- Community(properties = CPS(Cascade.CLS3_1),
                                nodes = Cascade.food.web.CLS3_1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS3_1, level='ChainAveragedTrophicLevel')

#CLS3_2
Cascade.CLS3_2<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS3_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS3_2<-Cascade.CLS3_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS3_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS3_2<-list(title= "CLS3_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS3_2<-Community(Cascade.food.web.CLS3_2,Properties.Casc.CLS3_2, trophic.links=links)
SaveCommunity(Cascade.CLS3_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS3_2/",fn='write.csv' )

Cascades.LS.CLS3_2 <- Community(properties = CPS(Cascade.CLS3_2),
                                nodes = Cascade.food.web.CLS3_2,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS3_2, level='ChainAveragedTrophicLevel')


#CLS3_3
Cascade.CLS3_3<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS3_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS3_3<-Cascade.CLS3_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS3_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS3_3<-list(title= "CLS3_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS3_3<-Community(Cascade.food.web.CLS3_3,Properties.Casc.CLS3_3, trophic.links=links)
SaveCommunity(Cascade.CLS3_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS3_3/",fn='write.csv' )

Cascades.LS.CLS3_3 <- Community(properties = CPS(Cascade.CLS3_3),
                                nodes = Cascade.food.web.CLS3_3,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS3_3, level='ChainAveragedTrophicLevel')

#CLS4_1
Cascade.CLS4_1<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS4_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS4_1<-Cascade.CLS4_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS4_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS4_1<-list(title= "CLS4_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS4_1<-Community(Cascade.food.web.CLS4_1,Properties.Casc.CLS4_1, trophic.links=links)
SaveCommunity(Cascade.CLS4_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS4_1/",fn='write.csv' )

Cascades.LS.CLS4_1 <- Community(properties = CPS(Cascade.CLS4_1),
                                nodes = Cascade.food.web.CLS4_1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS4_1, level='ChainAveragedTrophicLevel')

#CLS4_2
Cascade.CLS4_2<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS4_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS4_2<-Cascade.CLS4_2%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS4_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS4_2<-list(title= "CLS4_2", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS4_2<-Community(Cascade.food.web.CLS4_2,Properties.Casc.CLS4_2, trophic.links=links)
SaveCommunity(Cascade.CLS4_2, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS4_2/",fn='write.csv' )

Cascades.LS.CLS4_2 <- Community(properties = CPS(Cascade.CLS4_2),
                                nodes = Cascade.food.web.CLS4_2,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS4_2, level='ChainAveragedTrophicLevel')

#CLS4_3
Cascade.CLS4_3<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS4_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS4_3<-Cascade.CLS4_3%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS4_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS4_3<-list(title= "CLS4_3", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS4_3<-Community(Cascade.food.web.CLS4_3,Properties.Casc.CLS4_3, trophic.links=links)
SaveCommunity(Cascade.CLS4_3, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS4_3/",fn='write.csv' )

Cascades.LS.CLS4_3 <- Community(properties = CPS(Cascade.CLS4_3),
                                nodes = Cascade.food.web.CLS4_3,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS4_3, level='ChainAveragedTrophicLevel')

#CLS5_1
Cascade.CLS5_1<-food.webz%>%
  filter(Network=="CASCADE" & Site=="CLS5_1 ")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

Cascade.food.web.CLS5_1<-Cascade.CLS5_1%>%
  dplyr::select(c(N,M, phylum ,class, order, family, genus, node))


#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(Cascade.food.web.CLS5_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.CLS5_1<-list(title= "CLS5_1", project="Sierra Nevada Lake-Stream Nets", M.units="m^2", N.units="mg")
Cascade.CLS5_1<-Community(Cascade.food.web.CLS5_1,Properties.Casc.CLS5_1, trophic.links=links)
SaveCommunity(Cascade.CLS5_1, dir ="~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/CLS5_1/",fn='write.csv' )

Cascades.LS.CLS5_1 <- Community(properties = CPS(Cascade.CLS5_1),
                                nodes = Cascade.food.web.CLS5_1,
                                trophic.links = links)

PlotWebByLevel(Cascade.CLS5_1, level='ChainAveragedTrophicLevel')


#Done with Cascade LS food webs wohooooo!
################################################################################################################################################################################################
#COmbine three local food webs into communtiy colelction

Cascade.order<-CommunityCollection(list(Cascades.LS.CLS1_1,Cascades.LS.CLS1_3,Cascades.LS.CLS2_1,Cascades.LS.CLS2_3,Cascades.LS.CLS3_1,Cascades.LS.CLS3_2,Cascades.LS.CLS3_3,Cascades.LS.CLS4_1,Cascades.LS.CLS4_2,Cascades.LS.CLS4_3,Cascades.LS.CLS5_1))

CollectionCPS(Cascading,  properties=NULL)

dads<-CollectionCPS(Cascading, 
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

temp.path <-"~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/Sites/Cascade/Cascade.sites.order/"
SaveCollection(Cascade.order, temp.path)
Cascade.order.loaded<-LoadCollection(temp.path)

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
  lm(substitute(dads$T~i, list(i = as.name(y))), data = cascade.env)
})
lapply(models, summary)





CollectionNPS(Nets, properties=NULL)

CollectionTLPS(Nets, node.properties=NULL, link.properties=NULL)

###################################################################