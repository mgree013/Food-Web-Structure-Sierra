#Local.Food.Webs Sierra Nevada
library(tidyverse )
library(cheddar)
##################################################################################################################################################################################################################
#setwd("~/Users/matthewdouglasgreen/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web.Web/Sierra.Nevada.LS.Food.Web")
registry<-read.csv('newest.registry.csv')

food.web<-read.csv("dog.csv")

################################################################################################################################################################################################################
#RAE Sites Individually
setwd("~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE")

#RAE RLS1_3
RAE.RLS1_3<-food.web%>%
  filter(Network=="RAE" & Site=="RLS1_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS1_3<-food.web%>%
  filter(Network=="RAE"& Site=="RLS1_3")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS1_3<-left_join(RAE.RLS1_3,RAE.food.web.RLS1_3)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS1_3)<-RAE.RLS1_3$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS1_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS1_3<-list(title= "RLS1_3", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS1_3<-Community(RAEs.RLS1_3,Properties.Casc.RLS1_3, trophic.links=links)
SaveCommunity(RAE.RLS1_3, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS1_3/",fn='write.csv' )

RAEs.LS.RLS1_3 <- Community(properties = CPS(RAE.RLS1_3),
                                nodes = RAEs.RLS1_3,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS1_3, level='ChainAveragedTrophicLevel')

#RLS2_1
RAE.RLS2_1<-food.web%>%
  filter(Network=="RAE" & Site=="RLS2_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS2_1<-food.web%>%
  filter(Network=="RAE"& Site=="RLS2_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS2_1<-left_join(RAE.RLS2_1,RAE.food.web.RLS2_1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS2_1)<-RAE.RLS2_1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS2_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS2_1<-list(title= "RLS2_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS2_1<-Community(RAEs.RLS2_1,Properties.Casc.RLS2_1, trophic.links=links)
SaveCommunity(RAE.RLS2_1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS2_1/",fn='write.csv' )

RAEs.LS.RLS2_1 <- Community(properties = CPS(RAE.RLS2_1),
                                nodes = RAEs.RLS2_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS2_1, level='ChainAveragedTrophicLevel')


#RLS3_1
RAE.RLS3_1<-food.web%>%
  filter(Network=="RAE" & Site=="RLS3_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS3_1<-food.web%>%
  filter(Network=="RAE"& Site=="RLS3_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS3_1<-left_join(RAE.RLS3_1,RAE.food.web.RLS3_1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS3_1)<-RAE.RLS3_1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS3_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS3_1<-list(title= "RLS3_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS3_1<-Community(RAEs.RLS3_1,Properties.Casc.RLS3_1, trophic.links=links)
SaveCommunity(RAE.RLS3_1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS3_1/",fn='write.csv' )

RAEs.LS.RLS3_1 <- Community(properties = CPS(RAE.RLS3_1),
                                nodes = RAEs.RLS3_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS3_1, level='ChainAveragedTrophicLevel')

#RLS3_2
RAE.RLS3_2<-food.web%>%
  filter(Network=="RAE" & Site=="RLS3_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS3_2<-food.web%>%
  filter(Network=="RAE"& Site=="RLS3_2")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS3_2<-left_join(RAE.RLS3_2,RAE.food.web.RLS3_2)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS3_2)<-RAE.RLS3_2$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS3_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS3_2<-list(title= "RLS3_2", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS3_2<-Community(RAEs.RLS3_2,Properties.Casc.RLS3_2, trophic.links=links)
SaveCommunity(RAE.RLS3_2, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS3_2/",fn='write.csv' )

RAEs.LS.RLS3_2 <- Community(properties = CPS(RAE.RLS3_2),
                                nodes = RAEs.RLS3_2,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS3_2, level='ChainAveragedTrophicLevel')


#RLS4_1
RAE.RLS4_1<-food.web%>%
  filter(Network=="RAE" & Site=="RLS4_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS4_1<-food.web%>%
  filter(Network=="RAE"& Site=="RLS4_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS4_1<-left_join(RAE.RLS4_1,RAE.food.web.RLS4_1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS4_1)<-RAE.RLS4_1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS4_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS4_1<-list(title= "RLS4_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS4_1<-Community(RAEs.RLS4_1,Properties.Casc.RLS4_1, trophic.links=links)
SaveCommunity(RAE.RLS4_1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS4_1/",fn='write.csv' )

RAEs.LS.RLS4_1 <- Community(properties = CPS(RAE.RLS4_1),
                                nodes = RAEs.RLS4_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS4_1, level='ChainAveragedTrophicLevel')

#RLS4_2
RAE.RLS4_2<-food.web%>%
  filter(Network=="RAE" & Site=="RLS4_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS4_2<-food.web%>%
  filter(Network=="RAE"& Site=="RLS4_2")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS4_2<-left_join(RAE.RLS4_2,RAE.food.web.RLS4_2)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS4_2)<-RAE.RLS4_2$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS4_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS4_2<-list(title= "RLS4_2", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS4_2<-Community(RAEs.RLS4_2,Properties.Casc.RLS4_2, trophic.links=links)
SaveCommunity(RAE.RLS4_2, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS4_2/",fn='write.csv' )

RAEs.LS.RLS4_2 <- Community(properties = CPS(RAE.RLS4_2),
                                nodes = RAEs.RLS4_2,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS4_2, level='ChainAveragedTrophicLevel')


#RLS4_3
RAE.RLS4_3<-food.web%>%
  filter(Network=="RAE" & Site=="RLS4_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS4_3<-food.web%>%
  filter(Network=="RAE"& Site=="RLS4_3")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS4_3<-left_join(RAE.RLS4_3,RAE.food.web.RLS4_3)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS4_3)<-RAE.RLS4_3$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS4_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS4_3<-list(title= "RLS4_3", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS4_3<-Community(RAEs.RLS4_3,Properties.Casc.RLS4_3, trophic.links=links)
SaveCommunity(RAE.RLS4_3, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS4_3/",fn='write.csv' )

RAEs.LS.RLS4_3 <- Community(properties = CPS(RAE.RLS4_3),
                                nodes = RAEs.RLS4_3,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS4_3, level='ChainAveragedTrophicLevel')

#RLS5_1
RAE.RLS5_1<-food.web%>%
  filter(Network=="RAE" & Site=="RLS5_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS5_1<-food.web%>%
  filter(Network=="RAE"& Site=="RLS5_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS5_1<-left_join(RAE.RLS5_1,RAE.food.web.RLS5_1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS5_1)<-RAE.RLS5_1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS5_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS5_1<-list(title= "RLS5_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS5_1<-Community(RAEs.RLS5_1,Properties.Casc.RLS5_1, trophic.links=links)
SaveCommunity(RAE.RLS5_1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS5_1/",fn='write.csv' )

RAEs.LS.RLS5_1 <- Community(properties = CPS(RAE.RLS5_1),
                                nodes = RAEs.RLS5_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS5_1, level='ChainAveragedTrophicLevel')

#RLS5_2
RAE.RLS5_2<-food.web%>%
  filter(Network=="RAE" & Site=="RLS5_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS5_2<-food.web%>%
  filter(Network=="RAE"& Site=="RLS5_2")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS5_2<-left_join(RAE.RLS5_2,RAE.food.web.RLS5_2)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS5_2)<-RAE.RLS5_2$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS5_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS5_2<-list(title= "RLS5_2", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS5_2<-Community(RAEs.RLS5_2,Properties.Casc.RLS5_2, trophic.links=links)
SaveCommunity(RAE.RLS5_2, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS5_2/",fn='write.csv' )

RAEs.LS.RLS5_2 <- Community(properties = CPS(RAE.RLS5_2),
                                nodes = RAEs.RLS5_2,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS5_2, level='ChainAveragedTrophicLevel')

#RLS5_3
RAE.RLS5_3<-food.web%>%
  filter(Network=="RAE" & Site=="RLS5_3")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS5_3<-food.web%>%
  filter(Network=="RAE"& Site=="RLS5_3")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS5_3<-left_join(RAE.RLS5_3,RAE.food.web.RLS5_3)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS5_3)<-RAE.RLS5_3$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS5_3,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS5_3<-list(title= "RLS5_3", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS5_3<-Community(RAEs.RLS5_3,Properties.Casc.RLS5_3, trophic.links=links)
SaveCommunity(RAE.RLS5_3, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS5_3/",fn='write.csv' )

RAEs.LS.RLS5_3 <- Community(properties = CPS(RAE.RLS5_3),
                                nodes = RAEs.RLS5_3,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS5_3, level='ChainAveragedTrophicLevel')

#RLS6_1
RAE.RLS6_1<-food.web%>%
  filter(Network=="RAE" & Site=="RLS6_1")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS6_1<-food.web%>%
  filter(Network=="RAE"& Site=="RLS6_1")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS6_1<-left_join(RAE.RLS6_1,RAE.food.web.RLS6_1)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS6_1)<-RAE.RLS6_1$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS6_1,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS6_1<-list(title= "RLS6_1", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS6_1<-Community(RAEs.RLS6_1,Properties.Casc.RLS6_1, trophic.links=links)
SaveCommunity(RAE.RLS6_1, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS6_1/",fn='write.csv' )

RAEs.LS.RLS6_1 <- Community(properties = CPS(RAE.RLS6_1),
                                nodes = RAEs.RLS6_1,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS6_1, level='ChainAveragedTrophicLevel')

#RLS6_2
RAE.RLS6_2<-food.web%>%
  filter(Network=="RAE" & Site=="RLS6_2")%>%
  distinct(Taxonomic_name, .keep_all = TRUE )

RAE.food.web.RLS6_2<-food.web%>%
  filter(Network=="RAE"& Site=="RLS6_2")%>%
  dplyr::group_by(Taxonomic_name )%>%
  summarize(density=mean(value))

RAEs.RLS6_2<-left_join(RAE.RLS6_2,RAE.food.web.RLS6_2)%>%
  select(density, phylum ,class, order, family, genus, node)
row.names(RAEs.RLS6_2)<-RAE.RLS6_2$Taxonomic_name

#from links, remove species that dont exist
#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method<- "family"

nodes<-cbind(RAEs.RLS6_2,minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))



Properties.Casc.RLS6_2<-list(title= "RLS6_2", project="Sierra Nevada Lake-Stream Nets", densit="m^2")
RAE.RLS6_2<-Community(RAEs.RLS6_2,Properties.Casc.RLS6_2, trophic.links=links)
SaveCommunity(RAE.RLS6_2, dir ="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site/RLS6_2/",fn='write.csv' )

RAEs.LS.RLS6_2 <- Community(properties = CPS(RAE.RLS6_2),
                                nodes = RAEs.RLS6_2,
                                trophic.links = links)

PlotWebByLevel(RAE.RLS6_2, level='ChainAveragedTrophicLevel')
#Done with RAE LS food webs wohooooo!
################################################################################################################################################################################################
#COmbine three local food webs into communtiy colelction

Rae.order<-CommunityCollection(list(RAEs.LS.RLS1_3,RAEs.LS.RLS2_1,RAEs.LS.RLS3_1,RAEs.LS.RLS3_2,RAEs.LS.RLS4_1,RAEs.LS.RLS4_2,RAEs.LS.RLS4_3,RAEs.LS.RLS5_1,RAEs.LS.RLS5_2,RAEs.LS.RLS5_3,RAEs.LS.RLS6_1,RAEs.LS.RLS6_2))

CollectionCPS(Raeing,  properties=NULL)

Rae.data<-CollectionCPS(Raeing, 
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

temp.path <- tempfile("~/Users/matthewdouglasgreen/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Community")
SaveCollection(Rae.order, temp.path)
Rae.order.loaded<-LoadCollection(temp.path)

envs <-read.csv("stream.env.csv")
RAE.env<-envs%>%filter(Network=="RAE")

RAE.all<-cbind(RAE.env,Rae.dads)

dev.off()
par(mfrow=c(4,6))
envlist=names(RAE.env)

for(i in c(2:11,15:17,19:25,27,30:32)) # this creates a loop of 23 iterations
{
  plot(RAE.env[,i],Rae.dads$C , xlab=envlist[c(i)],ylab="Connectance") 
  test2 = lm(Rae.dads$C ~RAE.env[,i])
  abline(test2)
}
varlist<-names(RAE.env)[c(2:11,15:17,19:25,27,30:32)]

models <- lapply(varlist, function(y) {
  lm(substitute(Rae.dads$C~i, list(i = as.name(y))), data = RAE.env)
})
lapply(models, summary)





CollectionNPS(Nets, properties=NULL)

CollectionTLPS(Nets, node.properties=NULL, link.properties=NULL)

###################################################################