library(reshape)
library(tidyverse)
#purpose: CONstruct trophick links for all Sierra Nevada species, USe those for local food web in local.food.webs.R

##################################################################################################################################################################################################################
setwd("~/Dropbox/Sierra Lake Project/Food.Webs//Joining.the.dots.Web/ScienceDirect_files_21Oct2019_15-14-40.23")


specie<-read.csv("Sp_density_updated.csv")
summary(specie)

envs <-read.csv("stream.env.csv")
summary(envs)

#Make wide data lonh
doggy<-cbind(envs[c(1,14)],specie)
cats<-melt(doggy, id.vars=c("Site", "Network"), variable_name = "Taxonomic_name", value.name ="Species_density")

#write.csv(cats, file="food.web.csv")
#remove 0 values
big.cats<-cats%>%
  filter(value > 0)

write.csv(big.cats, "dog.csv")

#Add in taxonomic info
#install.packages("taxize")
library("taxize")
specieslist<-as.vector(big.cats$Taxonomic_name)
specieslist<-unique(specieslist)
#sapply(strsplit(big.cats$Taxonomic_name, '[, ]+'), function(x) toString(dQuote(x)))
xx<-tax_name(query = c(specieslist), get = c("phylum","class","order", "family","genus"), db = "itis")

dogs.cats<-cbind(big.cats,xx)

food.web<-read.csv("dog.csv")
properties<-read.csv("properties.csv")
props<-food.web%>%
  distinct(Taxonomic_name, Resolved.to)
food.web.unique<-food.web

food.web.unique<-food.web%>%
  distinct(Taxonomic_name, class, order, family, genus, node)
row.names(food.web.unique)<-food.web.unique$Taxonomic_name
food.web.stream<-food.web.unique%>%select(node, class, order, family, genus)


#make preoperties list
Properties.SN<-list(title= "Sierra Nevada", value="m^2")
Sierra.Nevada<-Community(food.web.unique,Properties.SN, trophic.links=links)

#User input here can help make food webs better reoslved
minimum.res.method<- "family"
minimum.con.method <-  "family"


#minimum.res.method<- list(props$Resolved.to)
#minimum.con.method <- list(props$Resolved.to)
  
nodes<-cbind(food.web.stream,minimum.res.method,minimum.con.method)
#colnames(nodes) <-c("node", "class", "order", "family", "genus", "minimum.res.method", "minimum.con.method")

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

Sierra.Nevada.LS <- Community(properties = CPS(BroadstoneStream),
                               nodes = food.web.stream,
                               trophic.links = links)
                        
                              

table(links$con.method, links$res.method)

#visualise network
PlotWebByLevel(Sierra.Nevada.LS, level='ChainAveragedTrophicLevel')

PlotPredationMatrix(Sierra.Nevada.LS)
PlotCircularWeb(Sierra.Nevada.LS)
PlotWebByLevel(Sierra.Nevada.LS, level='TrophicSpecies')
NumberOfNodes(Sierra.Nevada.LS)
NumberOfTrophicLinks(Sierra.Nevada.LS)

PlotNPS(BroadstoneExample, 'TL', 'Log10N')

#Calulate some food web metrics
dogggs<-NPS(Sierra.Nevada.LS,list(TS1='TrophicSpecies', 
                                   TS2=list('TrophicSpecies', include.isolated=FALSE), 
                                   Iso='IsIsolatedNode'))

PlotNPSDistribution(Sierra.Nevada.LS, 
                  property=CPS(BroadstoneStream), 
                  main = CPS(BroadstoneStream)$title, 
                  density.args = list())
