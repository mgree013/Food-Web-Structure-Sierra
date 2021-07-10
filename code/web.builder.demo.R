#Gray et al Food Webs MS
#worked example of how to use the WebBuilder function within teh cheddar package
#require(devtools)
#install_version("caTools", version = "1.17.1.2", repos = "http://cran.us.r-project.org")

library(cheddar)
source("web.builder.R")

#add in links
#read in without factor levels so NAs can be removed later
registry<-read.csv('1-s2.0-S2352249615300045-mmc4.csv')

data(BroadstoneStream)
BroadstoneStream <- OrderCommunity(BroadstoneStream, 'M')
IsolatedNodes(BroadstoneStream)
BroadstoneStream<-RemoveNodes(community=BroadstoneStream, remove=c('Algae','CPOM','FPOM','Leptothrix spp.','Terrestrial invertebrates','Diptera'))

#extract node information
#n.b. node names need to be resolved using the GBIF database on Global Names Resolver 
#in order to match the taxonomy used in the database
nps<-NPS(BroadstoneStream)

#supply a list of desired level of generalisation, either a single list with one entry per node, or two
#with two entries per node, i.e. a 'resource method' and a 'consumer method'
minimum.res.method<- c("class",
                       "family",
                       "family",
                       "family",
                       "family",
                       "family",
                       "family",
                       "family",
                       "family",
                       "genus",
                       "genus",
                       "family",
                       "family",
                       "genus",
                       "genus",
                       "family",
                       "family",
                       "genus",
                       "genus",
                       "order",
                       "genus",
                       "family",
                       "class",
                       "family",
                       "genus",
                       "family",
                       "family",
                       "genus",
                       "genus",
                       "family",
                       "genus") 
minimum.con.method <- c("class",
                        "genus",
                        "genus",
                        "genus",
                        "genus",
                        "genus",
                        "genus",
                        "family",
                        "genus",
                        "genus",
                        "genus",
                        "family",
                        "family",
                        "genus",
                        "genus",
                        "genus",
                        "genus",
                        "genus",
                        "genus",
                        "order",
                        "genus",
                        "family",
                        "class",
                        "family",
                        "genus",
                        "family",
                        "family",
                        "genus",
                        "genus",
                        "family",
                        "genus")

nodes<-cbind(nps[,c(1,7:10)],minimum.res.method,minimum.con.method)

links<-WebBuilder(nodes, registry, method=c('exact','genus','family','order','class'))

BroadstoneExample <- Community(properties = CPS(BroadstoneStream),
                               nodes = NPS(BroadstoneStream),
                               trophic.links = links)

table(links$con.method, links$res.method)

#visualise network
PlotWebByLevel(BroadstoneExample, level='ChainAveragedTrophicLevel')


