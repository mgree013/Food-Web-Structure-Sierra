library(tidyverse)
library(cheddar)
library(ggplot2)
library(cowplot)

#cascade.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site.Genus/"
#cascade.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Cascade_Sites_Order_Level_02_12_20/"
cascade.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Cascade/Site.Family/"
Cascade.web<-LoadCollection(cascade.dir)

#evo.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/EVO/Site.Genus/"
#evo.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/EVO/Site.Order/"
evo.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/EVO/Site.Family/"
Evo.web<-LoadCollection(evo.dir)

#rae.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site.Genus/"
#rae.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Rae_site_order_02_12_20/"
rae.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/RAE/Site.Family/"
Rae.web<-LoadCollection(rae.dir)

#kern.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/KERN/Site.Genus/"
#kern.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/KERN/Site.Order/"
kern.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/KERN/Site.Family/"
Kern.web<-LoadCollection(kern.dir)

#bubbs.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site.Genus/"
#bubbs.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site.Order/"
bubbs.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Bubbs/Site.Family/"
Bubbs.web<-LoadCollection(bubbs.dir)

#rock.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Rock/Genus/"
#rock.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Rock/Family/"
rock.dir="~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web/Sites/Rock/Family/"
Rock.web<-LoadCollection(rock.dir)

Rae.data<-CollectionCPS(Rae.web, 
                        c('title',
                          S='NumberOfNodes',
                          L='NumberOfTrophicLinks',
                          L.S='LinkageDensity',
                          C='DirectedConnectance',
                          B='FractionBasalNodes',
                          I='FractionIntermediateNodes',
                          T='FractionTopLevelNodes',
                          N='FractionNonTopLevelNodes',
                          Isolated='FractionIsolatedNodes',
                          Can='FractionCannibalistic',
                          Omn='FractionOmnivorous',
                          Sim.mean= 'MeanMaximumTrophicSimilarity',
                          #Sim.max='MaximumTrophicSimilarity',
                          #Vul='NormalisedTrophicVulnerability',
                          #Gen='NormalisedTrophicGenerality',
                          Path='CharacteristicPathLength'
                        ))
Evo.data<-CollectionCPS(Evo.web, 
                        c('title',
                          S='NumberOfNodes',
                          L='NumberOfTrophicLinks',
                          L.S='LinkageDensity',
                          C='DirectedConnectance',
                          B='FractionBasalNodes',
                          I='FractionIntermediateNodes',
                          T='FractionTopLevelNodes',
                          N='FractionNonTopLevelNodes',
                          Isolated='FractionIsolatedNodes',
                          Can='FractionCannibalistic',
                          Omn='FractionOmnivorous',
                          Sim.mean= 'MeanMaximumTrophicSimilarity',
                          #Sim.max='MaximumTrophicSimilarity',
                          #Vul='NormalisedTrophicVulnerability',
                          #Gen='NormalisedTrophicGenerality',
                          Path='CharacteristicPathLength'
                        ))
Cascade.data<-CollectionCPS(Cascade.web, 
                        c('title',
                          S='NumberOfNodes',
                          L='NumberOfTrophicLinks',
                          L.S='LinkageDensity',
                          C='DirectedConnectance',
                          B='FractionBasalNodes',
                          I='FractionIntermediateNodes',
                          T='FractionTopLevelNodes',
                          N='FractionNonTopLevelNodes',
                          Isolated='FractionIsolatedNodes',
                          Can='FractionCannibalistic',
                          Omn='FractionOmnivorous',
                          Sim.mean= 'MeanMaximumTrophicSimilarity',
                          #Sim.max='MaximumTrophicSimilarity',
                          #Vul='NormalisedTrophicVulnerability',
                          #Gen='NormalisedTrophicGenerality',
                          Path='CharacteristicPathLength'
                          ))

Kern.data<-CollectionCPS(Kern.web, 
                            c('title',
                              S='NumberOfNodes',
                              L='NumberOfTrophicLinks',
                              L.S='LinkageDensity',
                              C='DirectedConnectance',
                              B='FractionBasalNodes',
                              I='FractionIntermediateNodes',
                              T='FractionTopLevelNodes',
                              N='FractionNonTopLevelNodes',
                              Isolated='FractionIsolatedNodes',
                              Can='FractionCannibalistic',
                              Omn='FractionOmnivorous',
                              Sim.mean= 'MeanMaximumTrophicSimilarity',
                              #Sim.max='MaximumTrophicSimilarity',
                              #Vul='NormalisedTrophicVulnerability',
                              #Gen='NormalisedTrophicGenerality',
                              Path='CharacteristicPathLength'
                            ))

Bubbs.data<-CollectionCPS(Bubbs.web, 
                         c('title',
                           S='NumberOfNodes',
                           L='NumberOfTrophicLinks',
                           L.S='LinkageDensity',
                           C='DirectedConnectance',
                           B='FractionBasalNodes',
                           I='FractionIntermediateNodes',
                           T='FractionTopLevelNodes',
                           N='FractionNonTopLevelNodes',
                           Isolated='FractionIsolatedNodes',
                           Can='FractionCannibalistic',
                           Omn='FractionOmnivorous',
                           Sim.mean= 'MeanMaximumTrophicSimilarity',
                           #Sim.max='MaximumTrophicSimilarity',
                           #Vul='NormalisedTrophicVulnerability',
                           #Gen='NormalisedTrophicGenerality',
                           Path='CharacteristicPathLength'
                         ))

Rock.data<-CollectionCPS(Rock.web, 
                          c('title',
                            S='NumberOfNodes',
                            L='NumberOfTrophicLinks',
                            L.S='LinkageDensity',
                            C='DirectedConnectance',
                            B='FractionBasalNodes',
                            I='FractionIntermediateNodes',
                            T='FractionTopLevelNodes',
                            N='FractionNonTopLevelNodes',
                            Isolated='FractionIsolatedNodes',
                            Can='FractionCannibalistic',
                            Omn='FractionOmnivorous',
                            Sim.mean= 'MeanMaximumTrophicSimilarity',
                            #Sim.max='MaximumTrophicSimilarity',
                            #Vul='NormalisedTrophicVulnerability',
                            #Gen='NormalisedTrophicGenerality',
                            Path='CharacteristicPathLength'
                          ))

all.webs<-rbind(Kern.data,Cascade.data,Evo.data,Bubbs.data,Rae.data, Rock.data)
all.webs<-all.webs%>%tibble::rownames_to_column()%>%rename(Site = title) 
#setwd("~/Dropbox/Sierra Lake Project/Food.Webs/MDG.Web/Sierra.Nevada.LS.Food.Web")

envs <-read.csv("dave.matt.env.full.12.29.19.csv")
env<-envs%>%filter(Network == "RAE" | Network=="EVO" | Network =="CASCADE"| O.NET =="KERN" | Network=="BUBBS" | Network=="ROCK")


env<-env%>%mutate(Euc.dist.lake=log(1+Euc.dist.lake),River.dist.lake=log(1+River.dist.lake),Head.river.dist=log(1+Head.river.dist))%>%
  mutate(Size.net.dist=Head.river.dist*Up.Lake.area,Size.river.dist=River.dist.lake*Up.Lake.area,Elev.dist=River.dist.lake/Elevation)
env.webs<-left_join(env,all.webs )



####PLotting####
env.webs%>%
  gather(S,L,L.S,C,B,I,Isolated,Can,Omn,Sim.mean,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = Head.river.dist, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme_bw()

env.webs%>%
  gather(L,L.S,C,B,I,Isolated,Can,Omn,Sim.mean,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = River.dist.lake, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme_bw()

env.webs%>%
  gather(C,L.S,L, key = "var", value = "value") %>% 
  ggplot(aes(x = Head.river.dist, y = value,colour=New.Net)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(var~New.Net, scales = "free_y") +
  theme_bw()

env.webs%>%
  gather(C,L.S,L, key = "var", value = "value") %>% 
  ggplot(aes(x = River.dist.lake, y = value,colour=New.Net)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(var~New.Net, scales = "free_y") +
  theme_bw()

env.webs%>%
  gather(C,L.S,L, key = "var", value = "value") %>% 
  ggplot(aes(x = River.dist.lake, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme_bw()

env.webs%>%
  gather(S,L,L.S,C,B,I,Isolated,Can,Omn,Sim.mean,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = Elevation, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme_bw()

env.webs%>%
  gather(C,L.S,L, key = "var", value = "value") %>% 
  ggplot(aes(x = Elevation, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme_bw()

env.webs%>%
  gather(Elevation,Head.river.dist,River.dist.lake,SHRUB_SCRUB,Up.Lake.area, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = C)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_wrap(~var, scales = "free") +
  theme_bw()

dog<-lm(L~(Head.river.dist), data=env.webs)
summary(dog)




#nice
a<-env.webs%>%
  gather(Head.river.dist,River.dist.lake,Elevation, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = L,colour=O.NET)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(O.NET~var, scales = "free") +
  theme_bw()+
  ylab("Number of Trophic Links (L)")+
  #xlab("Log Headwater Distance (m)           Log Distance from Lake (m)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")


b<-env.webs%>%
  gather(Head.river.dist,River.dist.lake,Elevation, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = C,colour=O.NET)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(O.NET~var, scales = "free") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"))+
  ylab("Directed Connectance (C)")+
  #xlab("Log Headwater Distance (m)           Log Distance from Lake (m)")+
  theme(legend.position = "none")


c<-env.webs%>%
  gather(Head.river.dist,River.dist.lake,Elevation, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = L.S,colour=O.NET)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(O.NET~var, scales = "free") +
  theme_bw()+
  ylab("Linkage Desnity (L/S)")+
  #xlab("Log Headwater Distance (m)           Log Distance from Lake (m)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

plot_grid(a,b,c, ncol=3)


varlist<-names(env.webs)[c(31:43)]

modRCLS <- lapply(varlist, function(y) {
  lm(substitute(i~env.webs$River.dist.lake*env.webs$Head.river.dist, list(i = as.name(y))), data = env.webs)
})
lapply(modRCLS, summary)


ggplot(env.webs, aes(x=as.factor(Stream.System), y=C))+geom_boxplot()
ggplot(env.webs, aes(x=as.factor(Stream.Number), y=C))+geom_boxplot()
ggplot(env.webs, aes(x=(Local.Distance), y=C, colour=Network))+geom_point()+geom_smooth(method="lm",se=F)


#Heat map of both head and River dist metrics
#Beta
lm.mod <- lm(C ~ Head.river.dist*River.dist.lake, data = env.webs)
summary(lm.mod)

range(env.webs$Head.river.dist)
range(env.webs$River.dist.lake)

prepplot1 <- as.data.frame(matrix(ncol = 3, nrow = 10000))
colnames(prepplot1) <- c("River.dist.lake", "Head.river.dist", "est.C")

prepplot1$Head.river.dist <- rep(seq(2.397895 ,10.063097, length.out = 100), 100)
prepplot1 <- prepplot1[order(prepplot1$Head.river.dist),]
prepplot1$River.dist.lake <- rep(seq(0.000000, 9.623564, length.out = 100), 100)
#prepplot1$est.C <- -4.99626     +0.21383 *prepplot1$Head.river.dist +0.75407 *prepplot1$River.dist.lake + 
#  -0.09176 *prepplot1$Head.river.dist*prepplot1$River.dist.lake
prepplot1$est.C<-predict(lm.mod,prepplot1, type="response")

st.bd<-ggplot(prepplot1) +
  geom_tile(aes(River.dist.lake, Head.river.dist, fill = est.C))+
  #ggtitle("e)") +
  scale_fill_viridis_c()+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))+
  ggplot2::xlab("Log Distance from Upstream Lakes (m)") + ylab("Log Distance from Headwaters (m)") +
  ggplot2::labs(fill = "C")


