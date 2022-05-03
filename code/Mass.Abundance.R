#Mass Abundance Curves by Site

#Matthew and Katherine
#September 17,2020

#Papers
#Relationships between body size and abundance in ecology

library(ggplot2)
library(tidyverse)

setwd("~/Dropbox/Manuscipts/L-S Food web/Food-Web-Structure-Sierra/data")

species<-read.csv(file = "sp.density.update.12.28.19.csv")
summary(species)

traits<-read.csv("Full_fn_trait.csv")
summary(traits)

env <-read.csv("dave.matt.env.full.12.29.19.csv")
summary(env)

#organize data
env<-env%>%mutate(Euc.dist.lake=log(Euc.dist.lake+1),River.dist.lake=log(River.dist.lake+1),Head.river.dist=log(Head.river.dist+1))
envs<-env%>%dplyr::select(c(Site,O.NET))
species_all<-species%>%pivot_longer(-Site, names_to="Taxon", values_to="abundance")%>%filter(abundance>0)
traits_mass<-traits%>%rename(Body_mass_mg=M)%>%dplyr::select(c(Taxon, Body_mass_mg))
species_mass_data<-left_join(species_all,traits_mass, by="Taxon")

species_mass_data_env<-left_join(species_mass_data,env, by="Site")%>%filter(O.NET != "YOUNG")
########################################################################################################################
#Create Mass abundance curves for each site, network, and regionally

#1)Mass Abundance by Site
CLS1_1<-species_mass_data_env%>%filter(Site=="CLS1_1")
CLS1_1%>%
  ggplot(aes(x=log(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")

#group by Networks and Site (repeat for our other Networks: BUBBS, EVO)
species_mass_data_env%>% 
  filter(O.NET=="CASCADE")%>%
  ggplot(aes(x=log(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~Site)

species_mass_data_env%>% 
  #filter(O.NET=="CASCADE")%>%
  ggplot(aes(x=log(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~Fish)

#2)Group by Network
species_mass_data_env_net<-species_mass_data_env%>%
  group_by(O.NET,Taxon,Body_mass_mg)%>%
  summarise(abundance=sum(abundance))

species_mass_data_env_net%>%
  ggplot(aes(x=(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~O.NET)

species_mass_data_env_net%>%
  ggplot(aes(x=(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")

#3)Regionally all sites together

species_mass_data_env_reg<-species_mass_data_env%>%
  group_by(Taxon,Body_mass_mg)%>%
  summarise(abundance=sum(abundance))

species_mass_data_env_reg%>%
  ggplot(aes(x=log(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")

species_mass_data_env%>%
  ggplot(aes(x=log(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~Order)



########################################################################################################################
#Part 2: CWM

species.traits<-read.csv(file = "sp.density.update.12.28.19_traits.csv", row.names = 1)
#tr.traits<-read.csv("Full_full_fn_trait.csv")


row.traits<-tr.traits%>%filter(Taxon !="Benthic.producers" & Taxon !="Pelagic.producers")
traitsy<-tr.traits%>%dplyr::rename(Body_mass_mg=M)%>%filter(Taxon !="Benthic.producers" & Taxon !="Pelagic.producers")%>%dplyr::select(c(Body_mass_mg))
str(traitsy)
#rownames(traitsy)<-rownames(row.traits)
rownames(traitsy)<-row.traits$Taxonomic_name

tres_bm = dbFD(traitsy,species.traits, corr = ("lingoes"),
               stand.FRic = TRUE, calc.FDiv = TRUE)

cwm=tres_bm$CWM
FDis=as.data.frame(tres_bm$FDis)
FEve=as.data.frame(tres_bm$FEve)
FRic=as.data.frame(tres_bm$FRic)

head(cwm)

datas<-cbind(env,cwm,FRic,FDis,FEve)
datasz<-datas%>%
  filter(River.dist.lake>0.1)%>%
  #filter(Head.river.dist>2.3)%>%
  #filter(Fish != "NA")%>%
  filter(O.NET != "YOUNG")%>%
  filter(Body_mass_mg<15)
  
datasz%>%
  ggplot(aes(x = log(Elevation+1), y = Body_mass_mg))+ #, colour=as.factor(Fish))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  #geom_smooth(method = "lm")+
  stat_smooth(method = glm, method.args = list(family = gaussian(link="identity")))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

datasz%>%
  filter(Fish !="NA")%>%
  gather(Head.river.dist,River.dist.lake,Elevation,key = "var", value = "value") %>% #PC2,PC3,PC4,River.dist.lake,
  ggplot(aes(x = value, y = Body_mass_mg, colour=as.factor(Fish)))+ #, colour=as.factor(Fish))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  #geom_smooth(method = "lm")+
  stat_smooth(method = glm, method.args = list(family = gaussian(link="identity")))+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

datasz%>%
  gather(Head.river.dist,River.dist.lake,key = "var", value = "value") %>% #PC2,PC3,PC4,River.dist.lake,
  ggplot(aes(x = value, y = Body_mass_mg))+ #, colour=as.factor(Fish))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  #geom_smooth(method = "lm")+
  stat_smooth(method = glm, method.args = list(family = gaussian(link="identity")))+
  facet_grid(O.NET~ var, scales = "free") +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

datasz%>%
  #filter(Fish != "NA")%>%
  ggplot(aes(x = as.factor(Fish), y = Body_mass_mg, fill=as.factor(Fish)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("no", "yes"))+
  xlab("Fish Presence")+
  labs(fill='Fish Presence') +
  #scale_fill_discrete(name = "Fish Presence", labels = c("no", "yes"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())


dog<-lm(Body_mass_mg~Head.river.dist, datasz)
summary(dog)

dog<-lm(Body_mass_mg~River.dist.lake, datasz)

dog<-lm(Body_mass_mg~River.dist.lake*Head.river.dist, datasz)
summary(dog)

dog<-glm(Body_mass_mg~as.factor(Fish)*River.dist.lake*Head.river.dist,family = gaussian(link = "identity"), datasz)
summary(dog)
r2(dog)

dog<-aov(Body_mass_mg~as.factor(Fish), datasz)

#################################################

