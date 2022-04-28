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
env<-env%>%mutate(Euc.dist.lake=log(Euc.dist.lake+1),River.dist.lake=log(River.dist.lake),Head.river.dist=sqrt(Head.river.dist+1))
envs<-env%>%dplyr::select(c(Site,O.NET))
species_all<-species%>%pivot_longer(-Site, names_to="Taxon", values_to="abundance")%>%filter(abundance>0)
traits_mass<-traits%>%rename(Body_mass_mg=M)%>%dplyr::select(c(Taxon, Body_mass_mg))
species_mass_data<-left_join(species_all,traits_mass, by="Taxon")

species_mass_data_env<-left_join(species_mass_data,env, by="Site")
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
  ggplot(aes(x=log(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~O.NET)

species_mass_data_env_net%>%
  ggplot(aes(x=log(Body_mass_mg),y=log(abundance)))+
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
