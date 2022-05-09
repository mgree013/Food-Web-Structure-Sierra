#Mass Abundance Curves by Site

#Matthew and Katherine
#September 17,2020

#Papers
#Relationships between body size and abundance in ecology

#Load Packages
Packages <- c("tidyverse", "ggplot2", "vegan", "reshape2","reshape", "adespatial", "sf", "mapview", "viridis", "FD","multcomp","semPlot","lavaan", "performance")
lapply(Packages, library, character.only = TRUE)

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
species_mass_data_env%>%
  filter(Fish !="NA")%>%
  ggplot(aes(x=Fish,y=log(abundance+1),fill=as.factor(Fish)))+
  geom_boxplot()+
  xlab("Fish Presence")+ylab("Log Density")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  facet_wrap(~Taxon, scales="free")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                 panel.border = element_blank(),panel.background = element_blank())


############################################################################################################################################
#Diversity
species<-read.csv(file = "sp.density.update.12.28.19.csv", row.names = 1)

diversity<-species%>%
  transmute(N0=rowSums(species > 0),H= diversity(species),N1 =exp(H),N2 =diversity(species, "inv"),J= H/log(N0),E10= (N1/N0),E20= (N2/N0),Com.Size=log(rowSums(species)+1),betas.LCBD=beta.div(species, method="hellinger",sqrt.D=TRUE)$LCBD) #,betas.LCBD=beta.div(species, method="hellinger",sqrt.D=TRUE)$LCBD ,betas.LCBD.p=beta.div(species, method="chord",sqrt.D=TRUE)$p.LCBD )

diversity.env<-diversity%>%rownames_to_column(var = "Site")%>%left_join(env, by="Site")%>%filter(Fish!="NA")


diversity.env%>%
  gather(N0,  N1,  E10, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=Elevation, y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  xlab("Elevation (m)")+
  facet_wrap(~var, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),legend.position = "none")

diversity.env%>%
  gather(N0, N1,  E10, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=as.factor(Fish), y=value, fill=as.factor(Fish)))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  xlab("Elevation (m)")+
  facet_wrap(~var, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())


diversity.env%>%
  gather(N0, N1,  E10, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=Elevation, y=value, colour=as.factor(Fish)))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  xlab("Elevation (m)")+
  facet_wrap(~var, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

diversity.env%>%
  gather(N0, N1,  E10, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=Elevation, y=value, colour=as.factor(Fish)))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  xlab("Elevation (m)")+
  facet_grid(var~Fish, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

############################################################################################################################################
#NMDS
species_env<-species%>%rownames_to_column("Site")%>%left_join(env, by="Site")%>%filter(Fish!="NA")
species_2<-species_env%>%column_to_rownames("Site")%>%dplyr::select(c(Acentrella:Wormaldia))
set.seed(29)
species<-species_2
dune.rel<-decostand(species,"total") #standardize community data
dune.bray<-vegdist(dune.rel) #calculate dissimilarity among sites (i.e. dissimilarity matrix)
dune.nmds=metaMDS(dune.rel, k=2, try=10) #NMDS code
dune.nmds
stressplot(dune.nmds) #this tells us if our plot is going to work, and it looks good

plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
#text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 1) 
ordihull(dune.nmds, groups=as.factor(species_env$Fish), draw="polygon", label=T)
#ordihull(dune.nmds, groups=sp_abund_env$lake_drainage_name, draw="polygon", label=T)
ordisurf(dune.nmds, species_env$Elevation, prioirty=,labcex=0.9, add = T,col="forestgreen")

#PERMANOVA analysis-Whats driving variation we see above?
adonis2(dune.bray ~ species_env$Fish+species_env$Elevation+species_env$O.NET, permutations = 99, method = "bray")
betad <- betadiver(dune.rel, "z")
adonis(betad ~ species_env$Fish+species_env$Elevation, data=species, perm=200)

adonis2(dune.bray ~ species_env$Fish, permutations = 99, method = "bray")
betad <- betadiver(dune.rel, "z")
adonis(betad ~ species_env$Fish, data=species, perm=200)

dune.envfit <- envfit(dune.nmds, env = species_env$Elevation, perm = 999) #standard envfit
dune.envfit
env.scores.dune <- as.data.frame(scores(dune.envfit, display = "vectors")) #extracts relevant scores from envifit
env.data = cbind(species_env$Elevation)
mds.data.envfit = envfit(dune.nmds, env.data)

plot(mds.data.envfit, col = "black", labels = c( "Elevation"), lwd = 2)

mod <- betadisper(dune.bray, species_env$Fish)
anova(mod)
print(mod)
permutest(mod)
boxplot(mod)

############################################################################################################################################
#CWM


############################################################################################################################################
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
  ggplot(aes(x=log(Body_mass_mg),y=log(abundance)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~Fish)

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
  facet_grid(~Fish)



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

