#Project:The Geography of High Elevation Benthic Food Webs
#Date: 03/21/19-Present
#Authors:Matthew D. Green, David Herbst, and Kurt E. Anderson

#Data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.2fqz612qw
################################################################################################################################

#Part 1: Local Food web metrics Analysis and Figures
################################################################################################################################

#explore Local Metrics along individual gradients

env.webzz%>%
  filter(Head.river.dist>3.5)%>%
  gather(S,L,L.S,C,Omn,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = E_PC1, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Environmental Gradient (E_PC1)")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.webzz%>%
  filter(Head.river.dist>3.5)%>%
  gather(S,L,L.S,C,Omn,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = S_PC1, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Spatial Gradient (S_PC1)")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.webs%>%
  filter(Head.river.dist>3.5)%>%
  gather(S,L,L.S,C,B,I,Sim.mean,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = Head.river.dist, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Distance from Headwaters (m)")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.webs%>%
  #filter(Head.river.dist>3.5)%>%
  gather(S,L,L.S,C,B,I,Sim.mean,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = River.dist.lake, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Distance from Upstream Lakes (m)")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.webs%>%
  filter(Head.river.dist>3.5)%>%
  gather(S,L,L.S,C,B,I,Isolated,Can,Omn,Sim.mean,Path, key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(Fish), y = value, fill=as.factor(Fish))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("no", "yes"))+
  xlab("Fish Presence")+
  labs(fill='Fish Presence') +
  #scale_fill_discrete(name = "Fish Presence", labels = c("no", "yes"))+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.webzz%>%
  filter(Head.river.dist>3.5)%>%
  gather(E_PC1,E_PC2,E_PC3,E_PC4,S_PC1,S_PC2,S_PC3,Chlorophyll.mean,Head.river.dist,River.dist.lake, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = C)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme(axis.title.x=element_blank(),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())


################################################################################################################################
#1A) Create Environemntal and Spatial PCA Gradients
#PCA's
envs<-env.webs%>%dplyr::select(c(Temp,Chlorophyll.mean,Conductivity,DO,pH,Discharge.Mean,SHRUB_SCRUB))

Epca = prcomp(envs, scale.=TRUE)
biplot(Epca)
summary(Epca)
Epca$rotation
ggbiplot(Epca)
ggbiplot(Epca, labels=rownames(env.webs$Site), groups=interaction(env.webs$Site), ellipse=TRUE)

Epca$x
env_pc_scores <- data.frame(Epca$x[,1:6])
colnames(env_pc_scores)<-c("E_PC1","E_PC2","E_PC3","E_PC4","E_PC5","E_PC6")
rownames(env_pc_scores)<-env.webs$Site

#Space
spatials<-env.webs%>%dplyr::select(c(Head.river.dist, River.dist.lake, Up.Lake.area, Elevation))

Epca = prcomp(spatials, scale.=TRUE)
biplot(Epca)
summary(Epca)
Epca$rotation
ggbiplot(Epca)


Epca$x
spatial_pc_scores <- data.frame(Epca$x[,1:4])
colnames(spatial_pc_scores)<-c("S_PC1","S_PC2","S_PC3","S_PC4")
rownames(spatial_pc_scores)<-env.webs$Site

################
spatial_pc_scores<-spatial_pc_scores%>%rownames_to_column("Site")
env_pc_scores<-env_pc_scores%>%rownames_to_column("Site")
#env.webs<-env.webs%>%rownames_to_column("Site")

env.webz<-left_join(env.webs,spatial_pc_scores, by="Site")
env.webzz<-left_join(env.webz,env_pc_scores, by="Site")
################################################################################################################################
#2) Calculate Community Size
species<-read.csv(file = "data/sp.density.update.12.28.19.csv", row.name=1)

diversity<-species%>%
  #group_by(Site,Network)%>%
  transmute(N0=rowSums(species > 0),H= diversity(species),N1 =exp(H),N2 =diversity(species, "inv"),J= H/log(N0),E10= (N1/N0),E20= (N2/N0),Com.Size=rowSums(species))%>%
  rownames_to_column("Site")

env.div.webz<-left_join(env.webzz,diversity, by="Site")
################################################################################################################################
#3)
