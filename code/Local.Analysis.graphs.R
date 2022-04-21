#Project:The Geography of High Elevation Benthic Food Webs
#Date: 03/21/19-Present
#Authors:Matthew D. Green, David Herbst, and Kurt E. Anderson

#Data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.2fqz612qw
################################################################################################################################

#CLS5_1


#Part 1: Local Food web metrics Analysis and Figures
################################################################################################################################

# Calculate Community Size
species<-read.csv(file = "sp.density.update.12.28.19.csv", row.name=1)

diversity<-species%>%
  #group_by(Site,Network)%>%
  transmute(N0=rowSums(species > 0),H= diversity(species),N1 =exp(H),N2 =diversity(species, "inv"),J= H/log(N0),E10= (N1/N0),E20= (N2/N0),Com.Size=rowSums(species))%>%
  tibble::rownames_to_column("Site")

#env.div.webz<-left_join(env.webzz,diversity, by="Site")%>%drop_na()%>%mutate(Com.Size=log(Com.Size+1))
env.div.webz<-left_join(env.webs,diversity, by="Site")%>%drop_na()%>%mutate(Com.Size=log(Com.Size+1))

################################################################################################################################
#Plots:explore Local Metrics along individual gradients
#S,L,L.S,C,B,I,T,N,Isolated,Can,Omn,Sim.mean,Path

env.div.webz%>%
  gather(L,L.S,C, key = "var", value = "value") %>% 
  ggplot(aes(x = Com.Size, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Community Size Gradient")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.div.webz%>%
  gather(L,L.S,C, key = "var", value = "value") %>% 
  ggplot(aes(x = Head.river.dist, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Distance from Headwaters (m)")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.div.webz%>%
  gather(L,L.S,C, key = "var", value = "value") %>% 
  ggplot(aes(x = River.dist.lake, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Distance from Upstream Lakes (m)")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.div.webz%>%
  gather(L,L.S,C, key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(Fish), y = value, fill=as.factor(Fish))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("no", "yes"))+
  xlab("Fish Presence")+
  labs(fill='Fish Presence') +
  #scale_fill_discrete(name = "Fish Presence", labels = c("no", "yes"))+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())


################################################################################################################################
#1A) Create Environemntal and Spatial PCA Gradients
env.webs.process<-env.webs%>%
  filter(O.NET !="KERN")

#PCA's
envs<-env.webs.process%>%
  dplyr::select(c(Temp,Chlorophyll.mean,Conductivity,DO,pH,Discharge.Mean,SHRUB_SCRUB))

Epca = prcomp(envs, scale.=TRUE)
biplot(Epca)
summary(Epca)
Epca$rotation
ggbiplot(Epca)
ggbiplot(Epca, labels=rownames(env.webs.process$Network), groups=interaction(env.webs.process$Network), ellipse=TRUE)

Epca$x
env_pc_scores <- data.frame(Epca$x[,1:6])
colnames(env_pc_scores)<-c("E_PC1","E_PC2","E_PC3","E_PC4","E_PC5","E_PC6")
rownames(env_pc_scores)<-env.webs.process$Site

#Space
spatials<-env.webs.process%>%
  dplyr::select(c(Head.river.dist, River.dist.lake, Up.Lake.area, Elevation))

Epca = prcomp(spatials, scale.=TRUE)
biplot(Epca)
summary(Epca)
Epca$rotation
ggbiplot(Epca)
ggbiplot(Epca, labels=rownames(env.webs.process$Network), groups=interaction(env.webs.process$Network), ellipse=TRUE)


Epca$x
spatial_pc_scores <- data.frame(Epca$x[,1:4])
colnames(spatial_pc_scores)<-c("S_PC1","S_PC2","S_PC3","S_PC4")
rownames(spatial_pc_scores)<-env.webs.process$Site

################
spatial_pc_scores<-spatial_pc_scores%>%rownames_to_column("Site")
env_pc_scores<-env_pc_scores%>%rownames_to_column("Site")
#env.webs<-env.webs%>%rownames_to_column("Site")

env.webz<-left_join(env.webs.process,spatial_pc_scores, by="Site")
env.webzz<-left_join(env.webz,env_pc_scores, by="Site")


#2) Calculate Community Size
species<-read.csv(file = "sp.density.update.12.28.19.csv", row.name=1)

diversity<-species%>%
  #group_by(Site,Network)%>%
  transmute(N0=rowSums(species > 0),H= diversity(species),N1 =exp(H),N2 =diversity(species, "inv"),J= H/log(N0),E10= (N1/N0),E20= (N2/N0),Com.Size=rowSums(species))%>%
  tibble::rownames_to_column("Site")

diversity
  

env.div.webz<-left_join(env.webzz,diversity, by="Site")%>%drop_na()%>%mutate(Com.Size=log(Com.Size+1))%>%filter(O.NET !="KERN")

################################################################################################################################

env.div.webz%>%
  gather(L,L.S,C, key = "var", value = "value") %>% 
  ggplot(aes(x = E_PC1, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Environmental Gradient (E_PC1)")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.div.webz%>%
  gather(L,L.S,C, key = "var", value = "value") %>% 
  ggplot(aes(x = S_PC1, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Spatial Gradient (S_PC1)")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.div.webz%>%
  gather(L,L.S,C, key = "var", value = "value") %>% 
  ggplot(aes(x = Com.Size, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Community Size Gradient")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.div.webz%>%
  gather(E_PC1,E_PC2,E_PC3,E_PC4,S_PC1,S_PC2,S_PC3,Chlorophyll.mean,Head.river.dist,River.dist.lake, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = C)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.div.webz%>%
  filter(Head.river.dist>3.5)%>%
  gather(E_PC1,E_PC2,E_PC3,E_PC4,S_PC1,S_PC2,S_PC3,Chlorophyll.mean,Head.river.dist,River.dist.lake, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = L)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env.div.webz%>%
  gather(E_PC1,E_PC2,E_PC3,E_PC4,S_PC1,S_PC2,S_PC3,Chlorophyll.mean,Head.river.dist,River.dist.lake, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = L.S)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var, scales = "free") +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

################################################################################################################################
#4) Analysis For Spatial Gradients Steam Hypothesis

#GLMM tmb

#Connectance Model
mod1<-glmmTMB(C~River.dist.lake+ (1|O.NET),family=beta_family(), data=env.div.webz)
mod2<-glmmTMB(C~Head.river.dist+ (1|O.NET),family=beta_family(),data=env.div.webz)
mod3<-glmmTMB(C~Head.river.dist*River.dist.lake+ (1|O.NET),family=beta_family(),data=env.div.webz)
null<-glmmTMB(C~1+ (1|O.NET),family=beta_family(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod2)
check_collinearity(mod3)
multicollinearity(mod3)

#Linkage Model
mod1<-glmmTMB(L~River.dist.lake+ (1|O.NET),family=poisson(), data=env.div.webz)
mod2<-glmmTMB(L~Head.river.dist+ (1|O.NET),family=poisson(),data=env.div.webz)
mod3<-glmmTMB(L~Head.river.dist*River.dist.lake+ (1|O.NET),family=poisson(),data=env.div.webz)
null<-glmmTMB(L~1+ (1|O.NET),family=poisson(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod3)
check_collinearity(mod3)
multicollinearity(mod3)


#Linkage Density Model
mod1<-glmmTMB(L.S~River.dist.lake+ (1|O.NET),family=gaussian(), data=env.div.webz)
mod2<-glmmTMB(L.S~Head.river.dist+ (1|O.NET),family=gaussian(),data=env.div.webz)
mod3<-glmmTMB(L.S~Head.river.dist*River.dist.lake+ (1|O.NET),family=gaussian(),data=env.div.webz)
null<-glmmTMB(L.S~1+ (1|O.NET),family=gaussian(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod1)
check_collinearity(mod3)
multicollinearity(mod3)
################################################################################################################################
#additive Stream Model

#Connectance Model
mod1<-glmmTMB(C~River.dist.lake+ (1|O.NET),family=beta_family(), data=env.div.webz)
mod2<-glmmTMB(C~Head.river.dist+ (1|O.NET),family=beta_family(),data=env.div.webz)
mod3<-glmmTMB(C~Head.river.dist+River.dist.lake+ (1|O.NET),family=beta_family(),data=env.div.webz)
null<-glmmTMB(C~1+ (1|O.NET),family=beta_family(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod2)
check_collinearity(mod3)
multicollinearity(mod3)

#Linkage Model
mod1<-glmmTMB(L~River.dist.lake+ (1|O.NET),family=poisson(), data=env.div.webz)
mod2<-glmmTMB(L~Head.river.dist+ (1|O.NET),family=poisson(),data=env.div.webz)
mod3<-glmmTMB(L~Head.river.dist+River.dist.lake+ (1|O.NET),family=poisson(),data=env.div.webz)
null<-glmmTMB(L~1+ (1|O.NET),family=poisson(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod1)
check_collinearity(mod3)
multicollinearity(mod3)


#Linkage Density Model
mod1<-glmmTMB(L.S~River.dist.lake+ (1|O.NET),family=gaussian(), data=env.div.webz)
mod2<-glmmTMB(L.S~Head.river.dist+ (1|O.NET),family=gaussian(),data=env.div.webz)
mod3<-glmmTMB(L.S~Head.river.dist+River.dist.lake+ (1|O.NET),family=gaussian(),data=env.div.webz)
null<-glmmTMB(L.S~1+ (1|O.NET),family=gaussian(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod1)
check_collinearity(mod3)
multicollinearity(mod3)

################################################################################################################################
#2) River Theory with Fish

#Connectance Model
mod1<-glmmTMB(C~River.dist.lake+ (1|O.NET),family=beta_family(), data=env.div.webz)
mod2<-glmmTMB(C~as.factor(Fish)+ (1|O.NET),family=beta_family(), data=env.div.webz)
mod3<-glmmTMB(C~Head.river.dist+ (1|O.NET),family=beta_family(),data=env.div.webz)
mod4<-glmmTMB(C~Head.river.dist*River.dist.lake+(1|O.NET),family=beta_family(),data=env.div.webz)
mod5<-glmmTMB(C~River.dist.lake* as.factor(Fish)+(1|O.NET),family=beta_family(),data=env.div.webz)
mod6<-glmmTMB(C~Head.river.dist* as.factor(Fish)+(1|O.NET),family=beta_family(),data=env.div.webz)
mod7<-glmmTMB(C~Head.river.dist*River.dist.lake* as.factor(Fish)+(1|O.NET),family=beta_family(),data=env.div.webz)
null<-glmmTMB(C~1+ (1|O.NET),family=beta_family(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod2)
check_collinearity(mod7)
multicollinearity(mod3)

#Linkage Model
mod1<-glmmTMB(L~River.dist.lake+ (1|O.NET),family=poisson(), data=env.div.webz)
mod2<-glmmTMB(L~as.factor(Fish)+ (1|O.NET),family=poisson(), data=env.div.webz)
mod3<-glmmTMB(L~Head.river.dist+ (1|O.NET),family=poisson(),data=env.div.webz)
mod4<-glmmTMB(L~Head.river.dist*River.dist.lake+(1|O.NET),family=poisson(),data=env.div.webz)
mod5<-glmmTMB(L~River.dist.lake* as.factor(Fish)+(1|O.NET),family=poisson(),data=env.div.webz)
mod6<-glmmTMB(L~Head.river.dist* as.factor(Fish)+(1|O.NET),family=poisson(),data=env.div.webz)
mod7<-glmmTMB(L~Head.river.dist*River.dist.lake* as.factor(Fish)+(1|O.NET),family=poisson(),data=env.div.webz)
null<-glmmTMB(L~1+ (1|O.NET),family=poisson(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod7)
check_collinearity(mod3)
multicollinearity(mod3)


#Linkage Density Model
mod1<-glmmTMB(L.S~River.dist.lake+ (1|O.NET),family=gaussian(), data=env.div.webz)
mod2<-glmmTMB(L.S~as.factor(Fish)+ (1|O.NET),family=gaussian(), data=env.div.webz)
mod3<-glmmTMB(L.S~Head.river.dist+ (1|O.NET),family=gaussian(),data=env.div.webz)
mod4<-glmmTMB(L.S~Head.river.dist*River.dist.lake+(1|O.NET),family=gaussian(),data=env.div.webz)
mod5<-glmmTMB(L.S~River.dist.lake* as.factor(Fish)+(1|O.NET),family=gaussian(),data=env.div.webz)
mod6<-glmmTMB(L.S~Head.river.dist* as.factor(Fish)+(1|O.NET),family=gaussian(),data=env.div.webz)
mod7<-glmmTMB(L.S~Head.river.dist*River.dist.lake* as.factor(Fish)+(1|O.NET),family=gaussian(),data=env.div.webz)
null<-glmmTMB(L.S~1+ (1|O.NET),family=gaussian(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod1)
check_collinearity(mod3)
multicollinearity(mod3)

################################################################################################################################
#3 Additive river theory with fish
#2) River Theory with Fish

#Connectance Model
mod1<-glmmTMB(C~River.dist.lake+ (1|O.NET),family=beta_family(), data=env.div.webz)
mod2<-glmmTMB(C~as.factor(Fish)+ (1|O.NET),family=beta_family(), data=env.div.webz)
mod3<-glmmTMB(C~Head.river.dist+ (1|O.NET),family=beta_family(),data=env.div.webz)
mod4<-glmmTMB(C~Head.river.dist+River.dist.lake+(1|O.NET),family=beta_family(),data=env.div.webz)
mod5<-glmmTMB(C~River.dist.lake+ as.factor(Fish)+(1|O.NET),family=beta_family(),data=env.div.webz)
mod6<-glmmTMB(C~Head.river.dist+ as.factor(Fish)+(1|O.NET),family=beta_family(),data=env.div.webz)
mod7<-glmmTMB(C~Head.river.dist+River.dist.lake+ as.factor(Fish)+(1|O.NET),family=beta_family(),data=env.div.webz)
null<-glmmTMB(C~1+ (1|O.NET),family=beta_family(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod7)
check_collinearity(mod7)
multicollinearity(mod3)

#Linkage Model
mod1<-glmmTMB(L~River.dist.lake+ (1|O.NET),family=poisson(), data=env.div.webz)
mod2<-glmmTMB(L~as.factor(Fish)+ (1|O.NET),family=poisson(), data=env.div.webz)
mod3<-glmmTMB(L~Head.river.dist+ (1|O.NET),family=poisson(),data=env.div.webz)
mod4<-glmmTMB(L~Head.river.dist+River.dist.lake+(1|O.NET),family=poisson(),data=env.div.webz)
mod5<-glmmTMB(L~River.dist.lake+ as.factor(Fish)+(1|O.NET),family=poisson(),data=env.div.webz)
mod6<-glmmTMB(L~Head.river.dist+ as.factor(Fish)+(1|O.NET),family=poisson(),data=env.div.webz)
mod7<-glmmTMB(L~Head.river.dist+River.dist.lake+ as.factor(Fish)+(1|O.NET),family=poisson(),data=env.div.webz)
null<-glmmTMB(L~1+ (1|O.NET),family=poisson(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod7)
check_collinearity(mod7)
multicollinearity(mod3)


#Linkage Density Model
mod1<-glmmTMB(L.S~River.dist.lake+ (1|O.NET),family=gaussian(), data=env.div.webz)
mod2<-glmmTMB(L.S~as.factor(Fish)+ (1|O.NET),family=gaussian(), data=env.div.webz)
mod3<-glmmTMB(L.S~Head.river.dist+ (1|O.NET),family=gaussian(),data=env.div.webz)
mod4<-glmmTMB(L.S~Head.river.dist+River.dist.lake+(1|O.NET),family=gaussian(),data=env.div.webz)
mod5<-glmmTMB(L.S~River.dist.lake+ as.factor(Fish)+(1|O.NET),family=gaussian(),data=env.div.webz)
mod6<-glmmTMB(L.S~Head.river.dist+ as.factor(Fish)+(1|O.NET),family=gaussian(),data=env.div.webz)
mod7<-glmmTMB(L.S~Head.river.dist+River.dist.lake+ as.factor(Fish)+(1|O.NET),family=gaussian(),data=env.div.webz)
null<-glmmTMB(L.S~1+ (1|O.NET),family=gaussian(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod1)
check_collinearity(mod7)
multicollinearity(mod3)

################################################################################################################################
#4) Analysis For Processes

#GLMM tmb

#Connectance Model
mod1<-glmmTMB(C~S_PC1+ (1|O.NET),family=beta_family(), data=env.div.webz)
mod2<-glmmTMB(C~E_PC1+ (1|O.NET),family=beta_family(),data=env.div.webz)
mod3<-glmmTMB(C~as.factor(Fish)+ (1|O.NET),family=beta_family(),data=env.div.webz)
mod4<-glmmTMB(C~as.factor(Fish)+S_PC1 +(1|O.NET),family=beta_family(),data=env.div.webz)
mod5<-glmmTMB(C~as.factor(Fish)+E_PC1 +(1|O.NET),family=beta_family(),data=env.div.webz)
mod6<-glmmTMB(C~S_PC1+E_PC1 +(1|O.NET),family=beta_family(),data=env.div.webz)
mod7<-glmmTMB(C~as.factor(Fish)+S_PC1+E_PC1 +(1|O.NET),family=beta_family(),data=env.div.webz)
null<-glmmTMB(C~1+ (1|O.NET),family=beta_family(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod1)
check_collinearity(mod3)
multicollinearity(mod3)

#Linkage Model
mod1<-glmmTMB(L~S_PC1+ (1|O.NET),family=poisson(), data=env.div.webz)
mod2<-glmmTMB(L~E_PC1+ (1|O.NET),family=poisson(),data=env.div.webz)
mod3<-glmmTMB(L~as.factor(Fish)+ (1|O.NET),family=poisson(),data=env.div.webz)
mod4<-glmmTMB(L~as.factor(Fish)+S_PC1 +(1|O.NET),family=poisson(),data=env.div.webz)
mod5<-glmmTMB(L~as.factor(Fish)+E_PC1 +(1|O.NET),family=poisson(),data=env.div.webz)
mod6<-glmmTMB(L~S_PC1+E_PC1 +(1|O.NET),family=poisson(),data=env.div.webz)
mod7<-glmmTMB(L~as.factor(Fish)+S_PC1+E_PC1 +(1|O.NET),family=poisson(),data=env.div.webz)
null<-glmmTMB(L~1+ (1|O.NET),family=poisson(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod6)
check_collinearity(mod7)
multicollinearity(mod3)


#Linkage Density Model
mod1<-glmmTMB(L.S~S_PC1+ (1|O.NET),family=gaussian(), data=env.div.webz)
mod2<-glmmTMB(L.S~E_PC1+ (1|O.NET),family=gaussian(),data=env.div.webz)
mod3<-glmmTMB(L.S~as.factor(Fish)+ (1|O.NET),family=gaussian(),data=env.div.webz)
mod4<-glmmTMB(L.S~as.factor(Fish)*S_PC1 +(1|O.NET),family=gaussian(),data=env.div.webz)
mod5<-glmmTMB(L.S~as.factor(Fish)*E_PC1 +(1|O.NET),family=gaussian(),data=env.div.webz)
mod6<-glmmTMB(L.S~S_PC1*E_PC1 +(1|O.NET),family=gaussian(),data=env.div.webz)
mod7<-glmmTMB(L.S~as.factor(Fish)*S_PC1*E_PC1 +(1|O.NET),family=gaussian(),data=env.div.webz)
null<-glmmTMB(L.S~1+ (1|O.NET),family=gaussian(),data=env.div.webz)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod7)
check_collinearity(mod7)
multicollinearity(mod7)

################################################################################################################################