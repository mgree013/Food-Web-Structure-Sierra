#Part 1: Local Food web metrics Analysis and Figures

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
