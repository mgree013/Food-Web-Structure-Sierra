#R GLobis
#Finding more trophic links!
#June 17th, 2020

install.packages("rglobi")
library(rglobi)
library(tidyverse)
library(cheddar)
#Acentrella
Acentrella <- get_interactions(taxon = "Acentrella", interaction.type = "preysOn")
head(Acentrella)

Acentrella <- get_prey_of("Acentrella")
head(Acentrella)

Acentrella <- get_predators_of("Acentrella")
head(Acentrella)

#Aedes
otherkeys = list("limit"=10, "skip"=0)

Zapada <- get_interactions_by_type(source.taxon= "Zapada",interactiontype = c("interactsWith"))
head(Zapada)

Zapada <- get_interactions_table(source.taxon.names = "Zapada", skip=0, limit=100)
head(Zapada)

#this appraoch only allow you to search for 10 interactions at a time, so maybe faster to downbload all and prune

#####
#Downloaded fuill data set and prunning out by name
remove(rglobis)
#1)Data reduction by names
#2) format to integrate with new regisotry.csv
rglobis<-read.csv("interactions.csv")
summary(rglobis)
str(rglobis)

as<-rglobis%>%
  filter(sourceTaxonGenusName =="Zapada"|sourceTaxonGenusName =="Yphria"|sourceTaxonGenusName =="Yoraperla"|sourceTaxonGenusName =="Wiedeman"|sourceTaxonGenusName =="Valvatidae"|sourceTaxonGenusName =="Tipula"|sourceTaxonGenusName =="Tinodes"|sourceTaxonGenusName =="Tabanus"|sourceTaxonGenusName =="Sweltsa"| sourceTaxonGenusName =="Suwallia"|
         sourceTaxonGenusName =="Stilobezzia"|sourceTaxonGenusName =="Stictotarsus"|sourceTaxonGenusName =="Stegopterna"|sourceTaxonGenusName =="Soyedina"|sourceTaxonGenusName =="Skwala"|sourceTaxonGenusName =="Siphlonurus"|sourceTaxonGenusName =="Simulium"|sourceTaxonGenusName =="Sialis"|
         sourceTaxonGenusName =="Serratella"|sourceTaxonGenusName =="Sciara"|sourceTaxonGenusName =="Sanfilippodytes"|sourceTaxonGenusName =="Rhyacophila"|sourceTaxonGenusName =="Rhizelmis"|sourceTaxonGenusName =="Rhithrogena"|sourceTaxonGenusName =="Rhabdomastix"|sourceTaxonGenusName =="Psychoglypha"|sourceTaxonGenusName =="Prosimulium"|sourceTaxonGenusName =="Procloeon"|
         sourceTaxonGenusName =="Polycentropus"|sourceTaxonGenusName =="Pleuroceridae"|sourceTaxonGenusName =="Pisidium"|sourceTaxonGenusName =="Perlinodes"|sourceTaxonGenusName =="Pedicia"|sourceTaxonGenusName =="Parapsyche"|sourceTaxonGenusName =="Paraperla"|sourceTaxonGenusName =="Paraleptophlebia"|sourceTaxonGenusName =="Ormosia"|
         sourceTaxonGenusName =="Oreogeton"|sourceTaxonGenusName =="Oreodytes"|sourceTaxonGenusName =="Optioservus"|sourceTaxonGenusName =="Onocosmoecus"|sourceTaxonGenusName =="Ochrotrichia"|sourceTaxonGenusName =="Nixe"|sourceTaxonGenusName =="Neothremma"|sourceTaxonGenusName =="Neoplasta"|
         sourceTaxonGenusName =="Nectopsyche"|sourceTaxonGenusName =="Muscidae"|sourceTaxonGenusName =="Monophilus"|sourceTaxonGenusName =="Monohelea"|sourceTaxonGenusName =="Micrasema"|sourceTaxonGenusName =="Metacnephia"|sourceTaxonGenusName =="Megarcys"|sourceTaxonGenusName =="Malenka"|
         sourceTaxonGenusName =="Limonia"|sourceTaxonGenusName =="Limnophora"|sourceTaxonGenusName =="Limnophila"|sourceTaxonGenusName =="Leucorrhinia"|sourceTaxonGenusName =="Lestes"|sourceTaxonGenusName =="Lepidostoma"|sourceTaxonGenusName =="Lednia"|sourceTaxonGenusName =="Laccophilus"|sourceTaxonGenusName =="Kogotus"|sourceTaxonGenusName =="Isoperla"|
         sourceTaxonGenusName =="Ironodes"|sourceTaxonGenusName =="Hypsibius"|sourceTaxonGenusName =="Hydropsyche"|sourceTaxonGenusName =="Hydroporus"|sourceTaxonGenusName =="Hydra"|sourceTaxonGenusName =="Hyalella"|sourceTaxonGenusName =="Hexatoma"|sourceTaxonGenusName =="Heteroplectron"|sourceTaxonGenusName =="Heterlimnius"|
         sourceTaxonGenusName =="Hesperoperla"|sourceTaxonGenusName =="Hemerodromia"|sourceTaxonGenusName =="Helodon"|sourceTaxonGenusName =="Haploperla"|sourceTaxonGenusName =="Gyraulus"|sourceTaxonGenusName =="Graptocorixa"|sourceTaxonGenusName =="Glutops"|sourceTaxonGenusName =="Glossosoma"|sourceTaxonGenusName =="Forcipomyia"|sourceTaxonGenusName =="Fallceon"|
         sourceTaxonGenusName =="Ephemerella"|sourceTaxonGenusName =="Epeorus"|sourceTaxonGenusName =="Ecclisomyia"|sourceTaxonGenusName =="Drunella"|sourceTaxonGenusName =="Doroneuria"|sourceTaxonGenusName =="Dolichopus"|sourceTaxonGenusName =="Dixa"|sourceTaxonGenusName =="Diphetor"|sourceTaxonGenusName =="Dicranota"|
           sourceTaxonGenusName =="Dicosmoecus"|sourceTaxonGenusName =="Despaxia"|sourceTaxonGenusName =="Desmona"|sourceTaxonGenusName =="Cultus"|sourceTaxonGenusName =="Culiseta"|sourceTaxonGenusName =="Culicoides"|sourceTaxonGenusName =="Cenocorixa"|sourceTaxonGenusName =="Caudatella"|sourceTaxonGenusName =="Capniidae"|sourceTaxonGenusName =="Calliperla"|
           sourceTaxonGenusName =="Callibaetis"|sourceTaxonGenusName =="Calineuria"|sourceTaxonGenusName =="Brachycentrus"|sourceTaxonGenusName =="Bezzia"|sourceTaxonGenusName =="Baetis"|sourceTaxonGenusName =="Atrichopogon"|sourceTaxonGenusName =="Arctocorisa"|sourceTaxonGenusName =="Aquarius"|sourceTaxonGenusName =="Apatania"|
           sourceTaxonGenusName =="Antocha"|sourceTaxonGenusName =="Amphizoa"|sourceTaxonGenusName =="Ameletus"|sourceTaxonGenusName =="Allotrichoma"|sourceTaxonGenusName =="Alloperla"|sourceTaxonGenusName =="Agraylea"|sourceTaxonGenusName =="Agabus"|sourceTaxonGenusName =="Aedes"|sourceTaxonGenusName =="Acentrella"|
           sourceTaxonClassName=="Turbellaria"|sourceTaxonGenusName =="Palpomyia"|sourceTaxonClassName =="Ostracoda"|sourceTaxonOrderName =="Oligochaeta"|sourceTaxonPhylumName =="Nemertea"|sourceTaxonPhylumName =="Nematomorpha"|sourceTaxonGenusName =="Hirudinea"|sourceTaxonClassName =="Euhirudinea"|sourceTaxonClassName =="Arachnida")


#sourceTaxonGenusName =="Turbellaria",|sourceTaxonGenusName =="Palpomyia"sourceTaxonGenusName =="Ostracoda"|sourceTaxonGenusName =="Oligochaeta"|sourceTaxonGenusName =="Nemertea"|sourceTaxonGenusName =="Nematomorpha"|sourceTaxonGenusName =="Hirundea"|sourceTaxonGenusName =="Euhirudinea"|sourceTaxonGenusName =="Arachnida"|
colnames(species)
getwd()
write.csv(as, "globis.interactions.csv")

levels(as$interactionTypeName)
ectoparasiteOf<-as%>%
  filter(interactionTypeName=="ectoparasiteOf")
write.csv(ectoparasiteOf, "globis.interactions.ectoparasiteOf.csv")

eats<-as%>%
  filter(interactionTypeName=="eats")
write.csv(eats, "globis.interactions.eats.csv")

####################

ass<-rglobis%>%
  filter(targetTaxonGenusName =="Zapada"|targetTaxonGenusName =="Yphria"|targetTaxonGenusName =="Yoraperla"|targetTaxonGenusName =="Wiedeman"|targetTaxonGenusName =="Valvatidae"|targetTaxonGenusName =="Tipula"|targetTaxonGenusName =="Tinodes"|targetTaxonGenusName =="Tabanus"|targetTaxonGenusName =="Sweltsa"| targetTaxonGenusName =="Suwallia"|
           targetTaxonGenusName =="Stilobezzia"|targetTaxonGenusName =="Stictotarsus"|targetTaxonGenusName =="Stegopterna"|targetTaxonGenusName =="Soyedina"|targetTaxonGenusName =="Skwala"|targetTaxonGenusName =="Siphlonurus"|targetTaxonGenusName =="Simulium"|targetTaxonGenusName =="Sialis"|
           targetTaxonGenusName =="Serratella"|targetTaxonGenusName =="Sciara"|targetTaxonGenusName =="Sanfilippodytes"|targetTaxonGenusName =="Rhyacophila"|targetTaxonGenusName =="Rhizelmis"|targetTaxonGenusName =="Rhithrogena"|targetTaxonGenusName =="Rhabdomastix"|targetTaxonGenusName =="Psychoglypha"|targetTaxonGenusName =="Prosimulium"|targetTaxonGenusName =="Procloeon"|
           targetTaxonGenusName =="Polycentropus"|targetTaxonGenusName =="Pleuroceridae"|targetTaxonGenusName =="Pisidium"|targetTaxonGenusName =="Perlinodes"|targetTaxonGenusName =="Pedicia"|targetTaxonGenusName =="Parapsyche"|targetTaxonGenusName =="Paraperla"|targetTaxonGenusName =="Paraleptophlebia"|targetTaxonGenusName =="Ormosia"|
           targetTaxonGenusName =="Oreogeton"|targetTaxonGenusName =="Oreodytes"|targetTaxonGenusName =="Optioservus"|targetTaxonGenusName =="Onocosmoecus"|targetTaxonGenusName =="Ochrotrichia"|targetTaxonGenusName =="Nixe"|targetTaxonGenusName =="Neothremma"|targetTaxonGenusName =="Neoplasta"|
           targetTaxonGenusName =="Nectopsyche"|targetTaxonGenusName =="Muscidae"|targetTaxonGenusName =="Monophilus"|targetTaxonGenusName =="Monohelea"|targetTaxonGenusName =="Micrasema"|targetTaxonGenusName =="Metacnephia"|targetTaxonGenusName =="Megarcys"|targetTaxonGenusName =="Malenka"|
           targetTaxonGenusName =="Limonia"|targetTaxonGenusName =="Limnophora"|targetTaxonGenusName =="Limnophila"|targetTaxonGenusName =="Leucorrhinia"|targetTaxonGenusName =="Lestes"|targetTaxonGenusName =="Lepidostoma"|targetTaxonGenusName =="Lednia"|targetTaxonGenusName =="Laccophilus"|targetTaxonGenusName =="Kogotus"|targetTaxonGenusName =="Isoperla"|
           targetTaxonGenusName =="Ironodes"|targetTaxonGenusName =="Hypsibius"|targetTaxonGenusName =="Hydropsyche"|targetTaxonGenusName =="Hydroporus"|targetTaxonGenusName =="Hydra"|targetTaxonGenusName =="Hyalella"|targetTaxonGenusName =="Hexatoma"|targetTaxonGenusName =="Heteroplectron"|targetTaxonGenusName =="Heterlimnius"|
           targetTaxonGenusName =="Hesperoperla"|targetTaxonGenusName =="Hemerodromia"|targetTaxonGenusName =="Helodon"|targetTaxonGenusName =="Haploperla"|targetTaxonGenusName =="Gyraulus"|targetTaxonGenusName =="Graptocorixa"|targetTaxonGenusName =="Glutops"|targetTaxonGenusName =="Glossosoma"|targetTaxonGenusName =="Forcipomyia"|targetTaxonGenusName =="Fallceon"|
           targetTaxonGenusName =="Ephemerella"|targetTaxonGenusName =="Epeorus"|targetTaxonGenusName =="Ecclisomyia"|targetTaxonGenusName =="Drunella"|targetTaxonGenusName =="Doroneuria"|targetTaxonGenusName =="Dolichopus"|targetTaxonGenusName =="Dixa"|targetTaxonGenusName =="Diphetor"|targetTaxonGenusName =="Dicranota"|
           targetTaxonGenusName =="Dicosmoecus"|targetTaxonGenusName =="Despaxia"|targetTaxonGenusName =="Desmona"|targetTaxonGenusName =="Cultus"|targetTaxonGenusName =="Culiseta"|targetTaxonGenusName =="Culicoides"|targetTaxonGenusName =="Cenocorixa"|targetTaxonGenusName =="Caudatella"|targetTaxonGenusName =="Capniidae"|targetTaxonGenusName =="Calliperla"|
           targetTaxonGenusName =="Callibaetis"|targetTaxonGenusName =="Calineuria"|targetTaxonGenusName =="Brachycentrus"|targetTaxonGenusName =="Bezzia"|targetTaxonGenusName =="Baetis"|targetTaxonGenusName =="Atrichopogon"|targetTaxonGenusName =="Arctocorisa"|targetTaxonGenusName =="Aquarius"|targetTaxonGenusName =="Apatania"|
           targetTaxonGenusName =="Antocha"|targetTaxonGenusName =="Amphizoa"|targetTaxonGenusName =="Ameletus"|targetTaxonGenusName =="Allotrichoma"|targetTaxonGenusName =="Alloperla"|targetTaxonGenusName =="Agraylea"|targetTaxonGenusName =="Agabus"|targetTaxonGenusName =="Aedes"|targetTaxonGenusName =="Acentrella"|
           targetTaxonClassName=="Turbellaria"|targetTaxonGenusName =="Palpomyia"|targetTaxonClassName =="Ostracoda"|targetTaxonOrderName =="Oligochaeta"|targetTaxonPhylumName =="Nemertea"|targetTaxonPhylumName =="Nematomorpha"|targetTaxonGenusName =="Hirudinea"|targetTaxonClassName =="Euhirudinea"|targetTaxonClassName =="Arachnida")
levels(ass$interactionTypeName)
preysOn<-ass%>%
  filter(interactionTypeName=="preysOn")
write.csv(preysOn, "globis.interactions.preysOn.target.csv")

eats<-ass%>%
  filter(interactionTypeName=="eats")
write.csv(eats, "globis.interactions.eats.target.csv")



#