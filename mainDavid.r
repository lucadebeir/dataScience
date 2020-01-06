install.packages("readxl") 
library("readxl")

install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

bdd_auto <- read_excel("C:/Users/alexa/Documents/R/dataScience/Data/bdd_auto-modif.xlsx")
bdd_2_roue <- read_excel("C:/Users/alexa/Documents/R/dataScience/Data/bdd_2_roue.xls")

#View(bdd_auto_modif) 
#View(bdd_2_roue)

#renommage des 2 premières colonnes de la base 2-roue
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...1"] <- "Identifiant"
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...2"] <- "Date"

#creation de la base de donnees conjointes (avec les personnes utilisant auto et 2-roue)
bdd <- merge(bdd_auto, bdd_2_roue, by="Identifiant")
View(bdd)
install.packages("FactoMineR")
library(FactoMineR)

# début diagrammes

#gros rouleur fréquence


gros_rouleurs <- bdd %>%
  select(Identifiant, moto1="Nb km 1", moto2="...56", moto3="...67", moto4="...78", auto="Q17 [1]")

#class(gros_rouleurs$moto1)
#On doit changer le type pour pouvoir sommer
gros_rouleurs$moto1 <- as.numeric(as.character(gros_rouleurs$moto1))
gros_rouleurs$moto2 <- as.numeric(as.character(gros_rouleurs$moto2))
gros_rouleurs$moto3 <- as.numeric(as.character(gros_rouleurs$moto3))
gros_rouleurs$moto4 <- as.numeric(as.character(gros_rouleurs$moto4))
#class(gros_rouleurs$moto1)

#On transforme les valeurs nulles en 0 pour pouvoir sommer

gros_rouleurs[is.na(gros_rouleurs)] <- 0
gr_sum <- gros_rouleurs %>% 
  mutate(sommeMoto = rowSums(.[2:5])) %>%
  arrange(desc(sommeMoto))
View(gr_sum)

#diagramme 
# on enleve des données qui semblent incohérentes
#voici le graphes des personnes gros rouleurs en voiture et en motos

moyenne_km_an_echantillon <- mean(gr_sum_total$kmTotaux) #27718.2 km
moyenne_km_an <- 17000
moy_km <- moyenne_km_an

gros_rouleur_voitureEtMoto <- gr_sum %>% 
  group_by(sommeMoto) %>%
  filter( between(sommeMoto, 17000, 100000)) %>%
  ungroup(.self) %>%
  group_by(auto) %>%
  filter( between(auto, 17000, 100000))

gr_sum_test <- gr_sum %>% 
  group_by(sommeMoto) %>%
  filter( between(sommeMoto, 0, 100000))

gr_sum_test <- gr_sum %>% 
  group_by(auto) %>%
  filter( between(auto, 0, 100000))
gr_sum_test

ggplot(gros_rouleur_voitureEtMoto, aes(x=auto, y=sommeMoto)) + 
  geom_point()

gr_sum3 <- gr_sum[,-1][,-1][,-1][,-1][,-1]

petit_rouleur_Moto <- gr_sum %>% 
  group_by(sommeMoto) %>%
  filter( between(sommeMoto, 0, 16999))

petit_rouleur_voiture <- gr_sum %>% 
  group_by(auto) %>%
  filter( between(auto, 0, 16999))

gros_rouleur_Moto <- gr_sum %>% 
  group_by(sommeMoto) %>%
  filter( between(sommeMoto, 17000, 100000))


ggplot(gros_rouleur_Moto, aes(x=sommeMoto, y=auto)) + 
  geom_point() 
  

gros_rouleur_voiture <- gr_sum %>% 
  group_by(auto) %>%
  filter( between(auto, 17000, 100000))

#############NEEDED#############
ggplot(gr_sum_test, aes(x=auto, y=sommeMoto)) + 
  geom_point(color = "darkblue") +
  labs(x="Km parcourus en voiture", 
       y="Km parcourus en moto", 
       title="Km parcourus en moto en fonction Km parcourus en voiture") 
#############NEEDED#############

show(gr_sum_test)

cor(gr_sum_test$auto,gr_sum_test$sommeMoto)

dim(gros_rouleur_voitureEtMoto)[1]
dim(petit_rouleur_voiture)[1]
dim(petit_rouleur_Moto)[1]
dim(gros_rouleur_voiture)[1]
dim(gros_rouleur_Moto)[1]

gr_sum2 <- gr_sum[,-1][,-1][,-1][,-1][,-1]
gr_sum2

ggplot(gr_sum, aes(x=auto, y=sommeMoto)) + 
  geom_point()+
  geom_smooth(method=lm)

resPCA<-PCA(gr_sum2)
plot.PCA(resPCA, axes=c(1, 2), choix="ind")

# utilisation des véhicules
pourcentage_utilisation_moto <- round((sum(gr_sum$sommeMoto) / (sum(gr_sum$sommeMoto) + sum(gr_sum$auto))) *100,2)
pourcentage_utilisation_auto <- round((sum(gr_sum$auto) / (sum(gr_sum$sommeMoto) + sum(gr_sum$auto))) *100,2)
pourcentage_utilisation <- data.frame(utilisation = c("voiture","moto"), pourcentage_utilisation = c(pourcentage_utilisation_auto,pourcentage_utilisation_moto))
View(pourcentage_utilisation)



diag_pourcentage_utilisation <- 
  ggplot(data=pourcentage_utilisation, aes(x=utilisation, y=pourcentage_utilisation, fill=factor(pourcentage_utilisation))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set1") +
  labs(fill="V?hicule", 
       x=NULL, 
       y="%", 
       title="Part de l'utilisation des véhicules" )+
  geom_text(aes(y = pourcentage_utilisation / 2, label = pourcentage_utilisation ), color = "white", size=5,check_overlap = TRUE) +
  theme(legend.position="none")
diag_pourcentage_utilisation