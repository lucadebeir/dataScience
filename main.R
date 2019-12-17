#----- INITIALISATION DATA -----#
install.packages("readxl") 
library("readxl")

bdd_auto <- read_excel("C:/Users/alexa/Documents/R/dataScience/Data/bdd_auto-modif.xlsx")
bdd_2_roue <- read_excel("C:/Users/alexa/Documents/R/dataScience/Data/bdd_2_roue.xls")

#View(bdd_auto_modif) 
#View(bdd_2_roue)

#renommage des 2 premiÃ¨res colonnes de la base 2-roue
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...1"] <- "Identifiant"
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...2"] <- "Date"

#creation de la base de donnees conjointes (avec les personnes utilisant auto et 2-roue)
bdd <- merge(bdd_auto, bdd_2_roue, by="Identifiant")
View(bdd)


################################################################
#----- Installation des packages et chargement des library-----#
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)


#########################################
#----- Diagramme du nombre d'autos -----#
intro <- bdd %>%
  select(Identifiant, Q1="Q1 [1]") %>%
  arrange(Q1) %>%
  group_by(Q1) %>%
  summarise("Nombre_de_personnes"= n())

#View(intro)

intro2 <- intro %>%
  arrange(desc(Q1)) %>%
  mutate(prop = round(Nombre_de_personnes / sum(intro$Nombre_de_personnes) *100,2)) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
  
#View(intro2)

#On ne met rien sur l'axe x pour pouvoir en faire un cercle
diag_intro <- ggplot(intro2, aes(x = "", y = prop, fill = factor(Q1))) + 
  geom_bar(width = 1,stat = "identity") + 
  theme_void() +
  labs(fill="Nombre d'autos", 
       x=NULL, 
       y=NULL, 
       title="Répartition du nombre d'autos", 
       caption="Figure 1: Diagramme R, répartition du nombre d'autos") +
  geom_text(aes(y = ypos, label = prop), color = "white", size=5,check_overlap = TRUE) +
  scale_fill_brewer(palette="Set1") +
  coord_polar("y", start=0)
#diag_intro


#####################################################
#----- Diagramme des gros rouleurs en moto/auto-----#

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

sup <- gr_sum %>% 
  group_by(sommeMoto) %>%
  filter(sommeMoto >= 17000)
inf <- gr_sum %>% 
  group_by(sommeMoto) %>%
  filter(sommeMoto < 17000)

View(sup)
View(inf)

#################################################################################
#----- Diagramme des gros rouleurs en auto parmis les gros rouleurs en moto-----#



