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

View(intro)

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

moyenne_km_an <- 17000
sup_moto <- gr_sum %>% 
  group_by(sommeMoto) %>%
  filter(sommeMoto >= moyenne_km_an)
inf_moto <- gr_sum %>% 
  group_by(sommeMoto) %>%
  filter(sommeMoto < moyenne_km_an)

sup_auto <- gr_sum %>% 
  group_by(auto) %>%
  filter(auto >= moyenne_km_an)
inf_auto <- gr_sum %>% 
  group_by(auto) %>%
  filter(auto < moyenne_km_an)

View(sup_auto)
View(inf_auto)

#TODO: afficher la part de gros rouleurs en moto 
#les gros rouleurs en voiture,
#les gros rouleurs en moto qui sont des gros rouleurs en voiture
#les gros rouleurs en voiture qui sont des gros rouleurs en moto
#à partir de ces diagrammes et calculs on déduit le reste
#str(sup_auto)

#resum_gros_rouleurs <- data.frame("rows_bdd" = dim(bdd)[1],
#                                  "rows_sup_auto" = dim(sup_auto)[1],
 #                                 "rows_sup_moto" = dim(sup_moto)[1],
  #                                "rows_inf_auto" = dim(inf_auto)[1],
   #                               "rows_inf_moto" = dim(inf_moto)[1],
    #                              "modalité de séparation" = "Moyenne fr: 17000 km")



effectifs <- data.frame(stat=c("Total étudié","Gros rouleurs en moto","Petits rouleurs en moto","Gros rouleurs en auto","Petits rouleurs en auto"),
                           effectif=c(dim(bdd)[1],dim(sup_moto)[1],dim(inf_moto)[1],dim(sup_auto)[1],dim(inf_auto)[1]))
effectifs

pourcentage_gros_rouleurs_fr <- effectifs[-5,][-3,][-1,] %>%
  mutate(pourcentage = round(effectif / dim(bdd)[1] * 100,2))
pourcentage_petits_rouleurs_fr <- effectifs[-4,][-2,][-1,] %>%
  mutate(pourcentage = round(effectif / dim(bdd)[1] * 100,2))

View(pourcentage_gros_rouleurs_fr)
View(pourcentage_petits_rouleurs_fr)

diag_pourcentage_GR_fr <- 
  ggplot(data=pourcentage_gros_rouleurs_fr, aes(x=stat, y=pourcentage, fill =factor(stat))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set1") +
  labs(fill="Véhicule", 
       x=NULL, 
       y="%", 
       title="Répartition du nombre de gros rouleurs en fonction du véhicule", 
       caption="Figure 2: Répartition du nombre de gros rouleurs en fonction du véhicule") +
  geom_text(aes(y = pourcentage / 2, label = pourcentage ), color = "white", size=5,check_overlap = TRUE)+ 
  theme(legend.position="none")
diag_pourcentage_GR_fr

diag_pourcentage_PR_fr <- 
  ggplot(data=pourcentage_petits_rouleurs_fr, aes(x=stat, y=pourcentage, fill =factor(stat))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set1") +
  labs(fill="Véhicule", 
       x=NULL, 
       y="%", 
       title="Répartition du nombre de petits rouleurs en fonction du véhicule", 
       caption="Figure 2: Répartition du nombre de petits rouleurs en fonction du véhicule") +
  geom_text(aes(y = pourcentage / 2, label = pourcentage ), color = "white", size=5,check_overlap = TRUE) + 
  theme(legend.position="none")
diag_pourcentage_PR_fr 




#################################################################################
#-----divison des usages-----#
usages <- bdd %>%
  select(Identifiant, usage_auto="Q16 [1]",usage_moto1="...66")
usages[is.na(usages)] <- 0

to_readable_auto <- function(code){
  if (code == 1)
    return ("Personnel")
  if (code == 2)
    return ("Domicile-travail")
  if (code == 3)
    return ("¨Travail")
  if (code == 4)
    return ("Personnel")
  if (code == 3)
    return ("¨Personnel")
  if (code == 4)
    return ("Personnel")
  
}
to_readable_auto_vector <- Vectorize(to_readable_auto, vectorize.args = "code")

to_readable_moto <- function(code){
  if (code == 0)
    return ("Non renseigné")
  if (code == 1)
    return ("Personnel")
  if (code == 2)
    return ("Domicile-travail")
  if (code == 3)
    return ("¨Travail")
}
to_readable_moto_vector <- Vectorize(to_readable_moto, vectorize.args = "code")

usages <- usages %>% 
  mutate(Usage_auto = to_readable_auto_vector(usage_auto)) %>%
  mutate(Usage_moto = to_readable_moto_vector(usage_moto1))

usages2 <- usages %>%
  filter(Usage_moto != "Non renseigné")

View(usages2)


