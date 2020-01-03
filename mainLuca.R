#----- INITIALISATION DATA -----#
install.packages("readxl") 
library("readxl")


bdd_2_roue <- read_excel("Data/bdd_2_roue.xls")
View(bdd_2_roue)

bdd_auto <- read_excel("Data/bdd_auto-modif.xlsx")
View(bdd_auto)

#renommage des 2 premieres colonnes de la base 2-roue
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...1"] <- "Identifiant"
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...2"] <- "Date"

#creation de la base de donnees conjointes (avec les personnes utilisant auto et 2-roue)
bdd <- merge(bdd_auto, bdd_2_roue, by="Identifiant")
View(bdd)
str(bdd)

#colnames(bdd_moto_2019)[colnames(bdd_moto_2019)=="IDENTIFIANT"] <- "Identifiant"
#creation de la 2eme base en prenant en compte les donnees 2019
#bdd_2019 <- merge(bdd, bdd_moto_2019, by="Identifiant")
#View(bdd_2019)

################################################################
#----- Installation des packages et chargement des library-----#
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)


#########################################
#----- Usage des voitures -----#

usage1 <- bdd %>%
  select(usage="Q15 [1]")
usage2 <- bdd %>%
  select(usage="Q15 [2]")
usage3 <- bdd %>%
  select(usage="Q15 [3]")
usage4 <- bdd %>%
  select(usage="Q15 [4]")
usage5 <- bdd %>%
  select(usage="Q15 [5]")
usage6 <- bdd %>%
  select(usage="Q15 [6]")

#correspond au nombre d'apparition de chaque type d'usage
usageVoiture <- rbind(usage1,usage2) %>% rbind(usage3) %>% rbind(usage4) %>% rbind(usage5) %>% rbind(usage6) %>%
  arrange(usage) %>%
  group_by(usage) %>%
  summarise("Nombre_de_personnes"= n())
View(usageVoiture)


#----- Usage des motos -----#
usage1 <- 0
usage2 <- 0
usage3 <- 0

#véhicule 1
v1a <- sum(bdd['Usage 1'] == 1)
v1b <- sum(bdd['Usage 1'] == 2)
v1c <- sum(bdd['Usage 1'] == 3)

#véhicule 2
v2a <- sum(bdd["...54"] == 1)
v2b <- sum(bdd['...54'] == 2)
v2c <- sum(bdd['...54'] == 3)

#véhicule 3
v3a <- sum(bdd['...65'] == 1)
v3b <- sum(bdd['...65'] == 2)
v3c <- sum(bdd['...65'] == 3)

#véhicule 4
v4a <- sum(bdd['...76'] == 1)
v4b <- sum(bdd['...76'] == 2)
v4c <- sum(bdd['...76'] == 3)

#véhicule 5
v5a <- sum(bdd['...88'] == 1)
v5b <- sum(bdd['...88'] == 2)
v5c <- sum(bdd['...88'] == 3)

#correspond au nombre d'apparition de chaque type d'usage
usageMotoV1 <- data.frame(usage=c(1,2,3), Nombre_de_personne=c(v1a,v1b,v1c))
usageMotoV2 <- data.frame(usage=c(1,2,3), Nombre_de_personne=c(v2a,v2b,v2c))
usageMotoV3 <- data.frame(usage=c(1,2,3), Nombre_de_personne=c(v3a,v3b,v3c))
usageMotoV4 <- data.frame(usage=c(1,2,3), Nombre_de_personne=c(v4a,v4b,v4c))
usageMotoV5 <- data.frame(usage=c(1,2,3), Nombre_de_personne=c(v5a,v5b,v5c))

#----- Usage table -----#

# début de la création d'une table avec tout les usages, ça correspondrait a la table utile 
# pour répondre a cet axe, il faut récupérer tout les usage et ainsi que le nombre de km associé

usage <- bdd %>%
  select(Identifiant, nbKmVoiture="Q17 [1]", usageVoiture1="Q15 [1]", usageVoiture2="Q15 [2]", 
         usageVoiture3="Q15 [3]", usageVoiture4="Q15 [4]", usageVoiture5="Q15 [5]", 
         usageVoiture6="Q15 [6]", usagev1="Usage 1", kmv1="Nb km 1", usagev2="...54", 
         kmv2="...56", usagev3="...65", kmv3="...67", usagev4="...76", kmv4="...78",
         usagev5="...88", kmv5="...90")



#-----Divison des usages-----#
usages <- bdd %>%
  select(Identifiant, usage_auto="Q15 [1]",usage_moto1="Usage 1")
usages[is.na(usages)] <- 0

#fonctions pour transformer les donnees numériques en texte
to_readable_auto <- function(code){
  if (code == 1)
    return ("Personnel")
  if (code == 2)
    return ("Domicile-travail")
  if (code == 3)
    return ("Travail")
  if (code == 4)
    return ("Courses")
  if (code == 5)
    return ("Personnel")
  if (code == 6)
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
    return ("Travail")
}
to_readable_moto_vector <- Vectorize(to_readable_moto, vectorize.args = "code")

#tableau avec id + usage principal de l'auto et de la moto1
usages <- usages %>% 
  mutate(Usage_auto = to_readable_auto_vector(usage_auto)) %>%
  mutate(Usage_moto = to_readable_moto_vector(usage_moto1))
