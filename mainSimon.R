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

usage1 <- bdd %>%
  select(usage = "Usage 1")
usage2 <- bdd %>%
  select(usage="...66")
usage3 <- bdd %>%
  select(usage="...77")
usage4 <- bdd %>%
  select(usage="...89")

#correspond au nombre d'apparition de chaque type d'usage
usageMoto <- rbind(usage1,usage2) %>% rbind(usage3) %>% rbind(usage4) %>%
  arrange(usage) %>%
  group_by(usage) %>%
  summarise("Nombre_de_personnes"= n())
View(usageMoto)

#----- Usage table -----#

# début de la création d'une table avec tout les usages, ça correspondrait a la table utile pour répondre a cet axe, il faut récupérer tout les usage et ainsi que le nombre de km associé

usage <- bdd %>%
         select(Identifiant, nbKmVoiture="Q17 [1]", usageVoiture1="Q15 [1]", usageVoiture2="Q15 [2]", usageVoiture3="Q15 [3]", usageVoiture4="Q15 [4]", usageVoiture5="Q15 [5]", usageVoiture6="Q15 [6]", 
                moto1="Nb km 1", moto2="...56", moto3="...67", moto4="...78")
