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

usageAutoPersonnelGlobal <- sum(usages['Usage_auto'] == 'Personnel')
usageAutoDomicileTravailGlobal <- sum(usages['Usage_auto'] == 'Domicile-travail')
usageAutoTravailGlobal <- sum(usages['Usage_auto'] == 'Travail')
usageAutoCourseGlobal <- sum(usages['Usage_auto'] == 'Courses')
nbUsager <- usageAutoPersonnelGlobal+usageAutoDomicileTravailGlobal+usageAutoTravailGlobal+usageAutoCourseGlobal

usageMotoPersonnelGlobal <- sum(usages['Usage_moto'] == 'Personnel')
usageMotoDomicileTravailGlobal <- sum(usages['Usage_moto'] == 'Domicile-travail')
usageMotoTravailGlobal <- sum(usages['Usage_moto'] == 'Travail')

#Camembert usage principal auto
usagePrincipalAuto <- data.frame(
  group = c("Personnel", "Domicile-travail", "Travail", "Courses"),
  value = c(round((usageAutoPersonnelGlobal/nbUsager),4), round((usageAutoDomicileTravailGlobal/nbUsager),4), 
            round((usageAutoTravailGlobal/nbUsager),4), round((usageAutoCourseGlobal/nbUsager),4))
)

library(extrafont)

loadfonts(device="win")

usagePrincipalAuto$group <- factor(usagePrincipalAuto$group, levels = rev(usagePrincipalAuto$group))


ggplot(data = usagePrincipalAuto, mapping = aes(x = factor(1), y = value, fill = group)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar(theta = "y") + 
  scale_fill_brewer(type = "seq",direction = -1, palette= "YlGnBu", guide = F) +
  geom_text(aes(x = c(1.3, 1.5, 1.3, 1.3), 
                y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label=paste(group,"\n",value*100, "%")), family = "Consolas")

#############################################################
#############################################################
#############################################################
#Camembert usage principal moto
usagePrincipalMoto <- data.frame(
  group = c("Domicile-travail", "Travail", "Personnel"),
  value = c(round((usageMotoDomicileTravailGlobal/nbUsager),4), round((usageMotoTravailGlobal/nbUsager),4),
            round((usageMotoPersonnelGlobal/nbUsager),4))
)

library(extrafont)

loadfonts(device="win")

usagePrincipalMoto$group <- factor(usagePrincipalMoto$group, levels = rev(usagePrincipalMoto$group))


ggplot(data = usagePrincipalMoto, mapping = aes(x = factor(1), y = value, fill = group)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar(theta = "y") + 
  scale_fill_brewer(type = "seq",direction = -1, palette= "YlGnBu", guide = F) +
  geom_text(aes(x = c(1.3, 1.5, 1.3), 
                y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label=paste(group,"\n",value*100, "%")), family = "Consolas")
#############################################################
#############################################################
#############################################################

#Camembert usage global auto
usageGlobalAutoPerso <- round(((usageVoiture[1,][2]+usageVoiture[5,][2]+usageVoiture[6,][2])/usageVoiture[7,][2]),4)
usageGlobalAutoDT <- round((usageVoiture[2,][2]/usageVoiture[7,][2]),4)
usageGlobalAutoT <- round((usageVoiture[3,][2]/usageVoiture[7,][2]),4)
usageGlobalAutoC <- round((usageVoiture[4,][2]/usageVoiture[7,][2]),4)

usageGlobalAuto <- data.frame(
  group = c("Personnel", "Domicile-travail", "Travail", "Courses"),
  value = c(usageGlobalAutoPerso[1,], 
            usageGlobalAutoDT[1,], 
            usageGlobalAutoT[1,], 
            usageGlobalAutoC[1,])
)

library(extrafont)

loadfonts(device="win")

usageGlobalAuto$group <- factor(usageGlobalAuto$group, levels = rev(usageGlobalAuto$group))


ggplot(data = usageGlobalAuto, mapping = aes(x = factor(1), y = value, fill = group)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar(theta = "y") + 
  scale_fill_brewer(type = "seq",direction = -1, palette= "YlGnBu", guide = F) +
  geom_text(aes(x = c(1.3, 1.5, 1.3, 1.3), 
                y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label=paste(group,"\n",value*100, "%")), family = "Consolas")





######## Données pour diviser les individus en 2 groupes (gros rouleur VS petit rouleur) ############

grosRouleurAuto <- 0
grosRouleurMoto <- 0
petitRouleurAuto <- 0
petitRouleurMoto <- 0



repartitionRouleur <- function(code) {
  if (code > 17000)
    return (1)
  else
    return (0)
}

for (i in (bdd$`Q17 [1]`))
  if (repartitionRouleur(strtoi(i)) == 1) {
    grosRouleurAuto <- grosRouleurAuto + 1
  } else {
    petitRouleurAuto <- petitRouleurAuto + 1
  }

    
for (i in (bdd$`Nb km 1`))
  if (repartitionRouleur(strtoi(i)) == 1) {
    grosRouleurMoto <- grosRouleurMoto + 1
  } else {
    petitRouleurMoto <- petitRouleurMoto + 1
  }


### Tableau de contingence ####

usageAutoPersonnelGrosRouleur <- 0
usageAutoTravailGrosRouleur <- 0
usageAutoDTGrosRouleur <- 0
usageAutoCourseGrosRouleur <- 0

usageAutoPersonnelPetitRouleur <- 0
usageAutoTravailPetitRouleur <- 0
usageAutoDTPetitRouleur <- 0
usageAutoCoursePetitRouleur <- 0

usageMotoPersonnelGrosRouleur <- 0
usageMotoTravailGrosRouleur <- 0
usageMotoDTGrosRouleur <- 0

usageMotoPersonnelPetitRouleur <- 0
usageMotoTravailPetitRouleur <- 0
usageMotoDTPetitRouleur <- 0

#répartition pour les voitures avec le premier choix coché

#pas super mais pas le choix

for (i in 1:nrow(bdd))
  if (bdd$`Q15 [1]`[i] == 1 || bdd$`Q15 [1]`[i] == 5 || bdd$`Q15 [1]`[i] == 6) {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      usageAutoPersonnelGrosRouleur <- usageAutoPersonnelGrosRouleur + 1
    } else {
      usageAutoPersonnelPetitRouleur <- usageAutoPersonnelPetitRouleur + 1
    }
  } else if (bdd$`Q15 [1]`[i] == 2) {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      usageAutoDTGrosRouleur <- usageAutoDTGrosRouleur + 1
    } else {
      usageAutoDTPetitRouleur <- usageAutoDTPetitRouleur + 1
    }
  } else if (bdd$`Q15 [1]`[i] == 3) {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      usageAutoTravailGrosRouleur <- usageAutoTravailGrosRouleur + 1
    } else {
      usageAutoTravailPetitRouleur <- usageAutoTravailPetitRouleur + 1
    }
  } else {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      usageAutoCourseGrosRouleur <- usageAutoCourseGrosRouleur + 1
    } else {
      usageAutoCoursePetitRouleur <- usageAutoCoursePetitRouleur + 1
    }
  }
  
for (i in 1:nrow(bdd))  
  if (bdd$`Usage 1`[i] == 1) {
    if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
      usageMotoPersonnelGrosRouleur <- usageMotoPersonnelGrosRouleur + 1
    } else {
      usageMotoPersonnelPetitRouleur <- usageMotoPersonnelPetitRouleur + 1
    }
  } else if (bdd$`Usage 1`[i] == 2) {
    if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
      usageMotoDTGrosRouleur <- usageMotoDTGrosRouleur + 1
    } else {
      usageMotoDTPetitRouleur <- usageMotoDTPetitRouleur + 1
    }
  } else {
    if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
      usageMotoTravailGrosRouleur <- usageMotoTravailGrosRouleur + 1
    } else {
      usageMotoTravailPetitRouleur <- usageMotoTravailPetitRouleur + 1
    }
  }


## Création du tableau de contingence ##

tabContingence <- data.frame(
             AutoUPersonnel=c(usageAutoPersonnelGrosRouleur,usageAutoPersonnelPetitRouleur), 
             AutoUDT=c(usageAutoDTGrosRouleur,usageAutoDTPetitRouleur), 
             AutoUTravail=c(usageAutoTravailGrosRouleur, usageAutoTravailPetitRouleur), 
             AutoUCourse=c(usageAutoCourseGrosRouleur,usageAutoCoursePetitRouleur),
             MotoUPersonnel=c(usageMotoPersonnelGrosRouleur,usageMotoPersonnelPetitRouleur), 
             MotoUDT=c(usageMotoDTGrosRouleur,usageMotoDTPetitRouleur), 
             MotoUTravail=c(usageMotoTravailGrosRouleur, usageMotoTravailPetitRouleur),
             row.names=c("Gros rouleur","Petit rouleur")
)

tabContingenceAvecSomme <- data.frame(
  AutoUPersonnel=c(usageAutoPersonnelGrosRouleur,usageAutoPersonnelPetitRouleur,usageAutoPersonnelGlobal), 
  AutoUDT=c(usageAutoDTGrosRouleur,usageAutoDTPetitRouleur,usageAutoDomicileTravailGlobal), 
  AutoUTravail=c(usageAutoTravailGrosRouleur, usageAutoTravailPetitRouleur,usageAutoTravailGlobal), 
  AutoUCourse=c(usageAutoCourseGrosRouleur,usageAutoCoursePetitRouleur,usageAutoCourseGlobal),
  MotoUPersonnel=c(usageMotoPersonnelGrosRouleur,usageMotoPersonnelPetitRouleur,usageMotoPersonnelGlobal), 
  MotoUDT=c(usageMotoDTGrosRouleur,usageMotoDTPetitRouleur,usageMotoDomicileTravailGlobal), 
  MotoUTravail=c(usageMotoTravailGrosRouleur, usageMotoTravailPetitRouleur,usageMotoTravailGlobal),
  Somme=c(grosRouleurAuto+grosRouleurMoto,petitRouleurAuto+petitRouleurMoto,
          grosRouleurAuto+grosRouleurMoto+petitRouleurAuto+petitRouleurMoto),
  row.names=c("Gros rouleur","Petit rouleur", "Somme")
)

resCA <- CA(tabContingence)
#une seule dimension -> sert à R..


#

###############

tabContingenceAuto <- data.frame(
  Personnel=c(usageAutoPersonnelGrosRouleur,
              usageAutoPersonnelPetitRouleur), 
  DomicileTravail=c(usageAutoDTGrosRouleur,
                    usageAutoDTPetitRouleur), 
  Travail=c(usageAutoTravailGrosRouleur,
            usageAutoTravailPetitRouleur),
  Course=c(usageAutoCourseGrosRouleur,
           usageAutoCoursePetitRouleur),
  row.names=c("Gros rouleur Auto", "Petit rouleur Auto")
)

resCA<-CA(tabContingenceAuto)

tabContingenceMoto <- data.frame(
  Personnel=c(usageMotoPersonnelGrosRouleur,
              usageMotoPersonnelPetitRouleur), 
  DomicileTravail=c(usageMotoDTGrosRouleur,
                    usageMotoDTPetitRouleur), 
  Travail=c(usageMotoTravailGrosRouleur,
            usageMotoTravailPetitRouleur), 
  row.names=c("Gros rouleur Moto", "Petit rouleur Moto")
)

resCA<-CA(tabContingenceMoto)

########AFC des familles

seulSansEnfantGrosRouleurAuto <- 0
seulAvecEnfantGrosRouleurAuto <- 0
enCoupleSansEnfantGrosRouleurAuto <- 0
enCoupleAvecEnfantGrosRouleurAuto <- 0

seulSansEnfantGrosRouleurMoto <- 0
seulAvecEnfantGrosRouleurMoto <- 0
enCoupleSansEnfantGrosRouleurMoto <- 0
enCoupleAvecEnfantGrosRouleurMoto <- 0

seulSansEnfantPetitRouleurAuto <- 0
seulAvecEnfantPetitRouleurAuto <- 0
enCoupleSansEnfantPetitRouleurAuto <- 0
enCoupleAvecEnfantPetitRouleurAuto <- 0

seulSansEnfantPetitRouleurMoto <- 0
seulAvecEnfantPetitRouleurMoto <- 0
enCoupleSansEnfantPetitRouleurMoto <- 0
enCoupleAvecEnfantPetitRouleurMoto <- 0

autreAutoGrosRouleur <- 0
autreAutoPetitRouleur <- 0
autreMotoGrosRouleur <- 0
autreMotoPetitRouleur <- 0

situation_familiale <- function(num){
  if (num == "1")
    return ("Seul(e) sans enfant")
  if (num == "2")
    return ("Seul(e) avec enfant(s)")
  if (num == "3")
    return ("En couple sans enfant")
  if (num == "4")
    return ("En couple avec enfant(s)")
  if (num == "5")
    return ("Autre")
}

for (i in 1:nrow(bdd))
  if (situation_familiale(bdd$`Situation familiale`[i]) == "Seul(e) sans enfant") {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      seulSansEnfantGrosRouleurAuto <- seulSansEnfantGrosRouleurAuto + 1
    } else {
      seulSansEnfantPetitRouleurAuto <- seulSansEnfantPetitRouleurAuto + 1
    }
  } else if (situation_familiale(bdd$`Situation familiale`[i]) == "Seul(e) avec enfant(s)") {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      seulAvecEnfantGrosRouleurAuto <- seulAvecEnfantGrosRouleurAuto + 1
    } else {
      seulAvecEnfantPetitRouleurAuto <- seulAvecEnfantPetitRouleurAuto + 1
    }
  } else if (situation_familiale(bdd$`Situation familiale`[i]) == "En couple sans enfant") {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      enCoupleSansEnfantGrosRouleurAuto <- enCoupleSansEnfantGrosRouleurAuto + 1
    } else {
      enCoupleSansEnfantPetitRouleurAuto <- enCoupleSansEnfantPetitRouleurAuto + 1
    }
  } else if (situation_familiale(bdd$`Situation familiale`[i]) == "En couple avec enfant(s)") {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      enCoupleAvecEnfantGrosRouleurAuto <- enCoupleAvecEnfantGrosRouleurAuto + 1
    } else {
      enCoupleAvecEnfantPetitRouleurAuto <- enCoupleAvecEnfantPetitRouleurAuto + 1
    }
  } else {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      autreAutoGrosRouleur <- autreAutoGrosRouleur + 1
    } else {
      autreAutoPetitRouleur <- autreAutoPetitRouleur + 1
    }
  }

for (i in 1:nrow(bdd))  
  if (situation_familiale(bdd$`Situation familiale`[i]) == "Seul(e) sans enfant") {
    if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
      seulSansEnfantGrosRouleurMoto <- seulSansEnfantGrosRouleurMoto + 1
    } else {
      seulSansEnfantPetitRouleurMoto <- seulSansEnfantPetitRouleurMoto + 1
    }
  } else if (situation_familiale(bdd$`Situation familiale`[i]) == "Seul(e) avec enfant(s)") {
    if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
      seulAvecEnfantGrosRouleurMoto <- seulAvecEnfantGrosRouleurMoto + 1
    } else {
      seulAvecEnfantPetitRouleurMoto <- seulAvecEnfantPetitRouleurMoto + 1
    }
  } else if (situation_familiale(bdd$`Situation familiale`[i]) == "En couple sans enfant") {
    if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
      enCoupleSansEnfantGrosRouleurMoto <- enCoupleSansEnfantGrosRouleurMoto + 1
    } else {
      enCoupleSansEnfantPetitRouleurMoto <- enCoupleSansEnfantPetitRouleurMoto + 1
    }
  } else if (situation_familiale(bdd$`Situation familiale`[i]) == "En couple avec enfant(s)") {
    if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
      enCoupleAvecEnfantGrosRouleurMoto <- enCoupleAvecEnfantGrosRouleurMoto + 1
    } else {
      enCoupleAvecEnfantPetitRouleurMoto <- enCoupleAvecEnfantPetitRouleurMoto + 1
    }
  } else {
    if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
      autreMotoGrosRouleur <- autreMotoGrosRouleur + 1
    } else {
      autreMotoPetitRouleur <- autreMotoPetitRouleur + 1
    }
  }


tabContingenceFamille <- data.frame(
  SeulSansEnfant=c(seulSansEnfantGrosRouleurAuto,seulSansEnfantGrosRouleurMoto,
              seulSansEnfantPetitRouleurAuto,seulSansEnfantPetitRouleurMoto), 
  SeulAvecEnfant=c(seulAvecEnfantGrosRouleurAuto,seulAvecEnfantGrosRouleurMoto,
                   seulAvecEnfantPetitRouleurAuto,seulAvecEnfantPetitRouleurMoto),
  EnCoupleSansEnfant=c(enCoupleSansEnfantGrosRouleurAuto,enCoupleSansEnfantGrosRouleurMoto,
                    enCoupleSansEnfantPetitRouleurAuto,enCoupleSansEnfantPetitRouleurMoto), 
  EnCoupleAvecEnfant=c(enCoupleAvecEnfantGrosRouleurAuto,enCoupleAvecEnfantGrosRouleurMoto,
            enCoupleAvecEnfantPetitRouleurAuto,enCoupleAvecEnfantPetitRouleurMoto), 
  Autre=c(autreAutoGrosRouleur,autreMotoGrosRouleur,autreAutoPetitRouleur,autreMotoPetitRouleur),
  row.names=c("Gros rouleur Auto", "Gros rouleur Moto", "Petit rouleur Auto", "Petit rouleur Moto")
)

resAFCFamille <- CA(tabContingenceFamille)



###### AFC rouleur auto/moto

GrosRouleurAutoetMoto <- 0
GrosRouleurAutoetPetitRouleurMoto <- 0
GrosRouleurMotoetPetitRouleurAuto <- 0
PetitRouleurAutoetMoto <- 0

for (i in 1:nrow(bdd))
  if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 1) {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      GrosRouleurAutoetMoto <- GrosRouleurAutoetMoto + 1
    } else {
      GrosRouleurMotoetPetitRouleurAuto <- GrosRouleurMotoetPetitRouleurAuto + 1
    }
  } else if (repartitionRouleur(strtoi(bdd$`Nb km 1`[i])) == 0) {
    if (repartitionRouleur(strtoi(bdd$`Q17 [1]`[i])) == 1) {
      GrosRouleurAutoetPetitRouleurMoto <- GrosRouleurAutoetPetitRouleurMoto + 1
    } else {
      PetitRouleurAutoetMoto <- PetitRouleurAutoetMoto + 1
    }
  }

tabContingenceRouleur <- data.frame(
  GrosRouleurAuto=c(GrosRouleurAutoetMoto,GrosRouleurAutoetPetitRouleurMoto), 
  PetitRouleurAuto=c(GrosRouleurMotoetPetitRouleurAuto,PetitRouleurAutoetMoto),
  row.names=c("GrosRouleurMoto", "PetitRouleurMoto")
)

tabContingenceRouleur2 <- data.frame(
  Auto=c(grosRouleurAuto,petitRouleurAuto), 
  Moto=c(grosRouleurMoto,petitRouleurMoto),
  row.names=c("Gros rouleur", "Petit rouleur")
)

resAFCRouleur2 <- CA(tabContingenceRouleur2)



