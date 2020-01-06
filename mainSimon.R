
################################################################
#----- Installation des packages et chargement des library-----#
install.packages("ggplot2")
install.packages("gplots")
install.packages("dplyr")
install.packages("readxl") 
install.packages(c("FactoMineR", "factoextra"))
remove.packages("ggplot2")
remove.packages("gplots")

library(gplots)
library(ggplot2)
library(dplyr)
library("readxl")
library("FactoMineR")
library("factoextra")

#----- INITIALISATION DATA -----#
bdd_2_roue <- read_excel("Data/bdd_2_roue.xls")

bdd_auto <- read_excel("Data/bdd_auto-modif.xlsx")

#renommage des 2 premieres colonnes de la base 2-roue
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...1"] <- "Identifiant"
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...2"] <- "Date"

#creation de la base de donnees conjointes (avec les personnes utilisant auto et 2-roue)
bdd <- merge(bdd_auto, bdd_2_roue, by="Identifiant")
str(bdd)

#colnames(bdd_moto_2019)[colnames(bdd_moto_2019)=="IDENTIFIANT"] <- "Identifiant"
#creation de la 2eme base en prenant en compte les donnees 2019
#bdd_2019 <- merge(bdd, bdd_moto_2019, by="Identifiant")
#View(bdd_2019)




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

usagecor <- cbind(usage1,usage2,usage3,usage4,usage5,usage6)
# on ne veux pas que les 3 usages personnels s'additionne cela donnerais l'impression que 
#que cet usage est plus représenté que les autre,usage3,usage4,usage5,usage6
to_filter_Usage <- function(usage){
  dim <- dim(usage)[1]
  for (i in 1:dim){
    for(j in 1:6){
      if(j != 1 && (usage[i,1] %in% c(1,5,6)) && (usage[i,j] %in% c(1,5,6))){
          usage[i,j] <- NA
      }
      if(j != 2 && (usage[i,2] %in% c(1,5,6)) && (usage[i,j] %in% c(1,5,6))){
        usage[i,j] <- NA
      }
      if(j != 3 && (usage[i,3] %in% c(1,5,6)) && (usage[i,j] %in% c(1,5,6))){
        usage[i,j] <- NA
      }
      if(j != 4 && (usage[i,4] %in% c(1,5,6)) && (usage[i,j] %in% c(1,5,6))){
        usage[i,j] <- NA
      }
      if(j != 5 && (usage[i,5] %in% c(1,5,6)) && (usage[i,j] %in% c(1,5,6))){
        usage[i,j] <- NA
      }
    }
  }
  return (usage)
}

usagecor <- to_filter_Usage(usagecor)

#attribution des valeur corigé au different usage
usage1[,1] <- usagecor[,1]
usage2[,1] <- usagecor[,2]
usage3[,1] <- usagecor[,3]
usage4[,1] <- usagecor[,4]
usage5[,1] <- usagecor[,5]
usage6[,1] <- usagecor[,6]

#correspond au nombre d'apparition de chaque type d'usage
usageVoiture <-  rbind(usage1,usage2) %>% rbind(usage3) %>% rbind(usage4) %>% rbind(usage5) %>% rbind(usage6) %>%
        arrange(usage) %>%
        group_by(usage) %>%
        summarise("Voiture"= n())
View(usageVoiture)

#regrouper les type d'utilisation des voiture pour qu'ils correspondent au moto
usageVoiture[1,2] <- usageVoiture[1,2] + usageVoiture[5,2] + usageVoiture[6,2]

#----- Usage des motos -----#

usage1 <- bdd %>%
  select(usage = "Usage 1")
usage2 <- bdd %>%
  select(usage = "...55")
usage3 <- bdd %>%
  select(usage="...66")
usage4 <- bdd %>%
  select(usage="...77")
usage5 <- bdd %>%
  select(usage="...89")

#correspond au nombre d'apparition de chaque type d'usage
# c'est l'usage total il n'est plus utile
usageMoto <- rbind(usage1,usage2) %>% rbind(usage3) %>% rbind(usage4) %>% rbind(usage5) %>%
  arrange(usage) %>%
  group_by(usage) %>%
  summarise("Nombre_de_personnes"= n())
View(usageMoto)


#-- AFC Usage par véhicule --#


#Usage par moto
usageMoto1 <- usage1 %>%
  arrange(usage) %>%
  group_by(usage) %>%
  summarise("Moto_1"= n())
usageMoto1[4,2] <- 0

usageMoto2 <- usage2 %>%
  arrange(usage) %>%
  group_by(usage) %>%
  summarise("Moto_2"= n())
usageMoto2[4,2] <- 0

usageMoto3 <- usage3 %>%
  arrange(usage) %>%
  group_by(usage) %>%
  summarise("Moto_3"= n())
usageMoto3[4,2] <- 0

usageMoto4 <- usage4 %>%
  arrange(usage) %>%
  group_by(usage) %>%
  summarise("Moto_4"= n())
usageMoto4[4,2] <- 0

usageMoto5 <- usage5 %>%
  filter() %>%
  arrange(usage) %>%
  group_by(usage) %>%
  summarise("Moto_5"= n())
usageMoto5[4,2] <- 0
#probleme avec les valeur manquante
usageMoto5[2:4,2] <- 0


AFC_usage <- usageVoiture[1:4,2]
AFC_usage[,2] <- usageMoto1[,2]
AFC_usage[,3] <- usageMoto2[,2]
AFC_usage[,4] <- usageMoto3[,2]
AFC_usage[,5] <- usageMoto4[,2]
AFC_usage[,6] <- usageMoto5[,2]

#renome les ligne
#rownames(AFC_usage) <- c("Personnel","Domicile-travail", "Travail", "Course")

#on enlève les course, on pourra essayer en les rajoutant
AFC_usage <- AFC_usage[1:3,]
rownames(AFC_usage) <- c("Loisir","Domicile-travail", "Travail")

# 1. convertir les données en tant que table
dt <- as.table(as.matrix (AFC_usage))

View(dt)
# 2. Graphique
balloonplot(t (dt), main = "AFC_usage", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)

# test du khi²
chisq <- chisq.test (AFC_usage)

#mise en place de l'AFC
res_AFC <- CA (AFC_usage, graph = TRUE) 

#analyse des valeur propre
eig.val <- get_eigenvalue (res_AFC)


# Contributions des colonnes à la dimension 1
fviz_contrib(res_AFC, choice = "col", axes = 1, top = 10)
# Contributions des colonnes à la dimension 2
fviz_contrib(res_AFC, choice = "col", axes = 2, top = 10)

# Contributions des lignes à la dimension 1
fviz_contrib(res_AFC, choice = "row", axes = 1, top = 10)
# Contributions des lignes à la dimension 2
fviz_contrib(res_AFC, choice = "row", axes = 2, top = 10)

# repel = TRUE pour éviter le chevauchement de texte
fviz_ca_biplot (res_AFC)
ggplot(res_AFC)








#----- Usage table -----#

# début de la création d'une table avec tout les usages, ça correspondrait a la table utile pour répondre a cet axe, il faut récupérer tout les usage et ainsi que le nombre de km associé


usage <- bdd %>%
  select(Identifiant, nbKmVoiture="Q17 [1]", usageVoiture1="Q15 [1]", usageVoiture2="Q15 [2]", 
         usageVoiture3="Q15 [3]", usageVoiture4="Q15 [4]", usageVoiture5="Q15 [5]", 
         usageVoiture6="Q15 [6]", usageM1="Usage 1", kmM1="Nb km 1", usageM2="...55", 
         kmM2="...56", usageM3="...66", kmM3="...67", usageM4="...77", kmM4="...78",
         usageM5="...89", kmM5="...90")


#fonctions pour transformer les donnees en qq chose de lisible
to_readable_auto <- function(code){
  if (is.na(code))
    return ("")
  if (code == 1)
    return ("Personnel")
  if (code == 2)
    return ("Domicile-travail")
  if (code == 3)
    return ("Travail")
  if (code == 4)
    return ("Course")
  if (code == 5)
    return ("Personnel")
  if (code == 6)
    return ("Personnel")
}
to_readable_auto_vector <- Vectorize(to_readable_auto, vectorize.args = "code")

to_readable_moto <- function(code){
  if (is.na(code))
    return ("")
  if (code == 1)
    return ("Personnel")
  if (code == 2)
    return ("Domicile-travail")
  if (code == 3)
    return ("Travail")
}
to_readable_moto_vector <- Vectorize(to_readable_moto, vectorize.args = "code")

usage_readable <- usage %>% 
  mutate(usageVoiture1 = to_readable_auto_vector(usageVoiture1)) %>%
  mutate(usageVoiture2 = to_readable_auto_vector(usageVoiture2)) %>%
  mutate(usageVoiture3 = to_readable_auto_vector(usageVoiture3)) %>%
  mutate(usageVoiture4 = to_readable_auto_vector(usageVoiture4)) %>%
  mutate(usageVoiture5 = to_readable_auto_vector(usageVoiture5)) %>%
  mutate(usageVoiture6 = to_readable_auto_vector(usageVoiture6)) %>%
  mutate(usageM1 = to_readable_moto_vector(usageM1)) %>%
  mutate(usageM2 = to_readable_moto_vector(usageM2)) %>%
  mutate(usageM3 = to_readable_moto_vector(usageM3)) %>%
  mutate(usageM4 = to_readable_moto_vector(usageM4)) %>%
  mutate(usageM5 = to_readable_moto_vector(usageM5)) 
  

View(usage)
View(usage_readable)


#-- données usage plot introduction --#

usageVoiturePLot <- usageVoiture[1:3,2] 
usageVoiturePLot <- mutate(usageVoiturePLot ,prop = round(Voiture / sum(usageVoiturePLot$Voiture) *100,2))%>%
                    mutate(ypos = cumsum(prop)- 0.5*prop ) 

dt <- data.frame(
  type =  c("Loisir","Domicile-travail", "Travail"),
  value = usageVoiturePLot[,1],
  prop = usageVoiturePLot[,2],
  ypos = usageVoiturePLot[,3]
)

#On ne met rien sur l'axe x pour pouvoir en faire un cercle
diag_intro <- ggplot(dt, aes(x = "", y = prop, fill = factor(type))) + 
  geom_bar(width = 1,stat = "identity") + 
  theme_void() +
  labs(fill="Type d'usage", 
       x=NULL, 
       y=NULL, 
       title="Répartition des usages des autos", 
       caption="Figure 1: Diagramme R, répartition des usages des autos") +
  geom_text(aes(y = ypos, label = prop), color = "white", size=5,check_overlap = TRUE) +
  scale_fill_brewer(palette="Dark2") +
  coord_polar("y", start=0)
diag_intro


usageMotoPLot <- usageMoto[1:3,2]
usageMotoPLot <- mutate(usageMotoPLot ,prop = round(Nombre_de_personnes / sum(usageMotoPLot$Nombre_de_personnes) *100,2))%>%
  mutate(ypos = cumsum(prop)- 0.5*prop ) 

dtm <- data.frame(
  type =  c("Loisir","Domicile-travail", "Travail"),
  value = usageMotoPLot[,1],
  prop = usageMotoPLot[,2],
  ypos = usageMotoPLot[,3]
)

#On ne met rien sur l'axe x pour pouvoir en faire un cercle
diag_intro_moto <- ggplot(dtm, aes(x = "", y = prop, fill = factor(type))) + 
  geom_bar(width = 1,stat = "identity") + 
  theme_void() +
  labs(fill="Type d'usage", 
       x=NULL, 
       y=NULL, 
       title="Répartition des usages des motos", 
       caption="Figure 2: Diagramme R, répartition des usages des motos") +
  geom_text(aes(y = ypos, label = prop), color = "white", size=5,check_overlap = TRUE) +
  scale_fill_brewer(palette="Dark2") +
  coord_polar("y", start=0)
diag_intro_moto


usagePLot <- usageMoto[1:3,2] + usageVoiture[1:3,2]
usagePLot <- mutate(usagePLot ,prop = round(Nombre_de_personnes / sum(usagePLot$Nombre_de_personnes) *100,2))%>%
  mutate(ypos = cumsum(prop)- 0.5*prop ) 
usagePLot
dtall <- data.frame(
  type =  c("Loisir","Domicile-travail", "Travail"),
  value = usagePLot[,1],
  prop = usagePLot[,2],
  ypos = usagePLot[,3]
)

#On ne met rien sur l'axe x pour pouvoir en faire un cercle
diag_intro_moto <- ggplot(dtall, aes(x = "", y = prop, fill = factor(type))) + 
  geom_bar(width = 1,stat = "identity") + 
  theme_void() +
  labs(fill="Type d'usage", 
       x=NULL, 
       y=NULL, 
       title="Répartition des usages pour les motos et les autos", 
       caption="Figure 2: Diagramme R, répartition des usages pour les motos et les autos") +
  geom_text(aes(y = ypos, label = prop), color = "white", size=5,check_overlap = TRUE) +
  scale_fill_brewer(palette="Dark2") +
  coord_polar("y", start=0)
diag_intro_moto


