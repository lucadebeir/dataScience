install.packages("readxl") 
library("readxl")

bdd_auto <- read_excel("Data/bdd_auto-modif.xlsx")
bdd_2_roue <- read_excel("Data/bdd_2_roue.xls")

#View(bdd_auto_modif) 
#View(bdd_2_roue)

#renommage des 2 premières colonnes de la base 2-roue
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...1"] <- "Identifiant"
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...2"] <- "Date"

#création de la base de données conjointes (avec les personnes utilisant auto et 2-roue)
bdd <- merge(bdd_auto, bdd_2_roue, by="Identifiant")

