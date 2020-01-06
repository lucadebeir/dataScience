#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("readxl")
library(ggplot2)
library(dplyr)

bdd_auto <- read_excel("C:/Users/alexa/Documents/R/dataScience/Data/bdd_auto-modif.xlsx")
bdd_2_roue <- read_excel("C:/Users/alexa/Documents/R/dataScience/Data/bdd_2_roue.xls")
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...1"] <- "Identifiant"
colnames(bdd_2_roue)[colnames(bdd_2_roue)=="...2"] <- "Date"
bdd <- merge(bdd_auto, bdd_2_roue, by="Identifiant")


gros_rouleurs <- bdd %>%
    select(Identifiant, moto1="Nb km 1", moto2="...56", moto3="...67", moto4="...78", kmAuto="Q17 [1]", Nombre_personnes_foyer = "Nombre personnes au foyer",Situation_familiale = "Situation familiale",Nombre_enfants = "Nombre enfants")

gros_rouleurs$moto1 <- as.numeric(as.character(gros_rouleurs$moto1))
gros_rouleurs$moto2 <- as.numeric(as.character(gros_rouleurs$moto2))
gros_rouleurs$moto3 <- as.numeric(as.character(gros_rouleurs$moto3))
gros_rouleurs$moto4 <- as.numeric(as.character(gros_rouleurs$moto4))
gros_rouleurs$Nombre_enfants <- as.numeric(as.character(gros_rouleurs$Nombre_enfants))

gros_rouleurs[is.na(gros_rouleurs)] <- 0
gr_sum <- gros_rouleurs %>% 
    mutate(kmMoto = rowSums(.[2:5])) %>%
    arrange(desc(kmMoto))


gr_sum_total <- gr_sum %>%
    mutate(kmTotaux = kmMoto + kmAuto)

isGrosRouleur_moto <- function(km){
    if (km <= moyenne_km_an)
        return (0)
    if (km > moyenne_km_an)
        return (1)
}
isGrosRouleur_moto_vector <- Vectorize(isGrosRouleur_moto, vectorize.args = "km")

isGrosRouleur_auto <- function(km){
    if (km <= moyenne_km_an)
        return (0)
    if (km > moyenne_km_an)
        return (1)
}
isGrosRouleur_auto_vector <- Vectorize(isGrosRouleur_auto, vectorize.args = "km")

isAssure <- function(num){
    if (num == "1")
        return (1)
    if (num == "2")
        return (0)
}
isAssure_vector <- Vectorize(isAssure, vectorize.args = "num")

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
situation_familiale_vector <- Vectorize(situation_familiale, vectorize.args = "num")

is_en_couple <- function(situation){
    if (situation == "Seul(e) sans enfant")
        return ("Celibataire")
    if (situation == "Seul(e) avec enfant(s)")
        return ("Celibataire")
    if (situation == "En couple sans enfant")
        return ("En couple")
    if (situation == "En couple avec enfant(s)")
        return ("En couple")
    if (situation == "Autre")
        return ("Celibataire")
    
}
is_en_couple_vector <- Vectorize(is_en_couple, vectorize.args = "situation")

hasEnfants <- function(situation){
    if (situation == "Seul(e) sans enfant")
        return (0)
    if (situation == "Seul(e) avec enfant(s)")
        return (1)
    if (situation == "En couple sans enfant")
        return (0)
    if (situation == "En couple avec enfant(s)")
        return (1)
    if (situation == "Autre")
        return (0)
    
}
hasEnfants_vector <- Vectorize(hasEnfants, vectorize.args = "situation")

type_rouleur <- gr_sum_total %>% 
    mutate(gros_rouleur_moto = isGrosRouleur_moto_vector(kmMoto)) %>%
    mutate(gros_rouleur_auto = isGrosRouleur_auto_vector(kmAuto)) %>%
    mutate(situation_familiale = situation_familiale_vector(Situation_familiale))%>%
    mutate(en_couple = is_en_couple_vector(situation_familiale)) %>%
    mutate(avec_enfant = hasEnfants_vector(situation_familiale)) %>%
    select(Identifiant,gros_rouleur_moto,gros_rouleur_auto,en_couple,Nombre_enfants,kmTotaux,kmAuto,kmMoto)


situation_couple <- type_rouleur %>%
    group_by(en_couple) %>%
    summarise(moy_km_moto = round(mean(kmAuto, na.rm=TRUE),2),
              moy_km_auto = round(mean(kmMoto),2),
              moy_km_total = round(mean(kmTotaux),2)
    )

situation_famille <- type_rouleur %>%
    group_by(Nombre_enfants) %>%
    summarise(moy_km_moto = round(mean(kmAuto, na.rm=TRUE),2),
              moy_km_auto = round(mean(kmMoto),2),
              moy_km_total = round(mean(kmTotaux),2)
    ) %>%
    filter(Nombre_enfants < 10)
View(situation_famille)

##################################################
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    output$plot1 <- renderPlot({
        
        #INPUT
        moyenne_km_an <- input$separation
        
        
        sup_moto <- gr_sum %>% 
            group_by(kmMoto) %>%
            filter(kmMoto >= moyenne_km_an)
        inf_moto <- gr_sum %>% 
            group_by(kmMoto) %>%
            filter(kmMoto < moyenne_km_an)
        
        sup_auto <- gr_sum %>% 
            group_by(kmAuto) %>%
            filter(kmAuto >= moyenne_km_an)
        inf_auto <- gr_sum %>% 
            group_by(kmAuto) %>%
            filter(kmAuto < moyenne_km_an)
        
        effectifs <- data.frame(stat=c("Total etudie","Gros rouleurs en moto","Petits rouleurs en moto","Gros rouleurs en auto","Petits rouleurs en auto"),
                                effectif=c(dim(bdd)[1],dim(sup_moto)[1],dim(inf_moto)[1],dim(sup_auto)[1],dim(inf_auto)[1]))
        
        pourcentage_gros_rouleurs_fr <- effectifs[-5,][-3,][-1,] %>%
            mutate(pourcentage = round(effectif / dim(bdd)[1] * 100,2))
        
        ggplot(data=pourcentage_gros_rouleurs_fr, aes(x=stat, y=pourcentage, fill =factor(stat))) +
            geom_bar(stat="identity") +
            scale_fill_brewer(palette="Set1") +
            labs(fill="Vehicule", 
                 x=NULL, 
                 y="%", 
                 title="Repartition du nombre de gros rouleurs en fonction du vehicule") +
            geom_text(aes(y = pourcentage / 2, label = pourcentage ), color = "white", size=5,check_overlap = TRUE)+ 
            theme(legend.position="none")
    })
    
    output$plot2 <- renderPlot({
        
        kmEtudie <- input$km

        ggplot(data=situation_couple, aes_string(x="en_couple", y=kmEtudie, fill = "en_couple")) +
            geom_bar(stat="identity") +
            scale_fill_brewer(palette="Set2") +
            labs(fill="situation", 
                 x="Situation", 
                 y="Km", 
                 title="Moyenne de km parcourus en fonction de la situation maritale") +
            geom_text(aes_string(y = 10000 , label = kmEtudie ), color = "white", size=5) + 
            theme(legend.position="none")
    })
    
    output$plot3 <- renderPlot({
        
        kmEtudie <- input$kmFamille
        
        ggplot(data=situation_famille, aes_string(x="Nombre_enfants", y=kmEtudie, fill = factor("Nombre_enfants"))) +
            geom_bar(stat="identity") +
            scale_fill_brewer(palette="Set2") +
            labs(fill="situation", 
                 x="Situation", 
                 y="Km", 
                 title="Moyenne de km parcourus en fonction de la situation familiale") +
            geom_text(aes_string(y = 7000 , label = kmEtudie ), color = "white", size=5) + 
            theme(legend.position="none")
    })

})
