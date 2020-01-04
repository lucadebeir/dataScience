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



##################################################
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    output$plot1 <- renderPlot({
        
        #INPUT
        moyenne_km_an <- input$separation
        
        gros_rouleurs <- bdd %>%
            select(Identifiant, moto1="Nb km 1", moto2="...56", moto3="...67", moto4="...78", kmAuto="Q17 [1]", Nombre_personnes_foyer = "Nombre personnes au foyer",Situation_familiale = "Situation familiale")
        
        gros_rouleurs$moto1 <- as.numeric(as.character(gros_rouleurs$moto1))
        gros_rouleurs$moto2 <- as.numeric(as.character(gros_rouleurs$moto2))
        gros_rouleurs$moto3 <- as.numeric(as.character(gros_rouleurs$moto3))
        gros_rouleurs$moto4 <- as.numeric(as.character(gros_rouleurs$moto4))
        
        gros_rouleurs[is.na(gros_rouleurs)] <- 0
        gr_sum <- gros_rouleurs %>% 
            mutate(kmMoto = rowSums(.[2:5])) %>%
            arrange(desc(kmMoto))
        
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

})
