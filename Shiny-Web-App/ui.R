#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
    
        # Application title
        titlePanel("Comparaison de l'utilisation de la voiture et celle du 2-roues"),
    
        mainPanel(
            tabsetPanel(
                tabPanel("Frequence",
                         wellPanel(
                             radioButtons("separation", "Kilometrage de separation des types de rouleurs:",
                                          c("Moyenne de l'echantillon" = "27718.2",
                                            "Moyenne francaise" = "17000")
                             )
                         ),
                         tabsetPanel(
                             tabPanel("Repartition des gros rouleurs",
                                      
                                      plotOutput("plot1")
                             ),
                             tabPanel("Situation Maritale",
                                      wellPanel(
                                          radioButtons("km", "Km a comparer",
                                                       c("Auto" = "moy_km_auto",
                                                         "Moto" = "moy_km_moto",
                                                         "Total" = "moy_km_total")
                                          )),
                                      plotOutput("plot2")
                             ),
                             tabPanel("Situation Familiale",
                                      wellPanel(
                                          radioButtons("kmFamille", "Km a comparer",
                                                       c("Auto" = "moy_km_auto",
                                                         "Moto" = "moy_km_moto",
                                                         "Total" = "moy_km_total")
                                          )),
                                      plotOutput("plot3")
                             )
                          )),
                tabPanel("Usages",
                         plotOutput(""))
                
            )
        )
    )
)
