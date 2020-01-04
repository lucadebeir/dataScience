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
                                          c("moyenne de l'echantillon" = "20000",
                                            "moyenne francaise" = "17000")
                             )
                         ),
                         tabsetPanel(
                             tabPanel("Repartition des gros rouleurs", 
                                      
                             plotOutput("plot1")
                             ),
                             tabPanel("heheheh")
                         )),
                tabPanel("Usages",
                         plotOutput(""))
                
            )
        )
    )
)
