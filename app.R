#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("data.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lactodouce: dosages hormonaux"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("hormone", h3("Choisir une hormone"),
                        choices = list("Prolactine" = "prl", "Progestérone" = "pgs",
                                       "Estradiol" = "est")),
            radioButtons("temps", h3("Choisir le moment"),
                        choices = list("Tout" = "tous", 
                                       "Avant MB" = "t1", 
                                       "1 mois après MB" = "t2", 
                                       "2 mois après MB" = "t3")),
            checkboxGroupInput("exploit", h3("Choisir les fermes"),
                        choices = list("M Pallais" = "PALLAIS",
                                       "Pampilles" = "PAMPILLES", 
                                       "AL Vautrin" = "VAUTRIN",
                                       "J Cheva" = "CHEVA",
                                       "C Vignon" = "VIGNON", 
                                       "D Brunet" = "BRUNET",
                                       "S Bost" = "BOST"),
                        selected = fermes)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("titre"),
          textOutput("lieu"),
          plotOutput("essai")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$titre <- renderText({
    paste("Dosage de ", input$hormone, " ", input$temps)
  })
  output$lieu <-renderText({
    fermes <- paste(input$exploit, collapse = ", ")
    paste("vous avez choisi ", fermes)
  })
  output$essai <- renderPlot(dosages(input$hormone, input$temps, input$exploit))
}

# Run the application 
shinyApp(ui = ui, server = server)
