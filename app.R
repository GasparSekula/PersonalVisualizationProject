### libraries 

library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)


server <- function(input, output, session) {
  
  ### SLEEP ###
  
  ## wykres boxplot liczba godzin snu w zależności od dnia tygodnia 
  
  # output$boxPlotSleepHours <- renderPlot({
  #   ggplot()
  # })
  
  ## wykres kolumnowy godzina rozpoczęcia snu w zal. od dnia tygodnia
  
  # output$colPlotSleepStart <- renderPlot({
  #   ggplot()
  # })
  
  ## wykres gęstosci godzina zakończenia snu w zal. od dnia tygodnia
  
  # output$densityPlotSleepEnd <- renderPlot({
  #   ggplot()
  # })
  
  ### STEPS ###
  
  ## wykres kolumnowy liczby przebiegniętych kroków (run_step)
  
  # output$colPlotRunning <- renderPlot({
  #   ggplot()
  # })
  
  ## wykres kolumnowy kroków dla poszczególnych dni
  
  # output$colPlotSteps <- renderPlot({
  #   ggplot()
  # })
  
  
  ### WATER ###
  
  ## wykres korelacji woda/kroki
  
  # output$pointPlotWaterSteps <- renderPlot({
  #   ggplot()
  # })
  
  ## wykres boxplot/violinplot woda w zal. od dnia tygodnia
  
  output$plotWaterWeekday <- renderPlot({
    ggplot(data = water_df %>% filter(weekday == input$waterDayOfWeek)) +
    geom_violin(aes(x = name, y = amount, fill = name))+
    geom_boxplot(aes(x = name, y = amount))+
      ylim(c(500, 2500))+
      scale_fill_discrete(name = "Person: ")+
      theme_minimal()
        
  })
  
  
}

### UI1 - sleep ###

ui1 <- fluidPage(
                # titlePanel("Samochody: moc vs cena"),
                # 
                #  sidebarLayout(
                #    sidebarPanel(
                #      selectInput(
                #        inputId = "color",
                #        label = "Parametr:",
                #        choices = c(
                #          "Rodzaj paliwa" = "Rodzaj.paliwa",
                #          "Liczba drzwi" = "Liczba.drzwi",
                #          "Skrzynia biegow" = "Skrzynia.biegow"
                #        )
                #      ),
                #      width = 3
                #    ),
                #    mainPanel(#plotOutput("pointPlot"),
                #      ### Zadanie 7 ###
                #      shinycssloaders::withSpinner(plotOutput("pointPlot"),
                #                                   type = getOption("spinner.type", default = 1),
                #                                   color = getOption("spinner.color", default = "#0275D8"),
                #                                   size = getOption("spinner.size", default = 1)
                #      ),
                #      ### Koniec - Zadanie 7 ###
                #      width = 9)
                #  )
                 )


### UI2 - STEPS ###

ui2 <- fluidPage(
#                   titlePanel("Rozkład ceny samochodów w zależności od roku produkcji"),
#                  ### Koniec - Zadanie 1 ###                  
#                  sidebarLayout(
#                    sidebarPanel(
#                      ### Zadanie 5 ###
#                      sliderInput(
#                        inputId = "rok",
#                        label = "Lata:",
#                        min = min(auta2012$Rok.produkcji),
#                        max = max(auta2012$Rok.produkcji),
#                        value = c(1980, 2007)
#                      ),
#                      ### Koniec - Zadanie 5 ### 
#                      width = 3
#                    ),
#                    mainPanel(
#                      ### Zadanie 4 ###
#                      shiny::markdown(
#                        "Potrzebujemy: \n 1. suwak, na którym można wybrać zakres lat z którego rozpatrujemy samochody \n 2. wykres boxplot cen samochodów w PLN dla wybranych lat z suwaka"
#                      ),
#                      ### Koniec - Zadanie 4 ### 
#                      ### Zadanie 5 ###
#                      plotOutput("boxPlot"),
#                      ### Koniec - Zadanie 5 ### 
#                      width = 9)
#                  )
  )


### UI3 - WATER ###

 ui3 <- fluidPage(
                titlePanel("Water intake by weekday"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput(
                       inputId = "waterDayOfWeek",
                       label = "Day of week:",
                       choices = unique(water_df$weekday)
                     ),
                     
                     width = 3
                   ),
                   
                   mainPanel(
                     
                     shiny::markdown(
                       "text"
                     ),
                     
                     plotOutput("plotWaterWeekday"),
                     
                     width = 9)
                 )
   )




app_ui <- navbarPage(
  title = "Health data analysis",
  tabPanel("Sleep", ui1),
  tabPanel("Steps", ui2,
           icon = icon("database")),
  tabPanel("Water intake", ui3,
           icon = icon("database")),

  theme = bslib::bs_theme(bootswatch = "cosmo"),
  
  
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2021 Copyright:
                  <a class='text-dark' href='https://www.mi2.ai/'>MI2</a>
                </p>
                </footer>
                "),
  
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
  
)

shinyApp(app_ui, server)


