### libraries 

library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)

### theme



theme_blue <- shinyDashboardThemeDIY(
  appFontFamily = "FuturaMedium" 
  ,appFontColor = "#edc9c7"
    ,primaryFontColor = "#434C5E"
    ,infoFontColor = "#434C5E"
    ,successFontColor = "#434C5E"
    ,warningFontColor = "#434C5E"
    ,dangerFontColor = "#434C5E"
    
  ,bodyBackColor = "#068fa4" 
    ,logoBackColor = "#068fa4" 
    
  ,headerButtonBackColor = "#068fa4" # kolor  na ikonce chowania
    ,headerButtonIconColor = "#edc9c7" # kolor pasków na ikonce chowania sidebaru
    ,headerButtonBackColorHover = "#edc9c7" # hover ikonki chowania
    ,headerButtonIconColorHover = "#068fa4" # kolor paskow podczas hovera
    ,headerBackColor = "#068fa4" # header
    ,headerBoxShadowColor = "" #cien pod headerem - nie zmieniac
  ,headerBoxShadowSize = "0px 0px 0px"  # nie zmieniac
  
  
  ,sidebarBackColor = "#068fa4"
    ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 1
  
  ,sidebarShadowRadius = "" 
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#D8DEE9"
    
  ,sidebarSearchBackColor = "#4C566A"
    ,sidebarSearchIconColor = "#068fa4"
    ,sidebarSearchBorderColor = "#4C566A"
    
  ,sidebarTabTextColor = "#ECEFF4"
    ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "#068fa4"
    ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "#edc9c7"
    ,sidebarTabTextColorSelected = "#068fa4" 
    ,sidebarTabRadiusSelected = "20px" 
  
  ,sidebarTabBackColorHover = "#edc9c7"
    ,sidebarTabTextColorHover = "#068fa4"
    ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "20px" 
  
  ,boxBackColor = "#068fa4" 
    ,boxBorderRadius = 1
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "#068fa4"
    ,boxPrimaryColor = "#068fa4"
    ,boxInfoColor = "#068fa4"
    ,boxSuccessColor = "#068fa4"
    ,boxWarningColor = "#068fa4"
    ,boxDangerColor = "#068fa4"
    
  ,tabBoxTabColor = "#068fa4"
    ,tabBoxTabTextSize = 16
  ,tabBoxTabTextColor = "#068fa4"
    ,tabBoxTabTextColorSelected = "#068fa4"
    ,tabBoxBackColor = "#BF616A"
    ,tabBoxHighlightColor = "#4C566A"
    ,tabBoxBorderRadius = 1
  
  ,buttonBackColor = "#068fa4"
    ,buttonTextColor = "#2E3440"
    ,buttonBorderColor = "#2E3440"
    ,buttonBorderRadius = 1
  
  ,buttonBackColorHover = "#068fa4"
    ,buttonTextColorHover = "#edc9c7"
    ,buttonBorderColorHover = "#edc9c7"
    
  ,textboxBackColor = "#068fa4" 
    ,textboxBorderColor = "#edc9c7" 
    ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "#068fa4"
    ,textboxBorderColorSelect = "#edc9c7"
    
  ,tableBackColor = "#068fa4"
    ,tableBorderColor = "#edc9c7"
    ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)
 

### server
 
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
   
   output$pointPlotWaterSteps <- renderPlot({
     
     steps_modified <- steps_df %>% 
       group_by(date) %>% 
       summarise(total_steps = sum(count))
     
     steps_modified %>% inner_join(water_df, by = "date") %>% 
     ggplot()+
       geom_point(aes(x = total_steps, y = amount, color = name))
   })
   
   output$textWaterSteps <- renderText({
     "text Water Steps"
   })
   
   ## wykres boxplot/violinplot woda w zal. od dnia tygodnia
   
   output$waterDayOfWeek <- renderUI({
     selectInput(
       inputId = "waterDayOfWeek",
       label = "Day of week:",
       choices = unique(water_df$weekday)
     )
   })
  
   
   output$plotWaterWeekday <- renderPlot({
     ggplot(data = water_df %>% filter(weekday == input$waterDayOfWeek)) +
       geom_violin(aes(x = name, y = amount, fill = name))+
       geom_boxplot(aes(x = name, y = amount))+
       ylim(c(500, 2500))+
       scale_fill_discrete(name = "Person: ")+
       theme_minimal()
     
   })
   
   output$textWaterWeekday <- renderText({
     "text Water Weekday"
   })
   
   
 }


### UI

## header

header <- dashboardHeader(
  title = "Health data analysis"
  
)

## sidebar

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sleep", tabName = "sleep"),
    menuItem("Steps", tabName = "steps"),
    menuItem("Water", tabName = "water")
  ),
  width = 250
  
)

## body

body <- dashboardBody(
  tabItem(
    tabName = "water",
    
    fluidRow(
      box(title = "Water intake"),
      column(width = 8,
             shinycssloaders::withSpinner(plotOutput("plotWaterWeekday"),
                                          type = getOption("spinner.type", default = 5),
                                          color = getOption("spinner.color", default = "#edc9c7"))
             ),
      column(width = 4,
             textOutput("textWaterWeekday"),
             br(),
             uiOutput("waterDayOfWeek"))
      
    ),
    fluidRow(
      box(title = "Correlation between water intake and daily steps"),
      column(width = 8, 
             shinycssloaders::withSpinner(plotOutput("pointPlotWaterSteps"),
                                          type = getOption("spinner.type", default = 5),
                                          color = getOption("spinner.color", default = "#edc9c7"))),
      column(width = 4,
             textOutput("textWaterSteps"))
    )
  ),
  theme_blue
)

## ui 

ui <- dashboardPage(
  
  header,
  sidebar,
  body
  
)

shinyApp(ui, server)


