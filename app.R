### libraries and packages

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(extrafont)
library(showtext)

### fonts 

# import fonts - takes ~5 minutes 
# font_import()

# default font
loadfonts() 
showtext_auto()
#font_add("Inter", regular = "C:/my_repos/PersonalVisualizationProject/Inter/Inter-VariableFont_slnt,wght.ttf")


### theme

theme_blue <- shinyDashboardThemeDIY(
  appFontFamily = "Helvetica" 
  ,appFontColor = "#edc9c7"
    ,primaryFontColor = "#edc9c7"
    ,infoFontColor = "#edc9c7"
    ,successFontColor = "#edc9c7"
    ,warningFontColor = "#edc9c7"
    ,dangerFontColor = "#edc9c7"
    
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
  
  ,sidebarUserTextColor = "#edc9c7"
    
  ,sidebarSearchBackColor = "#edc9c7"
    ,sidebarSearchIconColor = "#068fa4"
    ,sidebarSearchBorderColor = "#edc9c7"
    
  ,sidebarTabTextColor = "#edc9c7"
    ,sidebarTabTextSize = 20
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
  ,boxTitleSize = 36
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
    ,tabBoxBackColor = "#edc9c7"
    ,tabBoxHighlightColor = "#edc9c7"
    ,tabBoxBorderRadius = 1
  
  ,buttonBackColor = "#068fa4"
    ,buttonTextColor = "#edc9c7"
    ,buttonBorderColor = "#edc9c7"
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

plot_theme <- theme(panel.grid = element_line(colour = "#edc9c7",linetype = "dotted"),
                    panel.background = element_rect(fill = "transparent", color = 'transparent'),
                    plot.background = element_rect(fill = "#068fa4", color = 'transparent'),
                    plot.title = element_text(face = "bold", size = 25, hjust = 0.5,
                                              color = "#edc9c7", family = "Helvetica"),
                    axis.text = element_text(color = "#edc9c7", size = 15, family = "Helvetica"),
                    axis.title = element_text(color = "#edc9c7", size = 18, hjust = 0.5, family = "Helvetica"),
                    legend.key = element_rect(fill = "#068fa4", color = "transparent"),
                    legend.title = element_text(color = "#edc9c7", size = 18, family = "Helvetica"),
                    legend.background = element_rect(fill = "#068fa4", color = 'transparent'),
                    legend.text = element_text(color = "#edc9c7", size = 12, family = "Helvetica")) 
 

### server
 
 server <- function(input, output, session) {
   
   ### HOME PAGE ###
   
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
   
   output$colPlotSteps <- renderPlot({
     weekdays <- distinct(steps_df[, c("date", "weekday")])
     
     
     steps_df  %>% 
       group_by(date, name) %>%
       summarise(total_steps = sum(count)) %>%
       inner_join(weekdays) %>% 
       group_by(weekday, name) %>% 
       summarise(avg_steps = mean(total_steps)) %>% 
       ggplot(aes(fill = name, x = weekday, y = avg_steps)) +
       geom_bar(position="dodge", stat="identity") +
       scale_fill_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", 
                                     "Gentleman3" = "orange"),
                          name = "Person:")+
       labs(title = "Number of steps by day",
            x = "Day of the week",
            y = "Number of steps")+
       plot_theme
  })
   
   
   ### WATER ###
   
   ## wykres korelacji woda/kroki
   
   output$pointPlotWaterSteps <- renderPlot({
     
     steps_modified <- steps_df %>% 
       group_by(date) %>% 
       summarise(total_steps = sum(count))
     
     
     steps_modified %>% inner_join(water_df, by = "date") %>% 
     ggplot()+
       geom_point(aes(x = total_steps, y = amount, color = name), size = 3)+
       scale_color_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", 
                                     "Gentleman3" = "orange"),
                         name = "Person:")+
       labs(title = "Correlation between daily steps and water intake",
            x = "Total daily steps",
            y = "Amount of water consumed, in ml")+
       plot_theme
     
     
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
       scale_fill_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", "Gentleman3" = "orange"),
                         name = "Person:")+
       labs(title = paste0("Water consumption on ", input$waterDayOfWeek, "s"),
            x = "",
            y = "Amount of water consumed, in ml")+
       plot_theme
     
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
    menuItem("Sleep", tabName = "Sleep"),
    menuItem("Steps", tabName = "Steps"),
    menuItem("Water", tabName = "Water")
  ),
  width = 250
  
)

## body

body <- dashboardBody(
  tabItem(
    tabName = "Water",
    
    fluidRow(
      box(title = "Who of us drinks the most (water) on Fridays?"),
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
    br(),
    br(),
    fluidRow(
      box(title = "Do we drink more water if we walk more?"),
      column(width = 8, 
             shinycssloaders::withSpinner(plotOutput("pointPlotWaterSteps"),
                                          type = getOption("spinner.type", default = 5),
                                          color = getOption("spinner.color", default = "#edc9c7"))),
      column(width = 4,
             textOutput("textWaterSteps"))
    )
  ),
  tabItem(
    tabName = "Sleep",
    fluidRow(
      box(title = "What days do we walk the most?"),
      column(width = 8,
             shinycssloaders::withSpinner(plotOutput("colPlotSteps"),
                                          type = getOption("spinner.type", default = 5),
                                          color = getOption("spinner.color", default = "#edc9c7"))
      ))
  ),
  tabItem(
    tabName = "Steps"
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


