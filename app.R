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
library(lubridate)

### fonts 

# import fonts - takes ~5 minutes 
# font_import()

# default font
#loadfonts() 
#showtext_auto()
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
    ,tabBoxTabTextSize = 20
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
                    plot.title = element_text(face = "bold", size = 20, hjust = 0.5,
                                              color = "#edc9c7", family = "Helvetica"),
                    axis.text = element_text(color = "#edc9c7", size = 10, family = "Helvetica"),
                    axis.title = element_text(color = "#edc9c7", size = 12, hjust = 0.5, family = "Helvetica"),
                    legend.key = element_rect(fill = "#068fa4", color = "transparent"),
                    legend.title = element_text(color = "#edc9c7", size = 16, family = "Helvetica"),
                    legend.background = element_rect(fill = "#068fa4", color = 'transparent'),
                    legend.text = element_text(color = "#edc9c7", size = 12, family = "Helvetica")) 



### server

server <- function(input, output, session) {
  
  ### HOME PAGE ###
  
  ### SLEEP ###
  
  ## wykres boxplot liczba godzin snu w zależności od dnia tygodnia 
  output$sleepGentleman <- renderUI({
    selectInput(
      inputId = "sleepGentleman",
      label = "Person:",
      choices = unique(sleep_df$name)
    )
  })
  
  
  output$plotSleepGentleman <- renderPlotly({
    p <- sleep_df %>%
      filter(name == input$sleepGentleman) %>%
      ggplot(aes(x = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
                 y = hm(duration), fill = input$sleepGentleman)) +  # Specify fill aesthetic
      geom_boxplot() +
      scale_y_time(labels = scales::time_format("%H:%M")) +
      labs(x = "Weekday",
           y = "Duration",
           title = paste0("Distribution of sleep duration for ", input$sleepGentleman)) +
      scale_fill_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", "Gentleman3" = "orange")) +  # Assign colors
      plot_theme+
      guides(fill = FALSE)
    
    ggplotly(p)
  })
  
  output$textSleepGentleman <- renderText({
    "text sleep Gentleman"
  })
  
  
  ## wykres kolumnowy godzina rozpoczęcia snu w zal. od dnia tygodnia ??
  
  # output$colPlotSleepStart <- renderPlot({
  #   ggplot()
  # })
  
  
  
  output$histSleepStart <- renderPlotly({
    p <- sleep_df %>%
      filter(name == input$sleepGentleman) %>%
      ggplot(aes(x = hour(ymd_hms(start_time)), fill = input$sleepGentleman)) +
      geom_histogram(bins = 48) +
      scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d:00", seq(0, 23, by = 1))) +
      labs(x = "Hour of the Day",
           y = "Count",
           title = paste0("Distribution of sleep start times for ", input$sleepGentleman)) +
      scale_fill_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", "Gentleman3" = "orange")) +
      plot_theme +
      guides(fill = FALSE)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  
  
  output$textSleepGentleman2 <- renderText({
    "text sleep Gentleman"
  })
  
  

  
  
  output$histSleepEnd <- renderPlotly({
    p <- sleep_df %>%
      filter(name == input$sleepGentleman) %>%
      ggplot(aes(x = hour(ymd_hms(end_time)), fill = input$sleepGentleman)) +
      geom_histogram(bins = 48) +
      scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d:00", seq(0, 23, by = 1)),
                         limits = c(0, 23)) +
      labs(x = "Hour of the Day",
           y = "Count",
           title = paste0("Distribution of sleep end times for ", input$sleepGentleman)) +
      scale_fill_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", "Gentleman3" = "orange")) +
      plot_theme +
      guides(fill = FALSE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees
    
    ggplotly(p)
  })
  
  
  
  
  
  output$textSleepGentleman3 <- renderText({
    "text sleep Gentleman"
  })
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
  
  output$colPlotSteps <- renderPlotly({
    weekdays <- distinct(steps_df[, c("date", "weekday")])
    
    
    p <- steps_df  %>% 
      group_by(date, name) %>%
      summarise(total_steps = sum(count)) %>%
      inner_join(weekdays) %>% 
      group_by(weekday, name) %>% 
      summarise(avg_steps = mean(total_steps)) %>% 
      ggplot(aes(fill = name,
                 x = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
                 y = avg_steps)) +
      geom_bar(position="dodge", stat="identity") +
      scale_fill_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", 
                                   "Gentleman3" = "orange"),
                        name = "Person:")+
      labs(title = "Number of steps by day",
           x = "Weekday",
           y = "Steps")+
      plot_theme
    
    ggplotly(p)
  })
  
  ## wykres kolumnowy krokow dla dat z interaktywnym sliderem
  
  output$colPlotStepsByDate <- renderPlotly({
    steps_modified <- steps_df %>% 
      group_by(date, name) %>% 
      summarise(total_steps = sum(count))
    
    p <- ggplot(steps_modified, aes(x = date, y = total_steps, fill = name)) + 
      geom_bar(position="dodge", stat="identity") +
      scale_fill_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", 
                                   "Gentleman3" = "orange"),
                        name = "Person:")+
      labs(title = "Number of steps by day",
           x = "Weekday",
           y = "Steps") +
      plot_theme
    
    ggplotly(p) %>% 
      layout(xaxis = list(rangeslider = list(type = "date")))
    
  })
  
  ### WATER ###
  
  ## wykres korelacji woda/kroki
  
  output$pointPlotWaterSteps <- renderPlotly({
    
    steps_modified <- steps_df %>% 
      group_by(date) %>% 
      summarise(total_steps = sum(count))
    
    p <- steps_modified %>% inner_join(water_df, by = "date") %>% 
      ggplot()+
      geom_point(aes(x = total_steps, y = amount, color = name), size = 3)+
      scale_color_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", 
                                    "Gentleman3" = "orange"),
                         name = "Person:")+
      labs(title = "Correlation between daily steps and water intake",
           x = "Total daily steps",
           y = "Amount of water consumed, in ml")+
      plot_theme
    
    ggplotly(p)
    
    
  })
  
  output$textWaterSteps <- renderText({
    "text Water Steps"
  })
  
  ## wykres boxplot/violinplot woda w zal. od dnia tygodnia
  
  output$waterDayOfWeek <- renderUI({
    selectInput(
      inputId = "waterDayOfWeek",
      label = "Choose day of week:",
      choices = unique(water_df$weekday)
    )
  })
  
  
  output$plotWaterWeekday <- renderPlotly({
    p1 <- ggplot() +
      geom_violin(data = water_df, aes(x = name, y = amount, fill = name), alpha = 0.5)+
      geom_point(data = water_df %>% filter(weekday == input$waterDayOfWeek), aes(x=name, y=amount), size=3, color = "#edc9c7")+
      ylim(c(500, 2500))+
      scale_fill_manual(values = c("Gentleman1" = "red", "Gentleman2" = "gold", "Gentleman3" = "orange"),
                        name = "Person:")+
      labs(title = paste0("Water consumption on ", input$waterDayOfWeek, "s"),
           x = "",
           y = "Amount of water consumed, in ml")
    
    p2 <- p1 + plot_theme
    
    ggplotly(p2)
    
  })
  
  output$textWaterWeekday <- renderText({
    "Is there a weekday on which we have tendency to drink the most water? \n
    The plot visualizes the distribution of water consumption across our beloved individuals 
    (Gentleman1, Gentleman2, and Gentleman3) on a specific day of the week selected below. 
    The violin plot displays the probability density of water intake on all days, 
    while the individual data points highlight the actual values for each person for selected weekday.

 "
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
  tabItems(
    tabItem(
      tabName = "Water",
      
      fluidRow(
        box(title = "Who of us drinks the most (water) on Fridays?"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("plotWaterWeekday"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        ),
        column(width = 3,
               textOutput("textWaterWeekday"),
               br(),
               uiOutput("waterDayOfWeek"))
        
      ),
      br(),
      br(),
      fluidRow(
        box(title = "Do we drink more water if we walk more?"),
        column(width = 8, 
               shinycssloaders::withSpinner(plotlyOutput("pointPlotWaterSteps"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))),
        column(width = 4,
               textOutput("textWaterSteps"))
      )
    ),
    tabItem(
      tabName = "Steps",
      fluidRow(
        box(title = "What days do we walk the most?"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("colPlotSteps"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        )),
      
      fluidRow(
        box(title = "What days do we walk the most?"),
        column(width = 12,
               shinycssloaders::withSpinner(plotlyOutput("colPlotStepsByDate"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        )),
      
    ),
    tabItem(
      tabName = "Sleep",
      fluidRow(
        box(title = "Sleep durtation"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("plotSleepGentleman"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        ),
        column(width = 4,
               textOutput("textSleepGentleman"),
               br(),
               uiOutput("sleepGentleman"))
      ),
      fluidRow(
        box(title = "Sleep durtation"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("histSleepStart"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        ),
        column(width = 4,
               textOutput("textSleepGentleman2")
               
      )),
      fluidRow(
        box(title = "Sleep durtation"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("histSleepEnd"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        ),
        column(width = 4,
               textOutput("textSleepGentleman3")
               
        ))
    )),
  theme_blue
)

## ui 

ui <- dashboardPage(
  
  header,
  sidebar,
  body
  
)

shinyApp(ui, server)


