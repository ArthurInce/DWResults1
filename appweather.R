

## DW and WS  results app

#-----------LIBRARY LOADING---------------

library(stringr)
library(ggpubr)
library(shiny)
library(shinythemes)
library(tidyverse) # group of packages for data wrangling and visualisation (ggplot2)
library(lubridate) # convert date to R date format
library(DT) # Data Tables package for interactive html tables

#-------------DATASET LOADING--------------------
# Weather Table

main_data <- readRDS("Weather.rds") %>% 
 
  select(EasterSunday, Year, SeniorFlow, First, MaxWind, MinTemperature, CompletionRate) %>% 
  arrange(desc(Year), SeniorFlow, First)

menus <- main_data %>% 
  select(EasterSunday, Year, SeniorFlow, First, MaxWind, MinTemperature, CompletionRate) %>% 

  arrange(desc(Year),desc(SeniorFlow), desc(First), desc(MaxWind), desc(MinTemperature), desc(CompletionRate) ) %>% 
  unique()


Flow <- main_data %>% 
  select(SeniorFlow) %>% 
  arrange(SeniorFlow) %>% 
  unique()

FirstPlace <- main_data %>% 
  select(First) %>% 
  arrange(First) %>% 
  unique()

Wind <- main_data %>% 
  select(MaxWind) %>% 
  arrange(MaxWind) %>% 
  unique()

Temp <- main_data %>% 
  select(MinTemperature) %>% 
  arrange(MinTemperature) %>% 
  unique()

Completion <- main_data %>% 
  select(CompletionRate) %>% 
  arrange(CompletionRate) %>% 
  unique()




#------------- DW & WS UI SECTION ----
ui <- fluidPage(
  
  includeHTML("GA.html"), 
  
  
  theme = shinytheme("flatly"),
  
  list(tags$head(HTML('<link rel="icon", href="kayak.png", 
                      type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="DW Weather Factors"
      )
  ),
  
  fluidRow(column(10, tags$h1("DW Weather Factors")),
           column(2, tags$img(src="DW Logo.JPG", height = "80px", align = "left"))),
  
  tabsetPanel(
    
    tabPanel("DW Weather",
             
             
             # Create a new row for the table.
             fluidRow(
               column(12, DT::dataTableOutput("table"))
             )),
    
    tabPanel("Graphs", 
             
             fluidRow(column(12, plotOutput("Graphs"))
             )
    )
    

    ))

  
  
  
  

#----------------SERVER FUNCTION ---------------------

server <- function(input, output, session) {
  
  # Big Database table with reactive filtering
  output$table <- DT::renderDataTable({
    DT::datatable({
      data <- main_data
     
      data #i.e. render this dataset as a table
    },
    options = list(pageLength = 15), 
    rownames = FALSE)
  })
  
  output$Graphs <- renderPlot({
    
    poschartdata <- main_data %>% 
      select(Year, SeniorFlow, First, MaxWind, MinTemperature, CompletionRate) 
    
     
    
  a <-  ggplot(poschartdata)+
      geom_point(aes(First, SeniorFlow, color = Year),  size = 5)
  b <-  ggplot(poschartdata)+
    geom_point(aes(First, MaxWind, color = Year),  size = 5)
  c <-  ggplot(poschartdata)+
    geom_point(aes(First, MinTemperature, color = Year),  size = 5)
  
  d <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, SeniorFlow, color = Year),  size = 5)
  e <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, MaxWind, color = Year),  size = 5)
  f <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, MinTemperature, color = Year),  size = 5)
  
  
  
  ggarrange(a,b,c,d,e,f,
            
            ncol = 3, nrow = 2)
  
    

    
  })
  
  
  
  
  
 
  
}
 
  
shinyApp(ui, server)
