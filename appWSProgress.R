

## canoe marathon results app

#-----------LIBRARY LOADING---------------

library(shiny)
library(shinythemes)
library(tidyverse) # group of packages for data wrangling and visualisation (ggplot2)
library(lubridate) # convert date to R date format
library(DT) # Data Tables package for interactive html tables

#-------------DATASET LOADING--------------------

main_data <- readRDS("WS2017Final1.rds") %>% 
  mutate(Notes = case_when(is.na(Notes) ~ "Finish", TRUE ~ Notes)) %>% 
  mutate(Time = format(Time, format = "%H:%M:%S")) %>% 
  select(BoatType, Year, Position, Class, Race, Name, Club, Time, Notes) %>% 
  arrange(desc(Year), Race, Class, Position) %>% 
  mutate(Name2 = paste(Name))

table_data <- main_data %>% 
  select(-Name2, -BoatType)



menus <- main_data %>% 
  select(Year, Race) %>% 
  arrange(desc(Year), Race) %>% 
  unique()

paddlers <- main_data %>% 
  select(Name2) %>% 
  arrange(Name2) %>% 
  unique()

boattypes <- main_data %>% 
  select(BoatType) %>% 
  arrange(BoatType) %>% 
  unique()

divs <- main_data %>% 
  select(Class) %>% 
  arrange(Class) %>% 
  unique()

AddYears <- main_data %>%
  select(Year) %>% 
  arrange(desc(Year)) %>% 
  unique() 

top3s <- main_data %>% 
  select(Name2, Position) %>% 
  filter(Position %in%(c(1,2,3))) %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  select(-Position) %>% 
  unique()

medals <- main_data %>% 
  select(Name2, Position, Race) %>% 
  filter(Position %in%(c(1,2,3)), Race == "National Championships") %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  select(-Position) %>% 
  unique()

completeCount <- main_data %>% 
  select(Name2, Position) %>% 
  filter(!is.na(Position)) %>% 
  select(-Position) %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  unique()

faveLadiess <- main_data %>%
  select(Name2, Race) %>% 
  group_by(Name2, Race) %>% 
  add_tally() %>% 
  filter(n>1) %>% 
  arrange(Name2, -n) %>% 
  unique()





#-------------UI SECTION ----
ui <- fluidPage(
  
  includeHTML("GA.html"), 
  
  
  theme = shinytheme("flatly"),
  
  list(tags$head(HTML('<link rel="icon", href="kayak.png", 
                      type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="Marathon Results Database"
      )
  ),
  
  fluidRow(column(10, tags$h1("DW and Waterside Results Database")),
           column(2, tags$img(src="DW Logo.JPG", height = "80px", align = "left"))),
  
  tabsetPanel(
    
    tabPanel("Intro",
             
             fluidRow(column(12, tags$img(src="P1030108C.jpg", width = "60%"))),
             fluidRow(column(12, tags$h3("Congratulations to those Completing these gruelling events:")))
    ),
    
    tabPanel("Complete Database",
             fluidRow(column(12, tags$h3("Complete WS Database"))),
             fluidRow(column(4,selectInput("Year","Pick one or more Years",c("All",unique(menus$Year)),selected = "All",multiple = TRUE)),
               column(4,selectInput("Race","Select Race",c("All",unique(menus$Race)),selected = "All",multiple = TRUE)),
               column(4,selectInput("div","Select Class",c("All",unique(menus$Class)),selected = "All",multiple = TRUE))),
             
             # Create a new row for the table.
             fluidRow(
               column(12, DT::dataTableOutput("table"))
             )),
    
    tabPanel("Paddler History", 
             fluidRow(
               column(7, tags$h3(textOutput("paddlername"), tags$h5("Please note, paddlers may appear as duplicates in this system if they have raced for multiple clubs or if their names were entered incorrectly at races"))),
               column(5,selectInput("paddler", "Choose Paddler: (hit backspace to clear and type in a name)", c(unique(paddlers$Name2)), selected = "Ince John", multiple = FALSE))
             ),
             
             fluidRow(column(3, tags$h3("Positions"), tags$h4(textOutput("top3s")), tags$h4(textOutput("medals"))),
                      column(9, plotOutput("positions"))
             )
    ),
    
    tabPanel("Race Attendance", 
             fluidRow(
               column(6, tags$h3("Race Attendance")),
               column(4,selectInput("attdiv", "Choose one or more Classes", c(unique(divs$Class)), selected = "K2 Senior", multiple = TRUE)),
               column(6,selectInput("attyear","Pick one or more Years",c("All",unique(menus$Year)),selected = "All",multiple = TRUE))
             ),
               
             fluidRow(column(12, plotOutput("attendance"))
             )
    )
  )
  
  )

#----------------SERVER FUNCTION ---------------------

server <- function(input, output, session) {
  
  # Big Database table with reactive filtering
  output$table <- DT::renderDataTable({
    DT::datatable({
      data <- table_data
      if (input$Year != "All") {
        data <- data[data$Year %in% input$Year,]
      }
      if (input$Race != "All") {
        data <- data[data$Race %in% input$Race,]
      }
      if (input$div != "All") {
        data <- data[data$Class %in% input$div,]
      }
      data #i.e. render this dataset as a table
    },
    filter = list(position = 'top', clear = FALSE),
    options = list(pageLength = 15), 
    rownames = FALSE)
  }
  
  
  
  
  )
  
  #filtered inputs for main table
  menus_filtered1 <- reactive({
    if ("All" %in% input$Year) {
      menus
    } else {
      menus %>% filter(Year %in% input$Year)}
  })
  
  observe({
    updateSelectInput(session, "Race", choices = c("All", menus_filtered1()$Race), selected = "All")
  })
  
  menus_filtered2 <- reactive({
    if ("All" %in% input$Race) {
      menus_filtered1()
    } else {
      menus_filtered1() %>% filter(Race %in% c(input$Race))}
  })
  
  observe({
    updateSelectInput(session, "div", choices = c("All",menus_filtered2()$Class), selected = "All")
  })
  
  #define outputs for individual paddler
  output$paddlername <- renderText({paste("Paddler History:",input$paddler)})
  
  output$no_races <- renderText({
    paste(nrow(main_data[main_data$Name2 == input$paddler,]),
          "races entered,")
  })
  
  output$comprate <- renderText({
    paste(round(completeCount$n[completeCount$Name2 == input$paddler]/nrow(main_data[main_data$Name2 == input$paddler,])*100, digits = 0),
          "% completion rate")
  })
  
  output$top3s <- renderText({
    paste("Number of Top 3 finishes:",
          top3s$n[top3s$Name2 == input$paddler]
    )
  })
  
  output$medals <- renderText({
    paste("",
          medals$n[medals$Name2 == input$paddler]
    )
  })
  
  output$faveraces <- renderTable({
    faveraces1 <- faveLadies %>% 
      filter(Name2 == input$paddler) %>% 
      rename(Entries = n) %>% 
      head(5)
    
  }, rownames = FALSE)
  
  output$races <- renderPlot({
    
    racechartdata <- main_data %>% 
      select(Year, Name2) %>% 
      filter(Name2 == input$paddler)
    
    ggplot(racechartdata, aes(Year))+
      geom_histogram(aes(fill = Year), stat = "count")+
      ylab("Number of Races Entered")
    
  })
  
  output$positions <- renderPlot({
    
    poschartdata <- main_data %>% 
      select(Year, Position, Class, Name2) %>% 
      filter(Name2 == input$paddler)
    
    
    ggplot(poschartdata)+
      geom_point(aes(Year, Position, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+
      scale_y_continuous(trans = "reverse", breaks = unique(main_data$Position))
    
  
    
  })
  
  
    
    #WS
    output$attendance <- renderPlot({
      
      attchartdata <- main_data %>% 
        select( Race, Year, Class) %>%
        filter(Class == input$attdiv,Year == input$attyear)
     
      
      ggplot(attchartdata, aes(Race))+
        geom_bar(aes(fill = Class), stat = "count", position = "dodge")+
        coord_flip()+
        facet_wrap(~Year)
    
    
    
    
    
   
    
  
   
 
    
  })
  
  
  
  
  
}

shinyApp(ui, server)
