

## canoe marathon results app

#-----------LIBRARY LOADING---------------

library(shiny)
library(shinythemes)
library(tidyverse) # group of packages for data wrangling and visualisation (ggplot2)
library(lubridate) # convert date to R date format
library(DT) # Data Tables package for interactive html tables

#-------------DATASET LOADING--------------------

wmain_data <- readRDS("WS2017Final1.rds") %>% 
  mutate(Notes = case_when(is.na(Notes) ~ "Finish", TRUE ~ Notes)) %>% 
  mutate(Time = format(Time, format = "%H:%M:%S")) %>% 
  select(BoatType, Year, Position, Class, Race, Name, Club, Time, Notes) %>% 
  arrange(desc(Year), Race, Class, Position) %>% 
  mutate(Name2 = paste(Name))

wtable_data <- wmain_data %>% 
  select(-Name2, -BoatType)



wmenus <- wmain_data %>% 
  select(Year, Race) %>% 
  arrange(desc(Year), Race) %>% 
  unique()

wpaddlers <- wmain_data %>% 
  select(Name2) %>% 
  arrange(Name2) %>% 
  unique()

wboattypes <- wmain_data %>% 
  select(BoatType) %>% 
  arrange(BoatType) %>% 
  unique()

wdivs <- wmain_data %>% 
  select(Class) %>% 
  arrange(Class) %>% 
  unique()

wAddYears <- wmain_data %>%
  select(Year) %>% 
  arrange(desc(Year)) %>% 
  unique() 

wtop3s <- wmain_data %>% 
  select(Name2, Position) %>% 
  filter(Position %in%(c(1,2,3))) %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  select(-Position) %>% 
  unique()

wmedals <- wmain_data %>% 
  select(Name2, Position, Race) %>% 
  filter(Position %in%(c(1,2,3)), Race == "National Championships") %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  select(-Position) %>% 
  unique()

wcompleteCount <- wmain_data %>% 
  select(Name2, Position) %>% 
  filter(!is.na(Position)) %>% 
  select(-Position) %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  unique()

faveLadiess <- wmain_data %>%
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
  
  fluidRow(column(10, tags$h1("Waterside Results Database"))),
  
  tabsetPanel(
    
    tabPanel("WS Intro",
             
             fluidRow(column(12, tags$img(src="P1030108C.jpg", width = "60%"))),
             fluidRow(column(12, tags$h3("Congratulations to those Completing these gruelling events:")))
    ),
    
    tabPanel("WaterSide Database",
             fluidRow(column(12, tags$h3("Complete WS Database"))),
             fluidRow(column(4,selectInput("Year","Pick one or more Years",c("All",unique(wmenus$Year)),selected = "All",multiple = TRUE)),
               column(4,selectInput("Race","Select Race",c("All",unique(wmenus$Race)),selected = "All",multiple = TRUE)),
               column(4,selectInput("div","Select Class",c("All",unique(wmenus$Class)),selected = "All",multiple = TRUE))),
             
             # Create a new row for the table.
             fluidRow(
               column(12, DT::dataTableOutput("table"))
             )),
    
    tabPanel("WS Paddler History", 
             fluidRow(
               column(7, tags$h3(textOutput("paddlername"), tags$h5("Please note, wpaddlers may appear as duplicates in this system if they have raced for multiple clubs or if their names were entered incorrectly at races"))),
               column(5,selectInput("paddler", "Choose Paddler: (hit backspace to clear and type in a name)", c(unique(wpaddlers$Name2)), selected = "Ince John", multiple = FALSE))
             ),
             
             fluidRow(column(3, tags$h3("Positions"), tags$h4(textOutput("wtop3s")), tags$h4(textOutput("wmedals"))),
                      column(9, plotOutput("positions"))
             )
    ),
    
    tabPanel("WS Race Attendance", 
             fluidRow(
               column(6, tags$h3("Race Attendance")),
               column(3,selectInput("attdiv", "Choose one or more Classes", c(unique(wdivs$Class)), selected = "K2 Senior", multiple = TRUE)),
               column(3,selectInput("attyear","Pick one or more Years",c("All",unique(wmenus$Year)),selected = "2018",multiple = TRUE))
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
      data <- wtable_data
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
  wmenus_filtered1 <- reactive({
    if ("All" %in% input$Year) {
      wmenus
    } else {
      wmenus %>% filter(Year %in% input$Year)}
  })
  
  observe({
    updateSelectInput(session, "Race", choices = c("All", wmenus_filtered1()$Race), selected = "All")
  })
  
  wmenus_filtered2 <- reactive({
    if ("All" %in% input$Race) {
      wmenus_filtered1()
    } else {
      wmenus_filtered1() %>% filter(Race %in% c(input$Race))}
  })
  
  observe({
    updateSelectInput(session, "div", choices = c("All",wmenus_filtered2()$Class), selected = "All")
  })
  
  #define outputs for individual paddler
  output$paddlername <- renderText({paste("Paddler History:",input$paddler)})
  
  output$no_races <- renderText({
    paste(nrow(wmain_data[wmain_data$Name2 == input$paddler,]),
          "races entered,")
  })
  
  output$comprate <- renderText({
    paste(round(wcompleteCount$n[wcompleteCount$Name2 == input$paddler]/nrow(wmain_data[wmain_data$Name2 == input$paddler,])*100, digits = 0),
          "% completion rate")
  })
  
  output$wtop3s <- renderText({
    paste("Number of Top 3 finishes:",
          wtop3s$n[wtop3s$Name2 == input$paddler]
    )
  })
  
  output$wmedals <- renderText({
    paste("",
          wmedals$n[wmedals$Name2 == input$paddler]
    )
  })
  
  output$faveraces <- renderTable({
    faveraces1 <- faveLadies %>% 
      filter(Name2 == input$paddler) %>% 
      rename(Entries = n) %>% 
      head(5)
    
  }, rownames = FALSE)
  
  output$races <- renderPlot({
    
    racechartdata <- wmain_data %>% 
      select(Year, Name2) %>% 
      filter(Name2 == input$paddler)
    
    ggplot(racechartdata, aes(Year))+
      geom_histogram(aes(fill = Year), stat = "count")+
      ylab("Number of Races Entered")
    
  })
  
  output$positions <- renderPlot({
    
    poschartdata <- wmain_data %>% 
      select(Year, Position, Class, Name2) %>% 
      filter(Name2 == input$paddler)
    
    
    ggplot(poschartdata)+
      geom_point(aes(Year, Position, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+
      scale_y_continuous(trans = "reverse", breaks = unique(wmain_data$Position))
    
  
    
  })
  
  
    
    #WS
    output$attendance <- renderPlot({
      
      attchartdata <- wmain_data %>% 
        select( Race, Year, Class) %>%
        filter(Class == input$attdiv,Year == input$attyear)
     
      
      ggplot(attchartdata, aes(Race))+
        geom_bar(aes(fill = Class), stat = "count")+
        coord_flip()+
        facet_wrap(~Year)
    
    
    
    
    
   
    
  
   
 
    
  })
  
  
  
  
  
}

shinyApp(ui, server)
