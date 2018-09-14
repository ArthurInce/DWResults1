

## canoe marathon results app

#-----------LIBRARY LOADING---------------

#library(ggformula)
library(ggplot2)
library(rsconnect)
library(stringr)
library(ggpubr)
library(shiny)
library(shinythemes)
library(tidyverse) # group of packages for data wrangling and visualisation (ggplot2)
library(lubridate) # convert date to R date format
library(DT) # Data Tables package for interactive html tables
library(RColorBrewer)



#--------------Read Comparison data-------------

Comparison_data <- read.csv("ExportABCD.csv",fileEncoding="latin1") %>% 
  select(Year, Races, WatersideA, WatersideB, WatersideC,WatersideD, DWSenior, DWJunior, DWEndeavour, DWVetJunior, DWSingles) %>% 
  arrange(desc(Year)) 

Comparison_data[Comparison_data==0] <- NA
Comparison_data[Comparison_data>3000] <- NA

commenus <- Comparison_data %>% 
  select(Year, WatersideA) %>% 
  arrange(desc(Year), WatersideA) %>% 
  unique()

ComWSA <- Comparison_data %>% 
  select(WatersideB) %>% 
  arrange(WatersideB) %>% 
  unique()




Comparison_data$WatersideA <- as.numeric(as.character(Comparison_data$WatersideA))
Comparison_data$WatersideB <- as.numeric(as.character(Comparison_data$WatersideB))
Comparison_data$WatersideC <- as.numeric(as.character(Comparison_data$WatersideC))
Comparison_data$WatersideD <- as.numeric(as.character(Comparison_data$WatersideD))
Comparison_data$year <- as.numeric(as.character(Comparison_data$Year))
#Comparison_data$DWSenior <- as.numeric(as.character(Comparison_data$DWSenior))
#Comparison_data$DWJunior <- as.numeric(as.character(Comparison_data$DWJunior))
#Comparison_data$DWEndeavour <- as.numeric(as.character(Comparison_data$DWEndeavour))
#Comparison_data$DWVetJunior <- as.numeric(as.character(Comparison_data$DWVetJunior))
#Comparison_data$DWSingles <- as.numeric(as.character(Comparison_data$DWSingles))
#Comparison_data$CompletionRate <- as.numeric(as.character(Comparison_data$CompletionRate))



#WaterA <- Comparison_data %>% select(WatersideA) %>% arrange(WatersideA) %>% unique()
#WaterB <- Comparison_data %>% select(WatersideB) %>% arrange(WatersideB) %>% unique()
#WaterC <- Comparison_data %>% select(WatersideC) %>% arrange(WatersideC) %>% unique()
#WaterD <- Comparison_data %>% select(WatersideD) %>% arrange(WatersideD) %>% unique()




#-------------WaterSide DATASET LOADING--------------------

wsmain_data <- read.csv("ExportWS1.csv",fileEncoding="latin1") %>% 
  # mutate(WSNotes = case_when(is.na(WSNotes) ~ "Finish", TRUE ~ WSNotes)) %>% 
  #mutate(WSTime = format(WSTime, format = "%H:%M:%S")) %>% 
  select(WSBoatType, Year, WSPosition, WSClass, WSRace, WSName, WSClub, WSTime, WSNotes) %>% 
  arrange(desc(Year), WSRace, WSClass, WSPosition) %>% 
  mutate(WSName2 = paste(WSName)) 


wsmain_data$WSPosition <- as.numeric(as.character(wsmain_data$WSPosition))
wsmain_data$Year <- as.numeric(as.character(wsmain_data$Year))
wsmain_data$WSRace %<>% as.character
wsmain_data$WSBoatType %<>% as.character
wsmain_data$WSClass %<>% as.character
wsmain_data$WSRace %<>% as.character
wsmain_data$WSName %<>% as.character
wsmain_data$WSClub %<>% as.character
wsmain_data$WSNotes %<>% as.character



wstable_data <- wsmain_data %>% 
  select(-WSName2, -WSBoatType)

wsmenus <- wsmain_data %>% 
  select(Year, WSRace) %>% 
  arrange(desc(Year), WSRace) %>% 
  unique()

wsdivs <- wsmain_data %>% 
  select(WSClass) %>% 
  arrange(WSClass) %>% 
  unique()

wspaddlers <- wsmain_data %>% 
  select(WSName2) %>% 
  arrange(WSName2) %>% 
  unique()

WSBoatTypes <- wsmain_data %>% 
  select(WSBoatType) %>% 
  arrange(WSBoatType) %>% 
  unique()



wsAddYears <- wsmain_data %>%
  select(Year) %>% 
  arrange(desc(Year)) %>% 
  unique() 

wstop3s <- wsmain_data %>% 
  select(WSName2, WSPosition) %>% 
  filter(WSPosition %in%(c(1,2,3))) %>% 
  group_by(WSName2) %>% 
  add_tally() %>% 
  select(-WSPosition) %>% 
  unique()

wscompleteCount <- wsmain_data %>% 
  select(WSName2, WSPosition) %>% 
  filter(!is.na(WSPosition)) %>% 
  select(-WSPosition) %>% 
  group_by(WSName2) %>% 
  add_tally() %>% 
  unique()

wsfaveLadiess <- wsmain_data %>%
  select(WSName2, WSRace) %>% 
  group_by(WSName2, WSRace) %>% 
  add_tally() %>% 
  filter(n>1) %>% 
  arrange(WSName2, -n) %>% 
  unique()

# Weather Table

WMain_Data <- read.csv("Weather.csv",fileEncoding="latin1") %>% 
  
  select(EasterSunday, Year, SeniorFlow, First, MaxWind, MinTemperature, CompletionRate) %>% arrange(desc(Year), SeniorFlow, First)

Wmenus <- WMain_Data %>% select(EasterSunday, Year, SeniorFlow, First, MaxWind, MinTemperature, CompletionRate) %>% 
  arrange(desc(Year),desc(SeniorFlow), desc(First), desc(MaxWind), desc(MinTemperature), desc(CompletionRate) ) %>% unique()


Flow <- WMain_Data %>% select(SeniorFlow) %>% arrange(SeniorFlow) %>% unique()

FirstPlace <- WMain_Data %>% select(First) %>% arrange(First) %>% unique()

Wind <- WMain_Data %>% select(MaxWind) %>% arrange(MaxWind) %>%unique()

Temp <- WMain_Data %>% select(MinTemperature) %>%arrange(MinTemperature) %>% unique()

Completion <- WMain_Data %>% select(CompletionRate) %>% arrange(CompletionRate) %>% unique()



# DW Data

main_data <- read.csv("ExportRDW.csv",fileEncoding="latin1") %>% 
  # mutate(Notes = case_when(is.na(Notes) ~ "Finish", TRUE ~ Notes)) %>% 
  #  mutate(Time = format(Time, format = "%H:%M:%S")) %>% 
  select(BoatType, Year, Position, Class, SubClass, Ladies, Military, Name, Club, Time, DecimalTime,Trophies, Record, Notes) %>% 
  arrange(desc(Year), Position, Time, Ladies, Military) %>% 
  mutate(Name2 = paste(Name))


main_data$Year <- as.numeric(as.character(main_data$Year))
main_data$SubClass %<>% as.character
main_data$BoatType %<>% as.character
main_data$Class %<>% as.character
main_data$Ladies %<>% as.character
main_data$Military %<>% as.character
main_data$Club %<>% as.character
main_data$Notes %<>% as.character
main_data$Name %<>% as.character
main_data$Trophies %<>% as.character


table_data <- main_data %>% 
  select(-Name2, -BoatType)

menus <- main_data %>% 
  select(Year, Ladies, Military, Class, SubClass) %>% 
  arrange(desc(Year), Ladies, Military, Class, SubClass) %>% 
  unique()
#lists in order
paddlers <- main_data %>% 
  select(Name2) %>% 
  arrange(Name2) %>% 
  unique()



boattypes <- main_data %>% 
  select(BoatType) %>% 
  arrange(BoatType) %>% 
  unique()

divs <- main_data %>% 
  select(Military) %>% 
  arrange(Military) %>% 
  unique()

divssc <- main_data %>% 
  select(SubClass) %>% 
  arrange(SubClass) %>% 
  unique()

divsposition <- main_data %>% 
  select(Position) %>% 
  arrange(Position) %>% 
  unique()

divsclass <- main_data %>% 
  select(Class) %>% 
  arrange(Class) %>% 
  unique()

top3s <- main_data %>% 
  select(Name2, Position) %>% 
  filter(Position %in%(c(1,2,3))) %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  select(-Position) %>% 
  unique()


faveLadiess <- main_data %>%
  select(Name2, Ladies) %>% 
  group_by(Name2, Ladies) %>% 
  add_tally() %>% 
  filter(n>1) %>% 
  arrange(Name2, -n) %>% 
  unique()



data_sets <- c("Army","Canadian","Century","Civilian","European","Ladies","Ladies C2","Mixed","Navy","Over 50","Overseas","Police","RAF","Reserve","Scouts","Services","Tyne","U17 School","University","Vet Ladies","Veteran")



#-------------UI SECTION ----
ui <- fluidPage(
  
  
  
  theme = shinytheme("flatly"),
  
  list(tags$head(HTML('<link rel="icon", href="kayak.png", 
                      type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="DW and WS Results Database"
      )
  ),
  
  fluidRow(column(10, tags$h1("DW and Waterside Results Database in the R programming language")),
           column(2, tags$img(src="DW Logo.JPG", height = "80px", align = "left"))),
  
  tabsetPanel(
    
    tabPanel("Intro",
             fluidRow(column(12, tags$img(src="P1030108C.jpg", width = "60%"))),
             fluidRow(column(12, tags$h3("Congratulations to those Completing this gruelling event:")))),
    
    
    tabPanel("Application Intro",
             fluidRow(column(12, tags$h3("What Why How"))),
             fluidRow(column(12, tags$h3("What - This application is written in R code following the British Canoeing MRC app for Race Results."))), 
             fluidRow(column(12, tags$h3("Why - It is written in R code to enable dynamic graphics to be generated by the user with statistics.  The provision of an additional code base could offer an alternative future for the DW results database subject to a willing database administrator."))),
             fluidRow(column(12, tags$h3("How - The IBM Notes database (Web enabled in 2005) generates structured CSV files which are used by the application.  Please note that all comments and corrections must be made using the Master Notes system.  The R system data will only be updated annually."))),
             fluidRow(column(12, tags$h3("Code - The code is available to interested parties on GitHub."))),
             fluidRow(column(12, tags$h3("Acknowledgments-James Smythe (British Canoeing) and Callum Staff (data.giving.decisions)")))),
    
    tabPanel("DW Database",
             fluidRow(column(12, tags$h3("DW Database"))),
             fluidRow(column(4,selectInput("Year", "Pick one or more Years",c("All",unique(menus$Year)),selected = "All",multiple = TRUE)),
                      column(4,selectInput("Ladies","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
                      column(4,selectInput("div","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE))),
             # Create a new row for the table.
             fluidRow(column(12, DT::dataTableOutput("table")))),
    
    tabPanel("Paddler History", 
             fluidRow(
               column(6, tags$h3(textOutput("DW Paddlername"), tags$h5("Please check the DW Database for the correct Name. - Note: A Position of 0 indicates a completion outside the normal race rules"))),
               column(6,textInput("paddler", "Enter Paddler - The Name is Case Sensitive", value = ""))),
             fluidRow(column(3, tags$h3("DW Positions"))),
             fluidRow(column(12, plotOutput("positions")))),
    
    
    tabPanel("Attendance", 
             fluidRow(
               column(3, tags$h3("Civilian and Military Attendance")),
               column(3,selectInput("attdiv", "Choose Civilian and/or Arms", c(unique(divs$Military)), selected = "Civilian", multiple = TRUE)),
               column(3,selectInput("region", "Choose Boat Type:", c(unique(boattypes$BoatType)), selected = "MID", multiple = FALSE)),
               column(3,selectInput("attyear","Pick one or more Years",c(unique(menus$Year)),selected = "2017",multiple = TRUE))),
             fluidRow(column(12, plotOutput("Mattendance")))),
    
    tabPanel("Times", 
             fluidRow(
               column(3,selectInput("atttimeclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
               column(3,selectInput("Position", "Choose Position:", c(unique(divsposition$Position)), selected = "1", multiple = FALSE)),
               column(3,selectInput("attnyear","Plot after Year",c(unique(menus$Year)),selected = "1948",multiple = FALSE))),
             fluidRow(column(12, plotOutput("Time"))),
             fluidRow(column(12, tags$h3("Shows Times for Place over selected years.  NB Before 1971 No bank feeding was permitted.")))
             
    ),
    
    tabPanel("DW Weather",
             # Create a new row for the table.
             fluidRow(column(12, DT::dataTableOutput("Wtable")))),
    
    tabPanel("Graphs",
             fluidRow(column(12, plotOutput("Graphs"))),
             fluidRow(column(12, tags$h3("These charts illustrate that the most important factor for race times is flow.  Strong head winds will also affect times and completion rates")))),
    
    tabPanel("1st vs Flow",
             fluidRow(
               column(3,selectInput("attnfyear","Select a Year after 1970 for plot",c(unique(menus$Year)),selected = "1970",multiple = FALSE)),
               column(3,selectInput("comrate","With Completion Rate above",c(unique(Completion$CompletionRate)),multiple = FALSE)),
               column(3,selectInput("windg","With Wind below",c(unique(Wind$MaxWind)),selected = "46.5",multiple = FALSE)),
               column(3,selectInput("tempg","With Temp above",c(unique(Temp$MinTemperature)),selected = "-2",multiple = FALSE))),
             fluidRow(column(12, plotOutput("FGraphs"))),
             fluidRow(column(12, tags$h3("You may change the parameters for these plots but weather is not available before 1970.  The right chart is plotted by year to illustrate the importance of flow"))),
             fluidRow(column(12, tags$h3("The formula on the left chart indicates that first place with no flow would be 18.5 hours.  100cm/sec flow reduces the time by 1.59 hours (Before your changes)")))),
    
    tabPanel("WaterSide Database",
             fluidRow(column(12, tags$h3("WS Database - Click choices and press Delete to change"))),
             fluidRow(column(4,selectInput("Year","Pick one or more Years",c("All",unique(wsmenus$Year)),selected = "All",multiple = TRUE)),
                      column(4,selectInput("WSRace","Select Race",c("All",unique(wsmenus$WSRace)),selected = "All",multiple = TRUE)),
                      column(4,selectInput("WSCl","Select Class",c("All",unique(wsdivs$WSClass)),selected = "All",multiple = TRUE))),
             # Create a new row for the table.
             fluidRow(column(12, DT::dataTableOutput("WStable")))),
    
    tabPanel("WS Paddler History", 
             fluidRow(
               column(6, tags$h3(textOutput("WS paddlerName"), tags$h3("Please check the Waterside Database for the correct Name"))),
               column(6,textInput("WSpaddler", "Choose Paddler:(Name is Case Sensitive)", value = ""))),
             fluidRow(column(3, tags$h3("WS Positions"), tags$h4(textOutput("WS top3s")), tags$h4(textOutput("WS medals")))),
             fluidRow(column(12, plotOutput("WSPositions")))),
    
    
    tabPanel("WS Race Attendance", 
             fluidRow(
               column(6, tags$h3("WSRace Attendance - Click choices and press Delete to change")),
               column(3,selectInput("attclass", "Choose one or more Classes", c(unique(wsdivs$WSClass)), selected = "K2 Senior", multiple = TRUE)),
               column(3,selectInput("wsattyear","Pick one or more Years",c("All",unique(wsmenus$Year)),selected = "2018",multiple = TRUE))),
             fluidRow(column(12, plotOutput("WSattendance")))),
    
    #   tabPanel("Comparison Data",
    
    # Create a new row for the table.
    #         fluidRow(column(12, DT::dataTableOutput("Comptable")))),
    
    
    tabPanel("Comparison Graphs",
             fluidRow(
               column(6, tags$h3("Comparison Data")),
               column(6,selectInput("comattyear","Pick Year",c(unique(commenus$Year)),selected = "2017",multiple = FALSE))),
             fluidRow(column(12, plotOutput("CompGraphs")))
    )
  )
  
  )

#----------------SERVER FUNCTION ---------------------

server <- function(input, output, session)
{
  # DW Database table with reactive filtering
  output$table <- DT::renderDataTable({
    DT::datatable({data <- table_data
    if (input$Year != "All") {data <- data[data$Year %in% input$Year,]}
    if (input$Ladies != "All") {data <- data[data$Ladies %in% input$Ladies,]}
    if (input$div != "All") {data <- data[data$Military %in% input$div,]}
    data #i.e. render this dataset as a table
    },
    filter = list(position = 'top', clear = FALSE),
    options = list(pageLength = 15), 
    rownames = FALSE)})
  
  # WS Database table with reactive filtering
  output$WStable <- DT::renderDataTable({
    DT::datatable({data <- wstable_data
    if (input$Year != "All") {data <- data[data$Year %in% input$Year,]}
    if (input$WSRace != "All") {data <- data[data$WSRace %in% input$WSRace,]}
    if (input$WSCl != "All") {data <- data[data$WSClass %in% input$WSCLass,]}
    data #i.e. render this dataset as a table
    },
    filter = list(position = 'top', clear = FALSE),
    options = list(pageLength = 15), 
    rownames = FALSE)
  })
  
  
  
  #filtered inputs for main wstable
  wsmenus_filtered1 <- reactive({if ("All" %in% input$Year) { wsmenus} else {wsmenus %>% filter(Year %in% input$Year)}})
  observe({updateSelectInput(session, "WSRace", choices = c("All", wsmenus_filtered1()$WSRace), selected = "All")})
  wsmenus_filtered2 <- reactive({if ("All" %in% input$WSRace) {wsmenus_filtered1()} else {wsmenus_filtered1() %>% filter(WSRace %in% c(input$WSRace))}})
  observe({updateSelectInput(session, "div", choices = c("All",wsmenus_filtered2()$WSClass), selected = "All")})
  #filtered inputs for main table
  menus_filtered1 <- reactive({if ("All" %in% input$Year) {menus} else {menus %>% filter(Year %in% input$Year)}})
  observe({updateSelectInput(session, "Ladies", choices = c("All", menus_filtered1()$Ladies), selected = "All")})
  menus_filtered2 <- reactive({if ("All" %in% input$Ladies) {menus_filtered1()} else {menus_filtered1() %>% filter(Ladies %in% c(input$Ladies))}})
  observe({updateSelectInput(session, "div", choices = c("All",menus_filtered2()$Military), selected = "All")})
  
  #define outputs for individual paddler
  output$paddlername <- renderText({paste("Paddler History:",input$paddler)})
  output$no_races <- renderText({paste(nrow(main_data[main_data$Name2 == input$paddler,]),"races entered,")})
  output$comprate <- renderText({paste(round(completeCount$n[completeCount$Name2 == input$paddler]/nrow(main_data[main_data$Name2 == input$paddler,])*100, digits = 0),"% completion rate")})
  output$top3s <- renderText({paste("Number of Top 3 finishes:",top3s$n[top3s$Name2 == input$paddler])})
  output$medals <- renderText({paste("",medals$n[medals$Name2 == input$paddler])})
  output$faveraces <- renderTable({faveraces1 <- faveLadies %>% filter(Name2 == input$paddler) %>% rename(Entries = n) %>% head(5)}, rownames = FALSE)
  
  
  output$WSattendance <- renderPlot({
    
    attchartdata <- wsmain_data %>% 
      select( WSRace, Year, WSClass) %>%
      filter(WSClass %in% input$attclass,Year == input$wsattyear)
    
    
    ggplot(attchartdata, aes(WSRace))+
      geom_bar(aes(fill = WSClass), stat = "count")+
      coord_flip()+
      facet_wrap(~Year)
    
  })
  
  
  ##### 
  
  
  
  
  output$races <- renderPlot({racechartdata <- main_data %>% select(Year, Name2) %>% filter(Name2 %like% input$paddler)
  ggplot(racechartdata, aes(Year))+geom_histogram(aes(fill = Year), stat = "count")+ylab("Number of Races Entered")})
  
  output$positions <- renderPlot({poschartdata <- main_data %>% select(Year, Position, Class, Name2) %>% filter(str_detect(Name2, input$paddler))
  ggplot(poschartdata)+geom_point(aes(Year, Position, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks = unique(main_data$Position)) 
  })
  
  
  
  output$Mattendance <- renderPlot({
    attchartdata <- main_data %>% 
      select(BoatType, Ladies, Year, Military, Class, Club, SubClass) %>% 
      filter(BoatType == input$region, Year %in% input$attyear, Military %in% input$attdiv)
    bxp <- ggplot(attchartdata, aes(Ladies))+geom_bar(aes(fill = Military), stat = "count")+coord_flip()+facet_wrap(~Year) 
    bxp
    dp <- ggplot(attchartdata, aes(Class))+geom_bar(aes(fill = Military), stat = "count")+coord_flip()+facet_wrap(~Year)
    dp
    sc <- ggplot(attchartdata, aes(SubClass))+geom_bar(aes(fill = Military), stat = "count")+coord_flip()+facet_wrap(~Year) 
    sc
    ggarrange(bxp, dp, sc, ncol = 3, nrow = 1)})
  
  #menus_filtered1 <- reactive({if ("All" %in% input$Year) {menus} else {menus %>% filter(Year %in% input$Year)}})
  # observe({updateSelectInput(session, "Year", choices = c("All", menus_filtered1()$Year), selected = "All")})
  
  
  output$Time <- renderPlot({poschartdata <- main_data %>% select(Year, Position, Class, DecimalTime) %>% 
    filter(Class == input$atttimeclass, Position == input$Position, Year >= input$attnyear)
  a <- ggplot(poschartdata)+geom_jitter(aes(Year, DecimalTime, color = Class),  size = 3)
  
  
  ggarrange(a,ncol = 1, nrow = 1)
  })
  
  
  output$Wtable <- DT::renderDataTable({DT::datatable({data <- WMain_Data
  data #i.e. render this dataset as a table
  },options = list(pageLength = 15), rownames = FALSE)})
  
  #comparison output
  output$Comptable <- DT::renderDataTable({DT::datatable({data <- Comparison_data
  data #i.e. render this dataset as a table
  },options = list(pageLength = 15), rownames = FALSE)})
  
  
  
  output$FGraphs <- renderPlot({poschartdata <- WMain_Data %>% 
    select(Year, SeniorFlow, First, MaxWind, MinTemperature, CompletionRate) %>% 
    filter(Year >= input$attnfyear, CompletionRate >= input$comrate, MaxWind <= input$windg, MinTemperature >= input$tempg) 
  a <-  ggplot(poschartdata,aes(x =SeniorFlow,y = First))+
    geom_point(aes(SeniorFlow, First, color = MaxWind),  size = 5)
  
  
  lm_eqn = function(SeniorFlow, First, poschartdata){
    m <- lm(First ~ SeniorFlow, poschartdata);
    eq <- substitute(italic(SeniorFlow) == b %.% italic(First) + a,
                     list(a = format(coef(m)[1], digits = 3), 
                          b = format(coef(m)[2], digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  
 d <- ggplot(poschartdata,aes(x =SeniorFlow,y = First))+geom_point(color="red")+stat_smooth(method = "lm")+
    annotate("text", x = 75, y = 21, label = lm_eqn(poschartdata$SeniorFlow, poschartdata$First, poschartdata), color="black", size = 5, parse=TRUE)
  
  
  
  
  poschartdata1 <- main_data %>% select(Year, Position, Class, DecimalTime) %>% 
    filter(Class == input$atttimeclass, Position == input$Position, Year >= input$attnfyear)
  b <- ggplot(poschartdata1)+geom_point(aes(Year, DecimalTime, color = Class),  size = 3)
  
  
  #b <-  ggplot(poschartdata, aes(x = SeniorFlow, y = First)) + 
  #geom_point() +
  #stat_smooth(method = "lm", col = "red")
  
  ggarrange(d,b,ncol = 2, nrow = 1)})
  
  #+ geom_point() +stat_smooth(method = "lm", col = "red")
  
  
  output$Graphs <- renderPlot({poschartdata <- WMain_Data %>% 
    select(Year, SeniorFlow, First, MaxWind, MinTemperature, CompletionRate)  
  
  a <-  ggplot(poschartdata,aes(First,SeniorFlow))+
    geom_point(aes(color = MaxWind), size = 3)
  b <-  ggplot(poschartdata)+
    geom_point(aes(First, MaxWind, color = Year),  size = 3)
  c <-  ggplot(poschartdata)+
    geom_point(aes(First, MinTemperature, color = MaxWind),  size = 3)
  
  d <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, SeniorFlow, color = MaxWind),  size = 3)
  e <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, MaxWind, color = MinTemperature),  size = 3)
  f <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, MinTemperature, color = MaxWind),  size = 3)
  g <-  ggplot(poschartdata)+
    geom_point(aes(MaxWind, MinTemperature, color = Year),  size = 3)
  
  
  ggarrange(a,b,c,d,e,f,g,ncol = 4, nrow = 2)})
  
  
  #output$Time <- renderPlot({poschartdata <- main_data %>% select(Year, Position, Class, DecimalTime, Time) %>% 
  # filter(Class == input$atttimeclass, Position == input$Position, Year >= input$attnyear)
  #  ggplot(poschartdata)+geom_point(aes(Year, DecimalTime, color = Class),  size = 5)})
  
  
  
  
  output$CompGraphs <- renderPlot({poschartdata <- Comparison_data %>% 
    select(Year, WatersideA, WatersideB, WatersideC,WatersideD,DWSenior, DWJunior, DWEndeavour, DWVetJunior, DWSingles) %>%
    filter(Year == input$comattyear)
  a <-  ggplot(poschartdata)+
    geom_point(aes(x=WatersideA, y=WatersideB, color= "WatersideB"),  size = 2)+
    geom_point(aes(WatersideA, WatersideC, color= "WatersideC"),  size = 2)+
    geom_point(aes(WatersideA, WatersideD, color= "WatersideD"),  size = 2)
  
  
  
  
  
  b <-  ggplot(poschartdata)+
    geom_point(aes(WatersideD, DWSenior, color= "DWSenior"),  size = 2)+ 
    geom_point(aes(WatersideD, DWJunior, color= "DWJunior"),  size = 2)+ 
    geom_point(aes(WatersideD, DWEndeavour, color= "DWEndeavour"),  size = 2)+ 
    geom_point(aes(WatersideD, DWVetJunior, color= "DWVetJunior"),  size = 2)+ 
    geom_point(aes(WatersideD, DWSingles, color= "DWSingles"),  size = 2)
  
  c <- ggplot(poschartdata, aes(x = WatersideA, y = DWSenior)) + 
    geom_point(aes(x = WatersideA, y = DWSenior)) +
    stat_smooth(method = "lm", col = "red")
  
  lm_eqn = function(WatersideA, DWSenior, poschartdata){
    m <- lm(DWSenior ~ WatersideA, poschartdata);
    eq <- substitute(italic(DWSenior) == b %.% italic(WatersideA) + a,
                     list(a = format(coef(m)[1], digits = 3), 
                          b = format(coef(m)[2], digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  
  d <- ggplot(poschartdata,aes(x =WatersideA,y = DWSenior))+geom_point(color="red")+stat_smooth(method = "lm")+
    annotate("text", x = 3, y = 35, label = lm_eqn(poschartdata$watersideA, poschartdata$DWSenior, poschartdata), color="black", size = 5, parse=TRUE)
  
  lm_eqn = function(WatersideD, DWSenior, poschartdata){
    m <- lm(DWSenior ~ WatersideD, poschartdata);
    eq <- substitute(italic(DWSenior) == b %.% italic(WatersideD) + a,
                     list(a = format(coef(m)[1], digits = 3), 
                          b = format(coef(m)[2], digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  e <- ggplot(poschartdata,aes(x = WatersideD, y = DWSenior))+geom_point(color="red")+geom_smooth(method = "lm")+
    annotate("text", x = 7, y = 35, label = lm_eqn(poschartdata$watersideA, poschartdata$watersideB, poschartdata), color="black", size = 5, parse=TRUE)
  
  
  
  
  ggarrange(a,b,d,e,ncol = 2, nrow = 2)})
  
  
  
  
  #filtered inputs for ws main table
  wsmenus_filtered1 <- reactive({if ("All" %in% input$Year) {wsmenus} else {wsmenus %>% filter(Year %in% input$Year)}})
  
  observe({updateSelectInput(session, "WSRace", choices = c("All", wsmenus_filtered1()$WSRace), selected = "All")})
  
  wsmenus_filtered2 <- reactive({if ("All" %in% input$WSRace) {wsmenus_filtered1()} else {wsmenus_filtered1() %>% filter(WSRace %in% c(input$WSRace))}})
  
  observe({updateSelectInput(session, "div", choices = c("All",wsmenus_filtered2()$WSClass), selected = "All")})
  
  
  
  
  
  #define outputs for ws individual paddler
  output$paddlerWSName <- renderText({paste("Paddler History:",input$paddler)})
  
  output$no_WSRaces <- renderText({paste(nrow(wsmain_data[wsmain_data$WSName2 == input$paddler,]),"WSRaces entered,")})
  
  output$comprate <- renderText({
    paste(round(wscompleteCount$n[wscompleteCount$WSName2 == input$paddler]/nrow(wsmain_data[wsmain_data$WSName2 == input$paddler,])*100, digits = 0),"% completion rate")})
  
  output$wstop3s <- renderText({paste("Number of Top 3 finishes:",wstop3s$n[wstop3s$WSName2 == input$paddler])})
  
  output$wsmedals <- renderText({paste("",wsmedals$n[wsmedals$WSName2 == input$paddler])})
  
  output$faveWSRaces <- renderTable({faveWSRaces1 <- faveLadies %>% filter(WSName2 == input$paddler) %>% reWSName(Entries = n) %>% head(5)}, rowWSNames = FALSE)
  
  
  output$WSRaces <- renderPlot({WSRacechartdata <- wsmain_data %>% select(Year, WSName2) %>%filter(WSName2 == input$paddler)
  ggplot(WSRacechartdata, aes(Year))+geom_histogram(aes(fill = Year), stat = "count")+ylab("Number of WSRaces Entered")})
  
  output$WSPositions <- renderPlot({poschartdata <- wsmain_data %>% select(Year, WSPosition, WSClass, WSName2) %>%filter(str_detect(WSName2,input$WSpaddler))
  ggplot(poschartdata)+
    geom_point(aes(Year, WSPosition, color = WSClass), position=position_jitter(width=0.1, height=0.1), size = 5)+
    scale_y_continuous(breaks = unique(wsmain_data$WSPosition))})
  
  
  
  output$wstable <- renderPlot({output$wstable <- DT::renderDataTable(wstable_data)})
  
  output$wstable <- DT::renderDataTable({DT::datatable({data <- wstable_Data
  data #i.e. render this dataset as a table
  },options = list(pageLength = 15), rownames = FALSE)})
  
  
}




shinyApp(ui, server)