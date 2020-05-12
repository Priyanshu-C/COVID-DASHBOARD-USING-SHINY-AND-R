  
ui <- dashboardPage(
  dashboardHeader(
    #MAIN HEADER FOR LOGO AND DATA RANGE SECTION
    fluidRow(align="left", imageOutput("picture", width = "100%", height = "200px", inline = FALSE)),
    titlePanel(dateRangeInput("daterange1", "Date range:",
                            format = "yyyy/mm/dd",
                              start = "2019/09/02",
                              end   = Sys.Date())
                  )),
  dashboardSidebar(
    #SIDE BAR CONTAINING DIFFERENT TABS LIKE DASHBOARD,ANALYTICS AND MAPS
                    side = "left",
                   sidebarMenu(
                     menuItem(tabName = "Dashboard", text = "Dashboard", icon = icon("home")),
                     menuItem(tabName = "Analytics", text = "Analytics", icon = icon("home")),
                     menuItem(tabName = "MAP", text = "MAP", icon = icon("smile")))),
  dashboardBody(
    tabItems(
    #MAIN BODY
      #HEADER VALUE BOXES FOR CURRENT DATA OF INDIA
      tabItem(tabName = "Dashboard",
              valueBoxOutput("vbox1",width = 4),
              valueBoxOutput("vbox4",width = 4),
              valueBoxOutput("vbox2",width = 4),
              valueBoxOutput("vbox3",width = 4),
              fluidRow(
                #STATE CHART BOX
                box(title = "State Wise Chart", color = "red", width = 8,
                    plotlyOutput("statewisecount")),
                #LINE CHART BOX
                box(title = "Cases Daily/Total", color = "blue", width = 8,plotlyOutput("LINEPLOT")
                ))),
      
        #ANALYTICS TAB
      tabItem(tabName = "Analytics",
              fluidRow(
                #PATIENT AGE WISE PLOT BOX
                box(title = "PATIENT AGE", color = "red", width = 8,
                    plotlyOutput("Patientageplot")),
                #NATIONALITY WISE PLOT BOX
                box(title = "Nationality", color = "blue", width = 8,
                        plotlyOutput("NationalityPlot")),
                #GENDER WISE PLOT BOX
                box(title = "GENDER WISE CASES", color = "red", width = 8,
                    plotlyOutput("GenderWisePlot"))
              )),
      #MAP TAB 
      tabItem(tabName = "MAP",
              #MAIN BOX FOR PLOTTING AND SELECTION DIFFERENT CHOICES OF PLOTTING THE MAP
              fluidRow(
                box(
                  selectInput("choice1","Choice",choices = c("Active","Hospitalized","Recovered","Deceased")),
                  title = "Revenue per Account by GGPLOT"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                 ,plotlyOutput("MapPlot", height = "600px")
                )
                
              ))
    
  )), theme = "darkly")
