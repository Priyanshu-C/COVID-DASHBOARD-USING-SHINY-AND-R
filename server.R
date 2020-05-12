server <- function(input, output) {
  
  output$picture <- renderImage({
    filename <- 'logo.png'
    list(src = filename)
  }, deleteFile = FALSE)
  
  #HeaderBAR for COMNFIRMED DEATHS AND RECOVERY
  output$vbox1 <- renderValueBox({
    #CONFIRMED BOX
     valueBox(
      "Confirmed",
      HEADERDATAA[1,2], width = 4,
      icon = icon("heartbeat")
    )
  })
  
  output$vbox2 <- renderValueBox({
    #RECOVERY BOX
    valueBox(
      "Recovered",
      HEADERDATAR[1,2], width = 4,
      icon = icon("smile")
    )
  })
  
  output$vbox3 <- renderValueBox({
    #DEATHS BOX
    valueBox(
      "Deaths",
      HEADERDATAD[1,2], width = 4,
      icon = icon("frown")
    )
  })
  
  output$vbox4 <- renderValueBox({
    #CALCULATING ACTIVE AND PLOTTING ACTIVE 
    valueBox(
      "Active",
      HEADERDATAA[1,2]-HEADERDATAR[1,2]-HEADERDATAD[1,2],
      width = 4,
      icon = icon("hospital")
    )
  })
  

  #STATE WISE CHART

  output$statewisecount <- renderPlotly({
    
    globaldata2$date_announced <- as.Date(format(as.Date(globaldata2$date_announced,"%d/%m/%Y"),"%Y-%m-%d"))
    globaldata2 <- globaldata2[(globaldata2$date_announced >= input$daterange1[1] & globaldata2$date_announced <= input$daterange1[2]),]
    
    #DATA CLEANING FOR STATE PLOT 
    
    StateData <- subset(globaldata2, select = detected_state)
    StateData <- as.data.frame(table(StateData))
    
    StateRecovered <- filter(globaldata2,current_status == 'Recovered')
    StateRecovered <- subset(StateRecovered, select = detected_state)
    StateRecovered <- as.data.frame(table(StateRecovered))
    
    StateHospitalized <- filter(globaldata2,current_status == 'Hospitalized')
    StateHospitalized <- subset(StateHospitalized, select = detected_state)
    StateHospitalized <- as.data.frame(table(StateHospitalized))
    
    StateDeceased <- filter(globaldata2,current_status == 'Deceased')
    StateDeceased <- subset(StateDeceased, select = detected_state)
    StateDeceased <- as.data.frame(table(StateDeceased))
    
    #Plotting the STATE CHART
    
    statewisechart <- plot_ly(StateData, x = reorder(StateData$StateData,StateData$Freq),
                              y = StateData$Freq, name = "Confirmed",type = "bar",marker = list(color = active_color)) 
    statewisechart <- statewisechart %>% layout(barmode = 'stack',
                                                yaxis = list(title = "Total Cases (log scaled)", type = "log"),
                                                
                                                hovermode = "compare",
                                                legend = list(x = 0.1, y = 0.9),
                                                margin =  list(
                                                  # l = 60,
                                                  # r = 40,
                                                  b = 5,
                                                  t = 5,
                                                  pad = 0
                                                ))
    statewisechart <-  statewisechart %>%  add_trace(y= StateDeceased$Freq, name = "Deaths",marker = list(color = death_color))
    statewisechart <-  statewisechart %>% add_trace(y= StateRecovered$Freq, name = "Recovered",marker = list(color = recovered_color))
    statewisechart
    
  })

# Line PLOT for DAILY AND TOTAL CASES

  output$LINEPLOT <- renderPlotly({
    
    
    globaldata2$date_announced <- format(as.Date(globaldata2$date_announced,"%d/%m/%Y"),"%Y/%m/%d")
    globaldata2 <- globaldata2[(globaldata2$date_announced >= input$daterange1[1] & globaldata2$date_announced <= input$daterange1[2]),]
    
    #DATA CLEANING FOR LINE CHART
    newD2 <- globaldata2 %>% 
      group_by(date_announced,current_status) %>%
      summarise(count = n())  
    
    LineChartR = filter(newD2, current_status == "Recovered")
    LineChartD = filter(newD2, current_status == "Deceased")
    LineChartA = filter(newD2, current_status == "Hospitalized")
    
    Combined <- merge(LineChartA,LineChartD,by = 'date_announced',all = TRUE)
    Combined <- Combined <- merge(Combined,LineChartR,by = 'date_announced',all = TRUE)
    Combined <- Combined[order(as.Date(Combined$date_announced, format="%Y/%m/%d")),]
    
    Combined$count.x <- ifelse(!is.na(Combined$count.x),Combined$count.x,0)
    Combined$count.y <- ifelse(!is.na(Combined$count.y),Combined$count.y,0)
    Combined$count <- ifelse(!is.na(Combined$count),Combined$count,0)
    
    dummy <- as.data.frame(transform(Combined,cumFreq = cumsum(Combined$count.x)))
    Combined$count.x <- dummy$cumFreq
    
    dummy2 <- as.data.frame(transform(Combined,cumFreq = cumsum(Combined$count.y)))
    Combined$count.y <- dummy2$cumFreq
    
    dummy3 <- as.data.frame(transform(Combined,cumFreq = cumsum(Combined$count)))
    Combined$count <- dummy3$cumFreq
    
    Combined <- rename(Combined, "Hospitalized" = "count.x")
    Combined <- rename(Combined, "Deceased" = "count.y")
    Combined <- rename(Combined, "Recovered" = "count")
    Combined <- Combined[c("date_announced","Hospitalized","Recovered","Deceased")]
    
    #PLOTTING THE LINE CHART
    
    fig <- plot_ly(Combined, x = as.Date(Combined$date_announced,"%Y/%m/%d"),y = Combined$Hospitalized,name = 'Daily Confirmed',
                   type = "scatter",mode = "lines",line = list(color = active_color)
    )
    fig = fig %>% add_trace(y = Combined$Recovered,name = 'Daily Recovered',line = list(color = recovered_color),
                            mode = "lines")
    fig = fig %>% add_trace(y = Combined$Deceased,name = 'Daily Deceased',line = list(color = death_color),
                            mode = "lines")
    fig = fig %>% layout(title = "",
                         yaxis = list(title = "Cumulative Number of Cases"),
                         xaxis = list(title = "Date"),
                         hovermode = "compare")
    fig
    
    
  })
  
  #PLOTS ON ANALYTICS PAGE
  
  #GenderWisePlot
  
  output$GenderWisePlot <- renderPlotly(
    
    plot_ly(gender, labels = gender$gender, values = gender$Freq,
    textposition = 'inside',
    textinfo = 'label+percent',
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = 'text',
    marker = list(colors = c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)'),
                  line = list(color = '#FFFFFF', width = 1)), type = 'pie')
    %>% layout(title = 'Gender Wise Cases',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  )
  
  #PatientAgePlot
  
  
  output$Patientageplot <- renderPlotly(

     plot_ly(age_plot, labels = age_plot$AgeRange, values = ~age,title = 'Patient Age',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             marker = list(colors = c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)'),
                           line = list(color = '#FFFFFF', width = 1)), type = 'pie')
     %>% layout(
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  )
  
  #Nationality
  
  output$NationalityPlot <- renderPlotly(
    
    plot_ly(Nationality, labels = Nationality$Nationality, values = Nationality$Freq,
    textposition = 'inside',
    textinfo = 'label+percent',
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = 'text',marker = list(colors = c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)'),
                                     line = list(color = '#FFFFFF', width = 1)), type = 'pie')
    %>% layout(title = 'Nationality',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  )
  
  #PLOTS ON MAP PAGE
  
  output$MapPlot <- renderPlotly(
    
    ggplot()+
      geom_polygon(data = final.plot,aes(x = long, y = lat, group = group,fill = get(input$choice1)),color = "black", size = 0.25)+
      scale_fill_gradient(low="blue", high="red")+
      coord_map()
    
  )
  
  
  
}

