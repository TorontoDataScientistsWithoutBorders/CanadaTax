shinyServer(function(input, output,session) {
  url <- a("My Github Page", href="https://github.com/agrawalpiyush2309/My-Work-in-Shiny")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })  
  output$dri <- renderInfoBox({
    infoBox(
      title = tags$b("People-2015", style = "font-size: 50%;"),
      value = tags$b(paste(round(valueBoxesPeople()/1000000,1),"M")),
      color = "navy",
      fill = TRUE,
      icon = icon("female")
    )    
  }) 
  output$dri1 <- renderInfoBox({
    infoBox(
      title = tags$b("Taxpayers-2015", style = "font-size: 50%;"),
      value = tags$b(paste(round(valueBoxesTax()/1000000,1),"M")),
      color = "navy",
      fill = TRUE,
      icon = icon("male")
    )    
  })    
  observeEvent(input$panel_1_tab1, {
    updateTabsetPanel(session, "panel1", "Tab1")
  }) 
  observeEvent(input$panel_1_tab2, {
    updateTabsetPanel(session, "panel1", "Tab2")
  })
  observeEvent(input$panel_1_tab3, {
    updateTabsetPanel(session, "panel1", "Tab3")
  }) 
  observeEvent(input$panel_1_tab4, {
    updateTabsetPanel(session, "panel1", "Tab4")
  })
  observeEvent(input$panel_1_tab5, {
    updateTabsetPanel(session, "panel1", "Tab5")
  }) 
  observeEvent(input$panel_1_tab6, {
    updateTabsetPanel(session, "panel1", "Tab6")
  }) 
  observeEvent(input$panel_1_tab7, {
    updateTabsetPanel(session, "panel1", "Tab7")
  })  
  observeEvent(input$panel_2_tab1, {
    updateTabsetPanel(session, "panel2", "Tab1")
  }) 
  observeEvent(input$panel_2_tab2, {
    updateTabsetPanel(session, "panel2", "Tab2")
  })
  observeEvent(input$panel_2_tab3, {
    updateTabsetPanel(session, "panel2", "Tab3")
  }) 
  observeEvent(input$panel_2_tab4, {
    updateTabsetPanel(session, "panel2", "Tab4")
  })
  observeEvent(input$panel_2_tab5, {
    updateTabsetPanel(session, "panel2", "Tab5")
  }) 
  observeEvent(input$panel_2_tab6, {
    updateTabsetPanel(session, "panel2", "Tab6")
  }) 
  observeEvent(input$panel_2_tab7, {
    updateTabsetPanel(session, "panel2", "Tab7")
  })  
  province <- reactive({
    if(input$Pr=="ALL") newfile
    else newfile[newfile$PrTr==input$Pr,]
  })
  tax <- reactive({
    if(input$Pr=="ALL"){
      taxdata %>% filter(DON=="Number of taxfilers" & (!grepl(',', Geo)))
    }
    else{
      taxdata$Geo <- as.character(taxdata$Geo)
      taxdata
      }
      
  })
  valueBoxesPeople <- reactive({
    if(input$Pr=="ALL"){
      taxdata %>% filter(DON=="Number of persons" & Geo=="Canada" & ref_date==2015) %>% 
      select(c(value))
    }
    else{
      taxdata %>% filter(DON=="Number of persons" & grepl(input$Pr,Geo) & (!grepl(',',Geo)) & ref_date==2015) %>% select(c(value))
    }
    
  }) 
  valueBoxesTax <- reactive({
    if(input$Pr=="ALL"){
      taxdata %>% filter(DON=="Number of taxfilers" & Geo=="Canada" & ref_date==2015) %>% 
        select(c(value))
    }
    else{
      taxdata %>% filter(DON=="Number of taxfilers" & grepl(input$Pr,Geo) & (!grepl(',',Geo)) & ref_date==2015) %>% select(c(value))
    }
    
  })  
  
  p1data <- reactive({
    if(input$Pr=="ALL"){
    taxdata1 %>% filter(DON=="Percentage of taxfilers aged 0 to 24 years" & (grepl('Canada', Geo)))
    } else{
    taxdata1 %>% filter(DON=="Percentage of taxfilers aged 0 to 24 years" & (grepl(input$Pr, Geo)) & 
                      (!grepl(",", Geo)))  
    }   
  })
  p2data <- reactive({
    if(input$Pr=="ALL"){
      taxdata1 %>% filter(DON=="Percentage of taxfilers aged 25 to 44 years" & (grepl('Canada', Geo)))
    } else{
      taxdata1 %>% filter(DON=="Percentage of taxfilers aged 25 to 44 years" & (grepl(input$Pr, Geo)) & 
                           (!grepl(",", Geo)))  
    }   
  })  
  p3data <- reactive({
    if(input$Pr=="ALL"){
      taxdata1 %>% filter(DON=="Percentage of taxfilers aged 45 to 64 years" & (grepl('Canada', Geo)))
    } else{
      taxdata1 %>% filter(DON=="Percentage of taxfilers aged 45 to 64 years" & (grepl(input$Pr, Geo)) & 
                           (!grepl(",", Geo)))  
    }   
  })
  p4data <- reactive({
    if(input$Pr=="ALL"){
      taxdata1 %>% filter(DON=="Percentage of taxfilers aged 65 years and over" & (grepl('Canada', Geo)))
    } else{
      taxdata1 %>% filter(DON=="Percentage of taxfilers aged 65 years and over" & (grepl(input$Pr, Geo)) & 
                           (!grepl(",", Geo)))  
    }   
  })
  p5data <- reactive({
    if(input$Pr=="ALL"){
      taxdata2 %>% filter(DON=="Percentage of persons with total income $15,000 and over" & (grepl('Canada', Geo)))
    } else{
      taxdata2 %>% filter(DON=="Percentage of persons with total income $15,000 and over" & (grepl(input$Pr, Geo)) & 
                            (!grepl(",", Geo)))  
    }   
  })
  p6data <- reactive({
    if(input$Pr=="ALL"){
      taxdata2 %>% filter(DON=="Percentage of persons with total income $25,000 and over" & (grepl('Canada', Geo)))
    } else{
      taxdata2 %>% filter(DON=="Percentage of persons with total income $25,000 and over" & (grepl(input$Pr, Geo)) & 
                            (!grepl(",", Geo)))  
    }   
  })  
  p7data <- reactive({
    if(input$Pr=="ALL"){
      taxdata2 %>% filter(DON=="Percentage of persons with total income $35,000 and over" & (grepl('Canada', Geo)))
    } else{
      taxdata2 %>% filter(DON=="Percentage of persons with total income $35,000 and over" & (grepl(input$Pr, Geo)) & 
                            (!grepl(",", Geo)))  
    }   
  })
  p8data <- reactive({
    if(input$Pr=="ALL"){
      taxdata2 %>% filter(DON=="Percentage of persons with total income $50,000 and over" & (grepl('Canada', Geo)))
    } else{
      taxdata2 %>% filter(DON=="Percentage of persons with total income $50,000 and over" & (grepl(input$Pr, Geo)) & 
                            (!grepl(",", Geo)))  
    }   
  })  
  p9data <- reactive({
    if(input$Pr=="ALL"){
      taxdata2 %>% filter(DON=="Percentage of persons with total income $75,000 and over" & (grepl('Canada', Geo)))
    } else{
      taxdata2 %>% filter(DON=="Percentage of persons with total income $75,000 and over" & (grepl(input$Pr, Geo)) & 
                            (!grepl(",", Geo)))  
    }   
  })
  p10data <- reactive({
    if(input$Pr=="ALL"){
      taxdata2 %>% filter(DON=="Percentage of persons with total income $100,000 and over" & (grepl('Canada', Geo)))
    } else{
      taxdata2 %>% filter(DON=="Percentage of persons with total income $100,000 and over" & (grepl(input$Pr, Geo)) & 
                            (!grepl(",", Geo)))  
    }   
  })
  p11data <- reactive({
    if(input$Pr=="ALL"){
      taxdata3 %>% filter(grepl("Percentage of taxfilers,",DON) & (grepl('Canada', Geo)))
    } else{
      taxdata3 %>% filter(grepl("Percentage of taxfilers,",DON) & (grepl(input$Pr, Geo)) & (!grepl(",", Geo)))  
    } 
  }) 
  p12data <- reactive({
    if(input$Pr=="ALL"){
      taxdata %>% filter(grepl("Average age of",DON) & (grepl('Canada', Geo)))
    } else{
      taxdata %>% filter(grepl("Average age of",DON) & (grepl(input$Pr, Geo)) & (!grepl(",", Geo)))
    } 
  }) 
  p13data <- reactive({
    if(input$Pr=="ALL"){
      taxdata %>% filter(grepl("Median total income",DON) & (grepl('Canada', Geo)))
    } else{
      taxdata %>% filter(grepl("Median total income",DON) & (grepl(input$Pr, Geo)) & (!grepl(",", Geo)))
    } 
  })  
  p14data <- reactive({
    if(input$Pr=="ALL"){
      taxdata %>% filter(grepl("Median employment income",DON) & (grepl('Canada', Geo)))
    } else{
      taxdata %>% filter(grepl("Median employment income",DON) & (grepl(input$Pr, Geo)) & (!grepl(",", Geo)))
    } 
  })
  # p15data <- reactive({
  #   if(input$Pr=="ALL"){
  #     taxdata %>% filter(grepl("index of median total income",DON) & (grepl('Canada', Geo)))
  #   } else{
  #     taxdata %>% filter(grepl("index of median total income",DON) & (grepl(input$Pr, Geo)) & (!grepl(",", Geo)))
  #   } 
  # })  
  output$p1 <- renderPlotly({
    p1data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
    layout(title = "",autosize = F, width = 430, height = 158,
    xaxis = list(showgrid = FALSE,title = "Number of taxfilers aged 0 to 24 years",tickfont=axisfont,       titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p2 <- renderPlotly({
    p2data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of taxfilers aged 25 to 44 years",tickfont=axisfont,       titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  }) 
  output$p3 <- renderPlotly({
    p3data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of taxfilers aged 45 to 64 years",tickfont=axisfont,       titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p4 <- renderPlotly({
    p4data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of taxfilers aged 65 years and over",tickfont=axisfont,       titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p5 <- renderPlotly({
    p5data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of persons with total income $15,000 and over",tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p6 <- renderPlotly({
    p6data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of persons with total income $25,000 and over",tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p7 <- renderPlotly({
    p7data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of persons with total income $35,000 and over",tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p8 <- renderPlotly({
    p8data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of persons with total income $50,000 and over",tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p9 <- renderPlotly({
    p9data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of persons with total income $75,000 and over",tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p10 <- renderPlotly({
    p10data() %>% plot_ly(x=~ref_date,y=~Number,type="scatter",mode="lines+markers",text=~value) %>%
      layout(title = "",autosize = F, width = 430, height = 158,
             xaxis = list(showgrid = FALSE,title = "Number of persons with total income $100,000 and over",tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p11 <- renderPlotly({
   p11data() %>% select(c(ref_date,DON,Number)) %>% spread(key=DON,value=Number) %>%  rename(female    =`Percentage of taxfilers, female`,married=`Percentage of taxfilers, married`) %>% plot_ly(x =     ~ref_date, y = ~female, name = 'Female Taxfilers', type = 'scatter'     ,mode ='lines+markers')    %>% add_trace(y=~married,name = 'Married',      mode = 'lines+markers')  %>%
   layout(title = "",autosize = F, width = 430, height = 158,
   xaxis = list(showgrid = FALSE,title = "Number of taxfilers, married & Female"
   ,tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })  
  output$p12 <- renderPlotly({
    p12data() %>% select(c(ref_date,DON,value)) %>% spread(key=DON,value=value) %>%  rename          (taxfilers=`Average age of taxfilers (years)`,persons=`Average age of persons (years)`) %>%        plot_ly(x =~ref_date, y = ~persons, name = 'Persons', type = 'scatter',mode ='lines+markers') %>%   add_trace(y=~taxfilers,name = 'Taxfilers',mode = 'lines+markers')  %>%
    layout(title = "",autosize = F, width = 430, height = 158,
          xaxis = list(showgrid = FALSE,title = "Average Age of taxfilers & Persons"
          ,tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p13 <- renderPlotly({
    p13data() %>% select(c(ref_date,DON,value)) %>% spread(key=DON,value=value) %>%  rename          (males=`Median total income, males (dollars)`,females=`Median total income, females (dollars)`,combined=`Median total income, both sexes (dollars)`) %>% plot_ly(x =~ref_date, y = ~males, name = 'Males', type = 'scatter',mode ='lines+markers') %>% add_trace(y=~females,name = 'Females',mode = 'lines+markers')  %>% add_trace(y=~combined,name = 'Both Sexes',mode = 'lines+markers') %>%
  layout(title = "",autosize = F, width = 430, height = 158,
   xaxis = list(showgrid = FALSE,title = "Median total income(dollars)"
  ,tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  })
  output$p14 <- renderPlotly({  
  p14data() %>% select(c(ref_date,DON,value)) %>% spread(key=DON,value=value) %>%  rename          (males=`Median employment income, males (dollars)`,females=`Median employment income, females (dollars)`,combined=`Median employment income, both sexes (dollars)`) %>% plot_ly(x =~ref_date, y = ~males, name = 'Males', type = 'scatter',mode ='lines+markers') %>% add_trace(y=~females,name = 'Females',mode = 'lines+markers')  %>% add_trace(y=~combined,name = 'Both Sexes',mode = 'lines+markers') %>%
    layout(title = "",autosize = F, width = 430, height = 158,
    xaxis = list(showgrid = FALSE,title = "Median employment income(dollars)"
    ,tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
}) 
  # output$p15 <- renderPlotly({  
  #   p15data() %>% select(c(ref_date,DON,value)) %>% spread(key=DON,value=value) %>%  rename          (canadian=`Canadian index of median total income`,provincial=`Provincial index of median total income`) %>% plot_ly(x =~ref_date, y = ~canadian, name = 'Canadian', type = 'scatter',mode ='lines+markers') %>% add_trace(y=~provincial,name = 'Provincial',mode = 'lines+markers') %>%
  #     layout(title = "",autosize = F, width = 430, height = 158,
  #    xaxis = list(showgrid = FALSE,title = "Index of median total income"
  #   ,tickfont=axisfont,titlefont=titlefont),yaxis = list (title = "",tickfont=axisfont))
  # })  
  output$bars <- renderPlotly({
    taxdata <- tax()
    if(input$Pr=="ALL"){
    taxdata$Geo <- gsub(" ","_",taxdata$Geo)
    taxdata <- taxdata %>% filter(DON=="Number of taxfilers" & (!grepl(',', Geo) & (!grepl('Canada', Geo))))     %>% select(c(ref_date,Geo,value))      
    taxdata<- taxdata %>% group_by(ref_date) %>% mutate(per=round(100*value/sum(value),2)) %>% 
    select(-c(value)) %>% spread(key=Geo,value=per)
    l <- list(orientation = 'h',font = list(family = "sans-serif",size = 8,color = "#000")
              ,bordercolor = "#FFFFFF",borderwidth = 2,X=-5,xanchor="left")
    plot_ly(taxdata, y = ~ref_date, x = ~Ontario, name = 'ON', type = 'bar',marker = list(color = '#F5FF8D'), orientation = 'h') %>% add_trace(x = ~British_Columbia, name = 'BC',marker = list(color = '#FF5733')) %>%
      add_trace(x = ~Manitoba, name = 'MN', marker = list(color = '#B8FF33')) %>%
      add_trace(x = ~Nova_Scotia, name = 'NS',marker = list(color = '#33FFAF')) %>%
      add_trace(x = ~New_Brunswick, name = 'NB',marker = list(color = '#33FFFC')) %>%
      add_trace(x = ~Newfoundland_and_Labrador, name = 'NL',marker = list(color = '#338DFF')) %>%
      add_trace(x = ~Nunavut, name = 'NV',marker = list(color = '#FF33FF')) %>%
      add_trace(x = ~Prince_Edward_Island, name = 'PE',marker = list(color = '#FF3371')) %>%
      add_trace(x = ~Quebec, name = 'QC',marker = list(color = '#DAF7A6')) %>%
      add_trace(x = ~Saskatchewan, name = 'SK',marker = list(color = '#C70039')) %>%
      add_trace(x = ~Yukon, name = 'YK',marker = list(color = '#312F44')) %>%
      layout(title = '% Taxfilers across Provinces', barmode = 'stack',titlefont=titlefont1,
             xaxis = list(title = "",showgrid = FALSE),legend = l,
             yaxis = list(title = "",autotick = F,showgrid = FALSE))
    }else{
      b<- tax() %>% filter((DON=="Number of taxfilers"|DON=="Number of persons") & ((str_count(Geo, ",")==1) & (grepl(input$Pr, Geo)))) %>% select(c(ref_date,DON,Geo,value))%>% separate(Geo,c("City", "Province"), ",") %>% select(-c(Province)) %>% filter(DON=="Number of taxfilers") %>% complete(City,ref_date) %>% group_by(City, ref_date) %>% summarise(value = sum(value, na.rm = TRUE)) %>% filter(ref_date==2015)
   p<-plot_ly() %>% add_trace(data=b,y=~City,x=~value,name="Taxfilers",type='bar'
   ,orientation='h')
   b<- tax() %>% filter((DON=="Number of taxfilers"|DON=="Number of persons") & ((str_count(Geo, ",")==1) & (grepl(input$Pr, Geo)))) %>% select(c(ref_date,DON,Geo,value))%>% separate(Geo,c("City", "Province"), ",") %>% select(-c(Province)) %>% filter(DON=="Number of persons") %>% complete(City,ref_date) %>% group_by(City, ref_date) %>% summarise(value = sum(value, na.rm = TRUE)) %>% filter(ref_date==2015)
   p %>% add_trace(data=b,y=~City,x=~value,name="Persons",type='bar',orientation='h') %>% layout(title = 'Number of taxfilers and persons',titlefont=titlefont1, barmode = 'stack',xaxis = list(title = "",showgrid = FALSE),
yaxis = list(title = "",tickfont=axisfont,autotick = F,showgrid = FALSE))
   }
  })
  output$mymap <- renderLeaflet({
    newfile <- province()
    colors = c("blue","red","orange","green","yellow","lime")
    labels = c("Conservative","Liberal","NDP","Green Party","Independent","BlocQ")
    popup = paste("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                  "<img src = ", newfile$ImageSource, " width='80%' height='60%'>","<br/>","<font size=2 color=red>",paste(newfile$Name,",",newfile$Party),"<br/>",sep="")
    newfile %>% leaflet() %>%setView(lng=mean(newfile$lng),lat=mean(newfile$lat),zoom=3) %>% addTiles() %>% 
      addCircleMarkers(lng=~lng,lat=~lat,popup = popup, color = ~color,radius=4,label=~paste(Name,Constituency,"CLICK ME!",sep=",")) %>% addLegend("topright",colors=colors,labels=labels,opacity=0.5)
  })  
  
  }
)