library(shiny)
library(shinydashboard)
source("global.R")
shinyUI(dashboardPage(
  dashboardHeader(title = "Tax Analysis Canada"),
  dashboardSidebar(
    
    selectInput("Pr","Select Province",choices = append(as.character(unique(unique(newfile$PrTr))),"ALL",0)),
    p(),
    actionLink("panel_1_tab1", "Taxfilers between 0 & 24 age",style = "color: white;"),
    actionLink("panel_1_tab2", "Taxfilers between 25 & 44 age",style = "color: white;"),
    actionLink("panel_1_tab3", "Taxfilers between 45 & 64 age",style = "color: white;"),
    actionLink("panel_1_tab4", "Taxfilers between 65+ age",style = "color: white;"),
    actionLink("panel_1_tab5", "Avg age of Taxfilers & Persons",style = "color: white;"),
    actionLink("panel_1_tab6", "Mean Total Income(M & F)",style = "color: white;"),
    actionLink("panel_1_tab7", "Mean Emp Income (M & F)",style = "color: white;"),
    actionLink("panel_2_tab1", "Persons with Income $15k+",style = "color: white;"),
    actionLink("panel_2_tab2", "Persons with Income $25k+",style = "color: white;"),
    actionLink("panel_2_tab3", "Persons with Income $35k+",style = "color: white;"),
    actionLink("panel_2_tab4", "Persons with Income $50k+",style = "color: white;"),
    actionLink("panel_2_tab5", "Persons with Income $75k+",style = "color: white;"),
    actionLink("panel_2_tab6", "Persons with Income $100k+",style = "color: white;"),
    actionLink("panel_2_tab7", "# of Taxfilers (F & Married)",style = "color: white;"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    uiOutput("tab")
    ),
  dashboardBody(
  fluidRow(
    column(width = 7,
           leafletOutput("mymap")
           # box
           # (
           #   div(style = "height:300px;background-color: white;"),
           #   title = "Box title1", width = NULL, status = "primary",
           #   leafletOutput("mymap")
           # )
          ),
    column(width = 5,
           plotlyOutput("bars")
          )
    ),
  fluidRow(
    column(width = 5,
           tabsetPanel(tags$head(tags$style(type='text/css',".nav-tabs {font-size: 10px} ")),
             selected = "Tab1",id="panel1",
             tabPanel("Tab1", plotlyOutput("p1", height="200px")),
             tabPanel("Tab2", plotlyOutput("p2", height="200px")),
             tabPanel("Tab3", plotlyOutput("p3", height="200px")),
             tabPanel("Tab4", plotlyOutput("p4", height="200px")),
             tabPanel("Tab5", plotlyOutput("p12", height="200px")),
             tabPanel("Tab6", plotlyOutput("p13", height="200px")),
             tabPanel("Tab7", plotlyOutput("p14", height="200px"))
           )
    ),
    column(width = 5,
           tabsetPanel(id="panel2",
             tabPanel("Tab1", plotlyOutput("p5", height="200px")),
             tabPanel("Tab2", plotlyOutput("p6", height="200px")),
             tabPanel("Tab3", plotlyOutput("p7", height="200px")),
             tabPanel("Tab4", plotlyOutput("p8", height="200px")),
             tabPanel("Tab5", plotlyOutput("p9", height="200px")),
             tabPanel("Tab6", plotlyOutput("p10",height="200px")),
             tabPanel("Tab7", plotlyOutput("p11", height="200px"))
           )
    ),
    #column(width = 2, offset = 0, style='padding-top:40px;',infoBoxOutput("dri"))
    column(width = 2, offset = 0, style='padding:0px;',
           fluidRow(column(12,valueBoxOutput("dri"),tags$style("#dri {width:200px;}"))),
           fluidRow(column(12,valueBoxOutput("dri1"),tags$style("#dri1 {width:200px;}")))
          )
    ))

)
)