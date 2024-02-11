library(shiny)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(shinydashboard)
library(shinyalert)
library(shinyBS)



ui <- shinydashboardPlus::dashboardPage(
  dashboardHeader(tags$li(a(href = 'https://www.pmiscience.com/',target="_blank",
                            img(src = 'https://www.pmiscience.com/content/dam/pmiscience/logos/pmi-science-logo-black.svg',
                                title = "Company Home", height = "35",width = '80',),
                            style = "padding-top:5px; padding-bottom:5px;"),
                          class = "dropdown")),
  
  dashboardSidebar(disable = F,
                   sidebarMenu(
                     menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
                     menuItem("Exploratory data Analysis", tabName = "eda"),
                     menuItem('Sales Trends',tabName = 'trends'),
                     menuItem("Inventory Management", tabName = "inventory"),
                     menuItem("Sales Performance", tabName = "performance"),
                     menuItem("Sales Prediction", tabName = "model")
                     
                     
                   )
  ),
  
  dashboardBody(   
    
    tabItems(
    
    tabItem(
      tabName = 'welcome',
      
      mainPanel(
        h1("Adventure Works Analysis"),
      )
      
    ),
    
    tabItem(
      tabName = 'eda',
      

      
      
      tags$hr(),
      fluidRow(
        
        tabBox(id="tabchart1",width = 12,height = 1000,
               tabPanel("Sales Amounts and Product Price",
                        
                        fluidRow(
                          div(id = 'clickdiv1',style = "height: 3; width: 3",
                              infoBoxOutput("mean1",  width = 4)),
                          div(id = 'clickdiv2',
                              infoBoxOutput("median1" ,width = 4)),
                          div(id = 'clickdiv3',
                              infoBoxOutput("sd1" ,width = 4)),
                          div(id = 'clickdiv4',
                              infoBoxOutput("variance1", width = 4)),
                          div(id = 'clickdiv4',
                              infoBoxOutput("cv1", width = 4)),
                          div(id = 'clickdiv4',
                              infoBoxOutput("skewness1", width = 4)),
                        ),
                        
                        box(width = 3,
                            title = "Inputs",
                            solidHeader = T,
                            status = 'primary',
                            uiOutput('eda_vars'),
                            sliderInput("bins",
                                        "Number of bins:",
                                        min = 1,
                                        max = 50,
                                        value = 30)),
                        
                        box(width = 8,
                            height = 10,
                            plotOutput("Hist1"))
                        
                        ),
               
               tabPanel("Demograpics and Product Category",

                        
                        

                          box(width = 3,
                              uiOutput("eda_vars2")),
                        
                          box(
                          width = 8,
                          height = 10,
                          plotOutput("bar1"))
                        
               ),
               
               
        )
        
      )
        
        
        
        
        
        
    
   
        )
     
      )
    )
  )
