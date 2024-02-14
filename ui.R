library(shiny)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(shinydashboard)
library(shinyalert)
library(shinyBS)
library(DT)
library(tidyr)




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
               
               tabPanel("Product Category",

                        
                        

                          box(width = 3,
                              uiOutput("eda_vars2")),
                        
                          box(
                          width = 8,
                          height = 10,
                          plotOutput("bar1"))
                        
               ),
               
               
        )
        
      )
        
        
        
        
        
        
    
   
        ),
    
    
    
    tabItem(
      tabName = 'trends',
      
      mainPanel(
        fluidRow(
          column(width = 4, height = 300,
                 uiOutput("category"),
                 uiOutput("territory"),
                 uiOutput("year")
          ),
          column(width = 8, height = 300,
                 dataTableOutput("contents")
          )
        ),
        fluidRow(
          column(width = 12,
                 plotOutput("plot_line", height = "300px")
          )
        ),
        tags$br(),
        fluidRow(
          column(width = 6, plotOutput("plotCategoryPie", height = "300px")),
          column(width = 6, plotOutput("plotTerritoryPie", height = "300px"))
        ),
        tags$br(),
        downloadButton("downloadPDF", "Download Plots as PDF"),
        tags$br()
      ),
      
    ),
    
    
    tabItem(
      tabName = 'inventory',
      
      mainPanel(
        fluidRow(

          uiOutput("product_name"),
          column(width = 12, plotOutput("stackedFrequencyPlot", height = "600px")),
          column(width = 12, plotOutput("plot_inventory_scatter", height = "600px")),
          column(width = 12, plotOutput("plot_inventory0", height = "600px")),
 
          
          ),
          

        
        
      )
      
    ),
    
    tabItem(
      tabName = 'performance',
      mainPanel(
        fluidRow(
          column(width = 12, plotOutput("leadTimeSalesPlot", height = "600px", width = "100%")),
          column(width = 12, plotOutput("rejectionSalesPlot", height = "600px",  width = "100%")),
          column(width = 6, plotOutput("topVendorsPlot", height = "600px",  width = "100%")),
          column(width = 6, plotOutput("lastVendorsPlot", height = "600px",  width = "100%")),
          downloadButton("downloadReport", "Download Plots as PDF")
          
        ),
        
        
        
        
      )
      
    ),
    
    
        tabItem(
      tabName = 'model',
      fluidRow(
        tabBox(id="tabchart2",width = 12,height = 1200,
               
               
               tabPanel('Linear Regression',
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            
                            uiOutput("dependent_vars4"),
                            uiOutput("independet_vars4"),
                            actionButton("selectall2","Select All") ,
                            
                            tags$hr(),
                            
                            sliderInput("split2", label = "Please Select Train Size",min = 0.5, max = 0.95, value = 0.75, step = 0.05),
                            actionButton(inputId = 'modeling_linear', label = 'Create Model'),

                            
                            
                          ),
                          mainPanel(
                            fluidRow(
                              infoBoxOutput("RMSELinear", width = 6),
                              div(id = 'clickAUC', 
                                  infoBoxOutput("Rsquare", width = 6)),
                              box(plotOutput("residualplot")),
                              box(plotOutput("qqplot1")),
                              box(plotOutput("coef_plot_linear"), width = 12)
                              
                            ),
                            fluidRow(
                              div(id = 'clicklinearIntercept',
                                  infoBoxOutput("LinearIntercept")),
                              downloadButton("download2", "Download .csv"),
                              uiOutput("campains_linear"),
                              uiOutput("models_linear"),
                              textOutput('modelid_linear'),
                              useShinyalert(force = TRUE),
                              
                            )
                            
                          ),
                          
                          
                          
                        )
                ),
               
               
          ))
      
        )
    
    
    
    
    
     
    )
    
    
))

