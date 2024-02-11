library(shiny)
library(RMySQL)
library(memoise)
library(e1071)


function(input, output, session) {
  
  db_user <- 'root'
  db_password <- '12345678'
  db_name <- 'pmi'
  db_host <- '127.0.0.1'
  db_port <- 3306
  
  
  
  mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                     dbname = db_name, host = db_host, port = db_port)
  
  
  
  querry <- "SELECT 
    SOH.SalesOrderID,
    SOH.TotalDue AS SalesAmount,
    C.CustomerID,
    P.Title,
    P.EmailPromotion AS CustomerDemographics,
    PC.Name AS ProductCategoryName,
    PR.Name AS ProductName,
    PR.ListPrice AS ProductPrice
FROM Sales_SalesOrderHeader AS SOH
JOIN Sales_SalesOrderDetail AS SOD ON SOH.SalesOrderID = SOD.SalesOrderID
JOIN Sales_Customer AS C ON SOH.CustomerID = C.CustomerID
JOIN Person_Person AS P ON C.PersonID = P.BusinessEntityID
JOIN Production_Product AS PR ON SOD.ProductID = PR.ProductID
LEFT JOIN Production_ProductSubcategory AS PS ON PR.ProductSubcategoryID = PS.ProductSubcategoryID
LEFT JOIN Production_ProductCategory AS PC ON PS.ProductCategoryID = PC.ProductCategoryID;
"
  df <- fetch(dbSendQuery(mydb, querry), rs, n = -1)
  
  
  output$eda_vars <- renderUI({
    selectInput(inputId = 'var',
                label = 'Please select  Variable',
                choices = colnames(df)[sapply(df, is.numeric)])
  })
  
          ###### Sales Amounts and Product Price
          
          output$mean1 <- renderInfoBox(
            infoBox(
              "Mean", 
              paste0(round(mean(df$SalesAmount, na.rm = TRUE))),
              icon = icon("x"), 
              color = 'red', 
              width = 3
            )
          )
          
          output$median1 <- renderInfoBox(
            infoBox(
              "Median", 
              paste0(round(median(df$SalesAmount, na.rm = TRUE))),
              icon = icon("manat-sign"), 
              color = 'green', 
              width = 3
            )
          )
          
          output$sd1 <- renderInfoBox(
            infoBox(
              "Standard Deviation", 
              paste0(round(sd(df$SalesAmount, na.rm = TRUE))),
              icon = icon("square-root-variable"), 
              color = 'orange', 
              width = 3
            )
          )
          
          output$variance1 <- renderInfoBox(
            infoBox(
              "Variance", 
              paste0(round(var(df$SalesAmount, na.rm = TRUE))),
              icon = icon("s"), 
              color = 'blue', 
              width = 3
            )
          )
          
          output$cv1 <- renderInfoBox(
            infoBox(
              "Coeefecient Variation", 
              paste0(round((sd(df$SalesAmount, na.rm = TRUE) / mean(df$SalesAmount, na.rm = TRUE)) * 100)),
              icon = icon("c"), 
              color = 'yellow', 
              width = 3
            )
          )
          
          output$skewness1 <- renderInfoBox(
            infoBox(
              "Skewness", 
              paste0(round(skewness(df$SalesAmount, na.rm = TRUE)),2),
              icon = icon("k"), 
              color = 'black', 
              width = 3
            )
          )
          
          output$Hist1 <- renderPlot({
            
            ggplot(data = df) + geom_histogram(aes_string(x = input$var), bins = input$bins)
            
          },height = 700)
          
          
          
          ###### Demograpics and Product Category
          
          output$eda_vars2 <- renderUI({
            selectInput(inputId = 'var2',
                        label = 'Please select  Variable',
                        choices = colnames(df)[sapply(df, is.character)])
          })
          
          
          output$bar1 <- renderPlot({
            
            ggplot(df, aes_string(x = input$var2)) + 
              geom_bar() +
              labs(x = "Category", y = "Frequency") +
              theme_minimal()
            
          },height = 700)
          
          
          
          
          
}
