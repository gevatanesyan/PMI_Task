library(shiny)
library(RMySQL)
library(memoise)
library(e1071)
library(DT)

function(input, output, session) {
  
  db_user <- 'root'
  db_password <- '12345678'
  db_name <- 'pmi'
  db_host <- '127.0.0.1'
  db_port <- 3306
  
  
  mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                     dbname = db_name, host = db_host, port = db_port)
  
  
  
  querry <- "SELECT
    SOH.TotalDue AS SalesAmount,
    CAST(ExtractValue(P.Demographics, '/*/TotalPurchaseYTD') AS DECIMAL(10, 2)) AS TotalPurchaseYTD,
    PC.Name AS ProductCategoryName,
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
          
          
          
          ###### Trends
          
          trends_querry <- "SELECT ST.Name as Territory,
                            PC.Name as Category,
                            YEAR(SOH.OrderDate) AS Year,
                            MONTH(SOH.OrderDate) AS Month,
                            SUM(SOD.LineTotal) AS Total
                            FROM Sales_SalesOrderHeader AS SOH
                            JOIN Sales_SalesOrderDetail AS SOD ON SOH.SalesOrderID = SOD.SalesOrderID
                            JOIN Production_Product as P on SOD.ProductID = P.ProductID
                            JOIN Sales_SalesTerritory as ST on SOH.TerritoryID = ST.TerritoryID
                            JOIN Production_ProductSubcategory as PS on PS.ProductSubcategoryID = P.ProductSubcategoryID
                            JOIN Production_ProductCategory as PC on PC.ProductCategoryID = PS.ProductCategoryID
                            group by ST.NAME, PC.NAME, YEAR(SOH.OrderDate), MONTH(SOH.OrderDate);"
          
          df2 <- reactive(fetch(dbSendQuery(mydb, trends_querry), rs, n = -1))
          
          
          output$territory <- renderUI({
            checkboxGroupInput(inputId = 'var1',
                               label = 'Select Territories',
                               choices = unique(df2()[['Territory']]),
                               selected = "All")
          })
          
          output$category <- renderUI({
            checkboxGroupInput(inputId = 'var2',
                               label = 'Select Categories',
                               choices = unique(df2()[['Category']]),
                               selected = "All")
          })
          
          output$year <- renderUI({
            checkboxGroupInput(inputId = 'var3',
                               label = 'Select Years',
                               choices = sort(unique(as.character(df2()[['Year']]))))
          })
          
          
          
          filtered_data <- reactive({
            
            claim <- df2() 
            

            if (!is.null(input$var1) && !identical(input$var1, "All")) {
              claim <- claim %>% filter(Territory %in% input$var1)
            }
            
            
            if (!is.null(input$var2) && !identical(input$var2, "All")) {
              claim <- claim %>% filter(Category %in% input$var2)
            }
          
            if (!is.null(input$var3) && !identical(input$var3, "All")) {
              selected_years <- as.numeric(input$var3) 
              claim <- claim %>% filter(Year %in% selected_years)
            }
            
           
            claim
          })
          
          

          
          
          output$contents <- DT::renderDataTable({
            filtered_data()
          }, options = list(aLengthMenu = c(20, 40, 60), scrollX = TRUE, scrollY = 400))
          

          
          output$plot2 <- renderPlot({
            df3 <- filtered_data()
            df3$Date <- with(df3, as.Date(paste(Year, Month, "01", sep = "-")))
            
            ggplot(df3, aes(x = Date, y = Total, color = Category)) +
              geom_line() +  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + theme_minimal() +
              labs(title = "Sales Trends Over Time",
                   x = "Date",
                   y = "Total Sales",
                   color = "Category")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            
            
          })
          
          output$plot2 <- renderPlot({
            df3 <- filtered_data()
            df3$Date <- with(df3, as.Date(paste(Year, Month, "01", sep = "-")))
            
            ggplot(df3, aes(x = Date, y = Total, color = Category)) +
              geom_line() +  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + theme_minimal() +
              labs(title = "Sales Trends Over Time",
                   x = "Date",
                   y = "Total Sales",
                   color = "Category")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            
            
          })
          
       
          sales_by_category <- reactive({
            req(df2())
            df2() %>%
              group_by(Category) %>%
              summarise(Total = sum(Total, na.rm = TRUE)) %>%
              mutate(Percentage = Total / sum(Total) * 100)
          })
          
          sales_by_territory <- reactive({
            req(df2())  
            df2() %>%
              group_by(Territory) %>%
              summarise(Total = sum(Total, na.rm = TRUE)) %>%
              mutate(Percentage = Total / sum(Total) * 100)
          })
          
          output$plotCategoryPie <- renderPlot({
            df <- sales_by_category()
            df <- df %>% 
              arrange(desc(Category)) %>%
              mutate(label_position = cumsum(Percentage) - 0.5 * Percentage)
            
            ggplot(df, aes(x = "", y = Percentage, fill = Category)) +
              geom_bar(width = 1, stat = "identity") +
              coord_polar("y", start = 0) +
              geom_text(aes(y = label_position, label = paste0(round(Percentage, 1), "%")), color = "white") +
              theme_void() +
              labs(fill = "Category", title = "Sales Percentage by Product Category")
          })
          
          output$plotTerritoryPie <- renderPlot({
            df <- sales_by_territory()  
            df <- df %>% 
              arrange(desc(Territory)) %>%
              mutate(label_position = cumsum(Percentage) - 0.5 * Percentage)
            
            ggplot(df, aes(x = "", y = Percentage, fill = Territory)) +
              geom_bar(width = 1, stat = "identity") +
              coord_polar("y", start = 0) +
              geom_text(aes(y = label_position, label = paste0(round(Percentage, 1), "%")), color = "white") +
              theme_void() +
              labs(fill = "Territory", title = "Sales Percentage by Territory")
          })
          
          
          
          
          output$downloadPDF <- downloadHandler(
            filename = function() {
              paste("sales-plots-", Sys.Date(), ".pdf", sep="")
            },
            content = function(file) {

              pdf(file, width = 8, height = 4)

              df_for_plot1 <- filtered_data() %>%
                mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))  # Create Date column
              

              plot1 <- ggplot(df_for_plot1, aes(x = Date, y = Total, group = Category, color = Category)) +
                geom_line() +
                labs(title = "Sales Trends Over Time", x = "Date", y = "Total Sales") +
                theme_minimal()
              print(plot1)

              df_for_plot2 <- sales_by_category()  # Assuming this returns aggregated and percentage calculated data
              plot2 <- ggplot(df_for_plot2, aes(x = "", y = Percentage, fill = Category)) +
                geom_bar(width = 1, stat = "identity") +
                coord_polar("y", start = 0) +
                geom_text(aes(label = paste0(round(Percentage, 1), "%"), y = cumsum(Percentage) - (0.5 * Percentage)), color = "black") +
                labs(title = "Sales Percentage by Product Category") +
                theme_void()
              print(plot2)
              
              # Prepare data for the pie chart for sales by territory
              df_for_plot3 <- sales_by_territory()  # Assuming this returns aggregated and percentage calculated data
              plot3 <- ggplot(df_for_plot3, aes(x = "", y = Percentage, fill = Territory)) +
                geom_bar(width = 1, stat = "identity") +
                coord_polar("y", start = 0) +
                geom_text(aes(label = paste0(round(Percentage, 1), "%"), y = cumsum(Percentage) - (0.5 * Percentage)), color = "black") +
                labs(title = "Sales Percentage by Territory") +
                theme_void()
              print(plot3)
              
              dev.off()
            }
          )
          
          
          
          
          

}

