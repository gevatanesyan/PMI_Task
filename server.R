library(shiny)
library(RMySQL)
library(memoise)
library(e1071)
library(DT)
library(forcats)
library(Metrics)
library(caret)
library(shinyjs)

function(input, output, session) {
  
  db_user <- Sys.getenv('DB_USER')
  db_password <- Sys.getenv('DB_PASSWORD')
  db_name <- Sys.getenv('DB_NAME')
  db_host <- Sys.getenv('DB_HOST', '127.0.0.1')  
  db_port <- as.integer(Sys.getenv('DB_PORT', '3306'))
  
  mydb <- dbConnect(MySQL(), user = db_user, password = db_password,
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
    selectInput(inputId = 'var_hist',
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
            req(input$var_hist)
            ggplot(data = df) + geom_histogram(aes_string(x = input$var_hist), bins = input$bins)
            
          },height = 700)
          
          
          
          ###### Demograpics and Product Category
          
          output$eda_vars2 <- renderUI({
            selectInput(inputId = 'var_product_category',
                        label = 'Please select  Variable',
                        choices = colnames(df)[sapply(df, is.character)])
          })
          
          
          output$bar1 <- renderPlot({
            req(input$var_product_category)
            ggplot(df, aes_string(x = input$var_product_category)) + 
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
            checkboxGroupInput(inputId = 'var_territory',
                               label = 'Select Territories',
                               choices = unique(df2()[['Territory']]),
                               selected = "All")
          })
          
          output$category <- renderUI({
            checkboxGroupInput(inputId = 'var_category',
                               label = 'Select Categories',
                               choices = unique(df2()[['Category']]),
                               selected = "All")
          })
          
          output$year <- renderUI({
            checkboxGroupInput(inputId = 'var_year',
                               label = 'Select Years',
                               choices = sort(unique(as.character(df2()[['Year']]))))
          })
          
          
          
          filtered_data <- reactive({
            
            claim <- df2() 
            

            if (!is.null(input$var_territory) && !identical(input$var_territory, "All")) {
              claim <- claim %>% filter(Territory %in% input$var_territory)
            }
            
            
            if (!is.null(input$var_category) && !identical(input$var_category, "All")) {
              claim <- claim %>% filter(Category %in% input$var_category)
            }
          
            if (!is.null(input$var_year) && !identical(input$var_year, "All")) {
              selected_years <- as.numeric(input$var_year) 
              claim <- claim %>% filter(Year %in% selected_years)
            }
            
           
            claim
          })
          
          

          
          output$contents <- DT::renderDataTable({
            filtered_data()
          }, options = list(aLengthMenu = c(20, 40, 60), scrollX = TRUE, scrollY = 400))
          


          
          output$plot_line <- renderPlot({
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
                mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))  
              

              plot1 <- ggplot(df_for_plot1, aes(x = Date, y = Total, group = Category, color = Category)) +
                geom_line() +
                labs(title = "Sales Trends Over Time", x = "Date", y = "Total Sales") +
                theme_minimal()
              print(plot1)

              df_for_plot2 <- sales_by_category() 
              plot2 <- ggplot(df_for_plot2, aes(x = "", y = Percentage, fill = Category)) +
                geom_bar(width = 1, stat = "identity") +
                coord_polar("y", start = 0) +
                geom_text(aes(label = paste0(round(Percentage, 1), "%"), y = cumsum(Percentage) - (0.5 * Percentage)), color = "black") +
                labs(title = "Sales Percentage by Product Category") +
                theme_void()
              print(plot2)
              

              df_for_plot3 <- sales_by_territory() 
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
          
          
          ###### Inventory
          
          querry_inventory <- "WITH AggregatedSales AS (
                              SELECT 
                                  SOD.ProductID,
                                  SUM(SOD.OrderQty) AS TotalUnitsSold
                              FROM Sales_SalesOrderDetail AS SOD
                              JOIN Sales_SalesOrderHeader AS SOH ON SOD.SalesOrderID = SOH.SalesOrderID
                              GROUP BY SOD.ProductID
                          )
                          SELECT 
                              P.ProductID,
                              P.Name AS ProductName,
                              PSC.ProductSubcategoryID,
                              PSC.Name AS ProductSubcategoryName,
                              PC.ProductCategoryID,
                              PC.Name AS ProductCategoryName,
                              L.LocationID,
                              L.Name AS LocationName,
                              PI.Quantity AS InventoryQuantity,
                              COALESCE(ASales.TotalUnitsSold, 0) AS TotalUnitsSold 
                          FROM Production_Product AS P
                          JOIN Production_ProductSubcategory AS PSC ON P.ProductSubcategoryID = PSC.ProductSubcategoryID
                          JOIN Production_ProductCategory AS PC ON PSC.ProductCategoryID = PC.ProductCategoryID
                          JOIN Production_ProductInventory AS PI ON P.ProductID = PI.ProductID
                          JOIN Production_Location AS L ON PI.LocationID = L.LocationID
                          LEFT JOIN AggregatedSales AS ASales ON P.ProductID = ASales.ProductID;"
          
          
          
          df_invenotry <- reactive(fetch(dbSendQuery(mydb, querry_inventory), rs, n = -1))
          

          output$plot_inventory_scatter <- renderPlot({
            df_invenotry_plot1 <- df_invenotry()
            ggplot(df_invenotry_plot1, aes(x = InventoryQuantity, y = TotalUnitsSold, color = ProductCategoryName)) +
              geom_point() +
              geom_smooth(method = "lm", se = F) +
              labs(title = "Inventory Levels vs. Sales Demand",
                   x = "Inventory Quantity",
                   y = "Total Units Sold")

          })
          
          output$plot_inventory0<- renderPlot({
            unsold_products <- df_invenotry()
            
            unsold_products_aggregated <- unsold_products %>%
              filter(TotalUnitsSold == 0 & InventoryQuantity > 0) %>%
              group_by(ProductName) %>%
              summarise(TotalInventory = sum(InventoryQuantity)) %>%
              ungroup()
            
            unsold_products_aggregated <- unsold_products_aggregated %>%
              mutate(ProductName = fct_reorder(ProductName, TotalInventory, .desc = TRUE))
            

            ggplot(unsold_products_aggregated, aes(x = ProductName, y = TotalInventory)) +
              geom_bar(stat = "identity") +
              labs(title = "Inventory with No Sales", x = "Product Name", y = "Products ") +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

            
          })
          
          
          output$product_name <- renderUI({
            selectInput(inputId = 'var_product_category3',
                        label = 'Please select Product Name',
                        choices = unique(unique(df_invenotry()['ProductName'])),
                        multiple = TRUE,
                        selected = "HL Mountain Frame - Black, 38")
          })
          
          
          output$stackedFrequencyPlot <- renderPlot({
            req(input$var_product_category3) 
            req(df_invenotry()) 
            
            filtered_data <- df_invenotry() %>%
              filter(ProductName %in% input$var_product_category3) %>%
              mutate(Total = InventoryQuantity + TotalUnitsSold,
                     InventoryProportion = InventoryQuantity / Total,
                     SalesProportion = TotalUnitsSold / Total) %>%
              select(ProductName, InventoryProportion, SalesProportion)
            

            long_data <- filtered_data %>%
              pivot_longer(cols = c("InventoryProportion", "SalesProportion"), 
                           names_to = "Category", values_to = "Value")
            
            
            ggplot(long_data, aes(x = ProductName, y = Value, fill = Category)) +
              geom_bar(stat = "identity", position = "fill") +
              labs(title = "Inventory and Sales Proportion per Product", 
                   x = "Product Name", 
                   y = "Proportion") +
              theme_minimal()
          })
          
          ###### Vendors
          
          querry_performance <- "SELECT 
                                V.Name AS VendorName,
                                PV.AverageLeadTime,
                                P.ProductID,
                                P.Name AS ProductName,
                                POD.OrderQty,
                                POD.ReceivedQty,
                                POD.RejectedQty,
                                POH.OrderDate,
                                POH.ShipDate,
                                POH.SubTotal,
                                POH.TotalDue
                            FROM Purchasing_Vendor AS V
                            JOIN Purchasing_ProductVendor AS PV ON V.BusinessEntityID = PV.BusinessEntityID
                            JOIN Production_Product AS P ON PV.ProductID = P.ProductID
                            JOIN Purchasing_PurchaseOrderDetail AS POD ON P.ProductID = POD.ProductID
                            JOIN Purchasing_PurchaseOrderHeader AS POH ON POD.PurchaseOrderID = POH.PurchaseOrderID;"
          
          df_performance <- reactive(fetch(dbSendQuery(mydb, querry_performance), rs, n = -1))
          
          
          output$leadTimeSalesPlot <- renderPlot({
            
            df_summary <- df_performance() %>%
              group_by(VendorName) %>%
              summarize(AverageLeadTime = mean(AverageLeadTime, na.rm = TRUE),
                        TotalSales = sum(TotalDue),
                        .groups = 'drop') 

            ggplot(df_summary, aes(x = AverageLeadTime, y = TotalSales)) +
              geom_jitter(width =2, height = 1) +
              geom_smooth(method = "lm", color = "blue", se = F) +
              labs(title = "Correlation between Average Lead Time and Total Sales",
                   x = "Average Lead Time (days)", y = "Total Sales")
          })
          
          
          
          output$rejectionSalesPlot <- renderPlot({
            req(df_performance())  
            
            df_summary <- df_performance() %>%
              group_by(VendorName) %>%
              summarize(Rejected = sum(RejectedQty),
                        Ordered = sum(OrderQty),
                        RejectionRate = Rejected / Ordered,
                        TotalSales = sum(TotalDue),
                        .groups = 'drop') 
            
            ggplot(df_summary, aes(x = RejectionRate, y = TotalSales)) +
              geom_point() +
              geom_smooth(method = "lm", color = "blue", se = F ) +
              labs(title = "Correlation between Rejection Rate and Total Sales",
                   x = "Rejection Rate", y = "Total Sales")
          })
          
          output$topVendorsPlot <- renderPlot({
            req(df_performance())  
            
            df_summary <- df_performance() %>%
              group_by(VendorName) %>%
              summarize(TotalSales = sum(TotalDue), .groups = 'drop') %>%
              arrange(desc(TotalSales)) %>%
              slice_head(n = 5) 
            
            ggplot(df_summary, aes(x = reorder(VendorName, TotalSales), y = TotalSales)) +
              geom_bar(stat = "identity", fill = "steelblue") +
              labs(title = "Top 5 Vendors by Total Sales", x = "Vendor", y = "Total Sales") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          })
          
          output$lastVendorsPlot <- renderPlot({
            req(df_performance()) 
            
            df_summary <- df_performance() %>%
              group_by(VendorName) %>%
              summarize(TotalSales = sum(TotalDue), .groups = 'drop') %>%
              arrange(TotalSales) %>%
              slice_head(n = 5)  
            
            ggplot(df_summary, aes(x = reorder(VendorName, TotalSales), y = TotalSales)) +
              geom_bar(stat = "identity", fill = "tomato") +
              labs(title = "Last 5 Vendors by Total Sales", x = "Vendor", y = "Total Sales") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          })
          
          
          output$downloadReport <- downloadHandler(
            filename = function() {
              paste("report-", Sys.Date(), ".pdf", sep="")
            },
            content = function(file) {
              pdf(file, width = 11, height = 8.5)
              
              req(df_performance()) 
              
              df_summary_lead_time <- df_performance() %>%
                group_by(VendorName) %>%
                summarize(AverageLeadTime = mean(AverageLeadTime, na.rm = TRUE),
                          TotalSales = sum(TotalDue),
                          .groups = 'drop') 
              p1 <- ggplot(df_summary_lead_time, aes(x = AverageLeadTime, y = TotalSales)) +
                geom_jitter(width = 0.2, height = 0) +
                geom_smooth(method = "lm", color = "blue", se = FALSE) +
                labs(title = "Correlation between Average Lead Time and Total Sales",
                     x = "Average Lead Time (days)", y = "Total Sales")
              print(p1)
              
              df_summary_rejection <- df_performance() %>%
                group_by(VendorName) %>%
                summarize(Rejected = sum(RejectedQty),
                          Ordered = sum(OrderQty),
                          RejectionRate = Rejected / Ordered,
                          TotalSales = sum(TotalDue),
                          .groups = 'drop') 
              p2 <- ggplot(df_summary_rejection, aes(x = RejectionRate, y = TotalSales)) +
                geom_point() +
                geom_smooth(method = "lm", color = "blue", se = FALSE) +
                labs(title = "Correlation between Rejection Rate and Total Sales",
                     x = "Rejection Rate", y = "Total Sales")
              print(p2)
              
              df_top_vendors <- df_performance() %>%
                group_by(VendorName) %>%
                summarize(TotalSales = sum(TotalDue), .groups = 'drop') %>%
                arrange(desc(TotalSales)) %>%
                slice_head(n = 5)
              p3 <- ggplot(df_top_vendors, aes(x = reorder(VendorName, TotalSales), y = TotalSales)) +
                geom_bar(stat = "identity", fill = "steelblue") +
                labs(title = "Top 5 Vendors by Total Sales", x = "Vendor", y = "Total Sales") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
              print(p3)
              
              df_last_vendors <- df_performance() %>%
                group_by(VendorName) %>%
                summarize(TotalSales = sum(TotalDue), .groups = 'drop') %>%
                arrange(TotalSales) %>%
                slice_head(n = 5)
              p4 <- ggplot(df_last_vendors, aes(x = reorder(VendorName, TotalSales), y = TotalSales)) +
                geom_bar(stat = "identity", fill = "tomato") +
                labs(title = "Last 5 Vendors by Total Sales", x = "Vendor", y = "Total Sales") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
              print(p4)
              
              dev.off()
            }
          )
          
          
          
          
          ##### Linear regression
          
          
          querry_model <- " SELECT       TaxAmt,
                                        TotalDue,
                                        Freight,
                                        OrderQty,
                                        UnitPrice, 
                                        LineTotal,
                                        Name, 
                                        MakeFlag,
                                        Color,
                                        SafetyStockLevel,
                                        ReorderPoint,
                                        StandardCost,
                                        Size,
                                        Weight,
                                        DaysToManufacture,
                                        Class,
                                        ProductModelID 
                        FROM Sales_SalesOrderHeader AS SOH
                        JOIN Sales_SalesOrderDetail AS SOD ON SOH.SalesOrderID = SOD.SalesOrderID
                        JOIN Sales_Customer AS C ON SOH.CustomerID = C.CustomerID
                        JOIN Production_Product AS P ON SOD.ProductID = P.ProductID
                        JOIN Person_Person AS PP ON C.PersonID = PP.BusinessEntityID LIMIT 25000;"
          
          
          data <- reactive(fetch(dbSendQuery(mydb, querry_model), rs, n = -1))
          model_results <- reactiveValues(coefs = NULL)
          
          output$independet_vars4 <- renderUI({
            
            checkboxGroupInput(inputId = 'dependent_vars4',
                               label = 'Please Select Independent Variables',
                               inline = T,
                               width = '90%',
                               choices = as.list(names(data())),
            )
            
            
          })
          
          
          
          output$dependent_vars4 <-renderUI({
            
            selectInput(inputId = "independet_vars4",
                        label = "Please Select Dependent Variable", 
                        choices = as.list(names(data() %>% purrr::keep(is.numeric))),
                        multiple = FALSE,)
          })
          
          
          
          
          observe(
            if(req(input$independet_vars4) == 0) return(NULL)
            else {
              updateCheckboxGroupInput(session, inputId = "dependent_vars4", inline = T, choices = as.list(names(data()))[as.list(names(data()))!= req(input$independet_vars4)] )
            })
          
          
          
          
          
          observe({
            if(input$selectall2 == 0) return(NULL) 
            else if (input$selectall2%%2 == 0)
            {
              updateCheckboxGroupInput(session,inputId = "dependent_vars4",inline = T, choices=as.list(names(data()))[as.list(names(data()))!= req(input$independet_vars4)] )
            }
            else
            {
              updateCheckboxGroupInput(session,inputId = "dependent_vars4",inline = T, choices=as.list(names(data()))[as.list(names(data()))!= req(input$independet_vars4)] , selected=as.list(names(data())))
            }
          })
          
          
          
          
          observeEvent(input$modeling_linear, {
            
            tryCatch({
              options(scipen=999)
              
              f<-data()
              
              
              set.seed(123)
              smp_size <- floor(input$split2 * nrow(f))
              
              
              set.seed(123)
              train_ind <- sample(seq_len(nrow(f)), size = smp_size)
              set.seed(123)
              train <- f[train_ind, ]
              test <- f[-train_ind, ]
              
              train <- na.omit(train)
              test <- na.omit(test)
              
              if(is.null(input$dependent_vars4)){
                form <- sprintf("%s~%s",paste0(input$independet_vars4), 1)
              }else{
                form <- sprintf("%s~%s",paste0(input$independet_vars4), paste0(input$dependent_vars4,collapse="+"))
              }
              
              print(form)
              
              linear <- lm(as.formula(form), data=train, na.action = na.omit)

              
              
              linear_coefs <- reactive({data.frame(linear$coefficients)})
              
              model_results$coefs <- coef(linear)
              
              predictions_train <- predict(linear, newdata = train)

              predictions_test <-  predict(linear, newdata = test)
              
              output$RMSELinear <- renderInfoBox({
                infoBox(
                  "RMSE Train/Test",
                  paste0("Train: ", round(RMSE(predictions_train, train[[input$independet_vars4]])), "\n"),
                  paste0("Test: ", round(RMSE(predictions_test, test[[input$independet_vars4]])), "\n"),
                  color = "orange", fill = TRUE
                )
              })
              
              
              
              
              output$Rsquare <- renderInfoBox({
                infoBox(
                  "Adjusted R square", paste0(summary(linear)$adj.r.squared),
                  color = "orange", fill = TRUE
                )
              })
              
              

              

              
              
              residuals <- residuals(linear)
              fitted_values <- fitted(linear)
              
              residuals_data <- data.frame(Fitted = fitted_values, Residuals = residuals)
              
              qq_data <- data.frame(Standardized_Residuals = residuals)
              
              residual_plot <- ggplot(residuals_data, aes(x = Fitted, y = Residuals)) +
                geom_point() +
                geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                xlab("Fitted Values") +
                ylab("Residuals") +
                ggtitle("Residual Plot")
              
              qq_plot <- ggplot(qq_data, aes(sample = Standardized_Residuals)) +
                geom_qq() +
                geom_qq_line(color = "red") +
                xlab("Theoretical Quantiles") +
                ylab("Standardized Residuals") +
                ggtitle("Normal Q-Q Plot")
              
              
              coefficients <- coef(linear)[-1]
              
              coefficients_data <- data.frame(
                Predictor = names(coefficients),
                Coefficient = coefficients,
                Sign = ifelse(coefficients >= 0, "Positive", "Negative")
              )
              
              coefficients_plot <- ggplot(coefficients_data, aes(x = Predictor, y = Coefficient, fill = Sign)) +
                geom_bar(stat = "identity", position = "identity", color = "black") +
                scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "red")) +
                xlab("Predictor") +
                ylab("Coefficient") +
                ggtitle("Coefficients Plot") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
              
              
              output$residualplot <- renderPlot({
                residual_plot
                
              })
              
              
              output$qqplot1 <- renderPlot({
                qq_plot
              })
              
              output$coef_plot_linear <- renderPlot({
                coefficients_plot
              })
              
              
              
              output$download2 <- downloadHandler(
                filename = function() {
                  paste('linear_coefs-', Sys.Date(), ".csv", sep="")
                },
                content = function(file) {
                  
                  write.csv(model_results$coefs,file)
                  
                  
                }
              )
              
            },
            warning = function(warn){
              showNotification(paste0(warn), type = 'warning')
            },
            error = function(err){
              showNotification(paste0(err), type = 'err')
            })
            


            
            
            
          })
          
          
          observeEvent(input$clicklinearIntercept, {
            showModal(modalDialog(
              easyClose = TRUE,
              title = "Coefficients summary",
              renderPrint({as.data.frame(model_results$coefs)}) 
            ))
          })
          
          output$LinearIntercept <- renderUI({
            actionButton("clicklinearIntercept", "Coefficients - Click for more Info", class = "btn btn-warning")
          })

          
          

}

