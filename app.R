library(shiny)
library(DT)
library(openxlsx)
library(plotly)
library(ggplot2)
library(shinyalert)

# Sample inventory data frame
inventory <- data.frame(
  Location = character(0),
  Item = character(0),
  Quantity = numeric(0),
  Description = character(0),
  Type = character(0),
  Subcategory = character(0),
  Date = character(0),
  stringsAsFactors = FALSE
)

# Stock log to track stock in and out actions
stock_log <- data.frame(
  Action = character(0),
  Item = character(0),
  Quantity = numeric(0),
  Person = character(0),
  Email = character(0),
  Phone = character(0),
  Date = character(0),
  Location = character(0),
  stringsAsFactors = FALSE
)

# Alert thresholds
default_thresholds <- data.frame(
  Item = character(0),
  Threshold = numeric(0),
  stringsAsFactors = FALSE
)

threshold_data <- reactiveVal(default_thresholds)

# Define UI
ui <- fluidPage(
  useShinyalert(),  # Include shinyalert
  titlePanel("Advanced Inventory Management System"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("new_location", "Add New Location"),
      actionButton("save_location", "Save Location"),
      selectInput("location", "Select Location", choices = NULL),
      textInput("item_name", "Item Name"),
      numericInput("quantity", "Quantity", value = 0, min = 0),
      textInput("description", "Description"),
      textInput("type", "Type"),
      textInput("subcategory", "Subcategory"),
      dateInput("date", "Date", value = Sys.Date()),
      textInput("person", "Person Responsible"),
      textInput("email", "Email Address"),
      textInput("phone", "Phone Number"),
      numericInput("stock_quantity", "Stock Quantity", value = 0, min = 0),
      numericInput("threshold", "Set Alert Threshold", value = 0, min = 0),
      actionButton("add_item", "Add Item"),
      actionButton("update_item", "Update Item"),
      actionButton("stock_in", "Add Stock In"),
      actionButton("stock_out", "Add Stock Out"),
      actionButton("remove_item", "Remove Selected Item"),
      actionButton("save_inventory", "Save Complete Inventory"),
      downloadButton("download_report", "Download Inventory Report"),
      actionButton("generate_weekly_report", "Generate Weekly Report"),
      br(),
      downloadButton("download_item_report", "Download Low Stock Item Report"),
      DTOutput("inventory_table")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Inventory Overview",
                 selectInput("filter_location", "Filter by Location", choices = NULL, selected = NULL),
                 DTOutput("inventory_display")),
        tabPanel("Stock Log", DTOutput("stock_log_display")),
        tabPanel("Item History", DTOutput("item_history_display")),
        tabPanel("Dashboard", 
                 selectInput("dashboard_location", "Select Location for Dashboard", choices = NULL, selected = NULL),
                 plotlyOutput("dashboard_plot"),
                 DTOutput("dashboard_table"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  inventory_data <- reactiveVal(inventory)
  stock_log_data <- reactiveVal(stock_log)
  locations_list <- reactiveVal(character(0))
  
  observe({
    updateSelectInput(session, "location", choices = locations_list())
    updateSelectInput(session, "filter_location", choices = c("All", locations_list()))
    updateSelectInput(session, "dashboard_location", choices = c("All", locations_list()))
  })
  
  observeEvent(input$save_location, {
    new_location <- input$new_location
    if (new_location != "") {
      locations_list(c(locations_list(), new_location))
      updateTextInput(session, "new_location", value = "")
      showNotification(paste("Location", new_location, "added."), type = "message")
    } else {
      showNotification("Enter a valid location.", type = "error")
    }
  })
  
  observeEvent(input$add_item, {
    new_item <- data.frame(
      Location = input$location,
      Item = input$item_name,
      Quantity = input$quantity,
      Description = input$description,
      Type = input$type,
      Subcategory = input$subcategory,
      Date = as.character(input$date),
      stringsAsFactors = FALSE
    )
    updated_inventory <- inventory_data()
    
    # Add new item
    updated_inventory <- rbind(updated_inventory, new_item)
    inventory_data(updated_inventory)
    
    # Update thresholds
    new_threshold <- data.frame(
      Item = input$item_name,
      Threshold = input$threshold,
      stringsAsFactors = FALSE
    )
    threshold_data(rbind(threshold_data(), new_threshold))
    
    showNotification("Item added.", type = "message")
  })
  
  observeEvent(input$update_item, {
    updated_inventory <- inventory_data()
    index <- which(updated_inventory$Item == input$item_name & updated_inventory$Location == input$location)
    
    if (length(index) > 0) {
      updated_inventory[index, ] <- list(
        Location = input$location,
        Item = input$item_name,
        Quantity = input$quantity,
        Description = input$description,
        Type = input$type,
        Subcategory = input$subcategory,
        Date = as.character(input$date)
      )
      inventory_data(updated_inventory)
      
      # Update threshold
      thresholds <- threshold_data()
      thresholds[thresholds$Item == input$item_name, "Threshold"] <- input$threshold
      threshold_data(thresholds)
      
      showNotification("Item updated.", type = "message")
    } else {
      showNotification("Item not found.", type = "error")
    }
  })
  
  observeEvent(input$stock_in, {
    updated_inventory <- inventory_data()
    index <- which(updated_inventory$Item == input$item_name & updated_inventory$Location == input$location)
    
    if (length(index) > 0) {
      quantity_to_add <- input$stock_quantity
      updated_inventory[index, "Quantity"] <- updated_inventory[index, "Quantity"] + quantity_to_add
      inventory_data(updated_inventory)
      
      # Log stock in action
      log_entry <- data.frame(
        Action = "Stock In",
        Item = input$item_name,
        Quantity = quantity_to_add,
        Person = input$person,
        Email = input$email,
        Phone = input$phone,
        Date = Sys.Date(),
        Location = input$location,
        stringsAsFactors = FALSE
      )
      stock_log_data(rbind(stock_log_data(), log_entry))
      showNotification(paste(quantity_to_add, "units added to", input$item_name), type = "message")
    } else {
      showNotification("Item not found in inventory.", type = "error")
    }
  })
  
  observeEvent(input$stock_out, {
    updated_inventory <- inventory_data()
    index <- which(updated_inventory$Item == input$item_name & updated_inventory$Location == input$location)
    
    if (length(index) > 0) {
      quantity_to_remove <- input$stock_quantity
      if (updated_inventory[index, "Quantity"] >= quantity_to_remove) {
        updated_inventory[index, "Quantity"] <- updated_inventory[index, "Quantity"] - quantity_to_remove
        
        # Log stock out action
        log_entry <- data.frame(
          Action = "Stock Out",
          Item = input$item_name,
          Quantity = quantity_to_remove,
          Person = input$person,
          Email = input$email,
          Phone = input$phone,
          Date = Sys.Date(),
          Location = input$location,
          stringsAsFactors = FALSE
        )
        stock_log_data(rbind(stock_log_data(), log_entry))
        
        # Check threshold
        thresholds <- threshold_data()
        threshold_row <- thresholds[thresholds$Item == input$item_name, ]
        if (nrow(threshold_row) > 0 && updated_inventory[index, "Quantity"] <= threshold_row$Threshold) {
          shinyalert(
            title = "Low Stock Alert!",
            text = paste("Item:", input$item_name, "is below the threshold at location", input$location),
            type = "warning"
          )
          
          # Generate a report for the individual item below threshold
          item_report <- updated_inventory[index, ]
          report_file <- tempfile(fileext = ".xlsx")
          write.xlsx(item_report, report_file)
          showNotification("Item report generated for low stock item.", type = "message")
          
          # Make the item report available for download
          output$download_item_report <- downloadHandler(
            filename = function() { paste("low_stock_report_", input$item_name, "_", Sys.Date(), ".xlsx", sep = "") },
            content = function(file) {
              write.xlsx(item_report, file)
            }
          )
        }
        
        inventory_data(updated_inventory)
        showNotification(paste(quantity_to_remove, "units removed from", input$item_name), type = "message")
      } else {
        showNotification("Not enough stock to remove.", type = "error")
      }
    } else {
      showNotification("Item not found in inventory.", type = "error")
    }
  })
  
  observeEvent(input$remove_item, {
    selected <- input$inventory_display_rows_selected
    if (!is.null(selected)) {
      inventory <- inventory_data()
      inventory <- inventory[-selected, ]
      inventory_data(inventory)
      showNotification("Selected item removed.", type = "message")
    } else {
      showNotification("No item selected for removal.", type = "error")
    }
  })
  
  observeEvent(input$generate_weekly_report, {
    weekly_report <- stock_log_data()
    weekly_report <- weekly_report[as.Date(weekly_report$Date) >= Sys.Date() - 7, ]
    report_file <- tempfile(fileext = ".xlsx")
    write.xlsx(weekly_report, report_file)
    showNotification("Weekly report generated.", type = "message")
    shinyalert(
      title = "Report Generated",
      text = "Weekly inventory report has been generated and saved as an Excel file.",
      type = "success"
    )
  })
  
  output$inventory_display <- renderDT({
    filtered_inventory <- inventory_data()
    
    # Filter by selected location
    if (input$filter_location != "All" && !is.null(input$filter_location)) {
      filtered_inventory <- filtered_inventory[filtered_inventory$Location == input$filter_location, ]
    }
    
    datatable(
      filtered_inventory,
      editable = TRUE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    ) %>% 
      formatStyle(
        'Quantity',
        backgroundColor = styleInterval(0, c("red", "white"))
      )
  })
  
  proxy <- dataTableProxy("inventory_display")
  
  observeEvent(input$inventory_display_cell_edit, {
    info <- input$inventory_display_cell_edit
    inventory <- inventory_data()
    inventory[info$row, info$col + 1] <- info$value
    inventory_data(inventory)
  })
  
  output$stock_log_display <- renderDT({
    datatable(stock_log_data())
  })
  
  output$item_history_display <- renderDT({
    if (input$item_name == "") return(NULL)
    item_logs <- stock_log_data()[stock_log_data()$Item == input$item_name, ]
    datatable(item_logs)
  })
  
  output$dashboard_plot <- renderPlotly({
    inventory_summary <- inventory_data()
    
    # Filter by selected location for dashboard plot
    if (input$dashboard_location != "All") {
      inventory_summary <- inventory_summary[inventory_summary$Location == input$dashboard_location, ]
    }
    
    ggplot(inventory_summary, aes(x = Location, y = Quantity)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      theme_minimal() +
      labs(title = "Inventory Overview", x = "Location", y = "Quantity")
  })
  
  output$dashboard_table <- renderDT({
    inventory_data()
  })
}

shinyApp(ui, server)
