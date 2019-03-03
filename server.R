### Script builds the server necessary for the complaint inspections app

library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(dplyr)
library(ggplot2)
library(DT)

open_building_complaints <- readRDS(file = "open_building_complaints.rds")

server <- function(input, output, session) {
  
  ### Creating interactive map starting in Chicago
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
      ) %>%
      setView(lng = -87.6298, lat = 41.8781, zoom = 11)
  })
  
  observe({
    
    colour_by <- input$colour
    dark_colours <- c("#1b9e77", "#7570b3", "#d95f02")
    colour_pal <- colorFactor(dark_colours, 
                              domain = open_building_complaints[[colour_by]])
    
    address_label <- paste(strong("Building Complaint Details"),
                           hr(),
                           "Address:", open_building_complaints$street_address,
                           br(),
                           "Zip Code:", open_building_complaints$zip_code,
                           br(),
                           "Community Area:", open_building_complaints$community_area,
                           br(),
                           "Date Created:", open_building_complaints$created_date,
                           br(),
                           "Days Open:", open_building_complaints$days_open,
                           br(),
                           "Model Prediction:", open_building_complaints$complaint_pred,
                           br(),
                           "Risk Percentile:", paste0(
                             round(open_building_complaints$risk_percentile, 2) * 100, "%"
                             )
                           )
    
    leafletProxy("map", data = open_building_complaints) %>%
      clearShapes() %>%
      addCircles(lng = ~longitude, lat = ~latitude, 
                 radius = 155,
                 stroke = FALSE, 
                 fillOpacity = 0.75, 
                 popup = address_label,
                 fillColor = ~colour_pal(open_building_complaints[[colour_by]])) %>%
      addLegend("bottomleft", 
                pal = colour_pal, 
                values = open_building_complaints[[colour_by]], 
                title = "Complaint is:",
                layerId = "colorLegend")
  })
  
  ### Creating Data Explorer
  
  observe({
    addresses <- open_building_complaints %>%
      filter(zip_code %in% input$zip_code) %>%
        `$`("street_address") %>%
        unique() %>%
        sort()

    selection <- isolate(input$address[input$address %in% addresses])
    
    updateSelectInput(session, "address", choices = addresses,
                      selected = selection)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    
    isolate({
      map <- leafletProxy("map")
      
      dist <- 0.009
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      
      map %>% 
        fitBounds(lng1 = lng - dist, 
                  lat1 = lat - dist, 
                  lng2 = lng + dist, 
                  lat2 = lat + dist)
    })
  })
  
  updated_table <-  reactive({
    updated_table <- open_building_complaints %>%
      filter(status == "open",
             days_open >= input$complaint_min,
             days_open <= input$complaint_max,
             is.null(input$risk_category) | risk_category %in% input$risk_category,
             is.null(input$zip_code) | zip_code %in% input$zip_code,
             is.null(input$address) | street_address %in% input$address) %>%
      arrange(desc(days_open)) %>%
      mutate(risk_percentile = round(risk_percentile, 2) * 100,
              On_Map = paste('<a class="go-map" href="" data-lat="', latitude, '" data-long="', longitude, '" data-zip="', zip_code, '"><i class="fa fa-crosshairs"></i></a>', sep="")) %>%
      select(-longitude, -latitude, -year, -month, -day) %>%
      select(sr_number, status, street_address, zip_code, community_area, everything()) %>%
      rename_all(function(x) str_replace_all(string = x, pattern = "_", replacement = " "))
  })
  
  output$complaints <- DT::renderDataTable({
    
    action <- DT::dataTableAjax(session, updated_table())
    
    DT::datatable(updated_table(), options = list(ajax = list(url = action)), escape = FALSE)
    
  })
  
  ### Creating Complaints Dashboard
  
  theme_set(
    theme_minimal()
  )
  
  observe({
    months <- open_building_complaints %>%
      filter(year %in% input$year) %>%
      `$`("month") %>%
      unique() %>%
      sort()
    
    selection <- isolate(input$month[input$month %in% months])
    
    updateSelectInput(session, "month", choices = months,
                      selected = selection)
  })
  
  output$zip_count <- renderPlot({
    open_building_complaints %>%
      filter(status ==  "open",
             is.null(input$year) | year %in% input$year,
             is.null(input$month) | month %in% input$month,
             is.null(input$day_min) | day >= input$day_min,
             is.null(input$day_max) | day <= input$day_max) %>%
      count(status, zip_code) %>%
      top_n(n = 10, wt = n) %>%
      mutate(zip_code = reorder(zip_code, n)) %>%
      ggplot(aes(zip_code, n)) +
      geom_col(fill = "dodgerblue2") +
      coord_flip() +
      labs(title = "Top Zip 10 Codes by Open Complaints Count",
           x = NULL,
           y = "Open Complaints Count")
  })
  
  output$zip_preds <- renderPlot({
    open_building_complaints %>%
      filter(status ==  "open",
             is.null(input$year) | year %in% input$year,
             is.null(input$month) | month %in% input$month,
             is.null(input$day_min) | day >= input$day_min,
             is.null(input$day_max) | day <= input$day_max) %>%
      count(complaint_pred, zip_code) %>%
      top_n(n = 10, wt = n) %>%
      mutate(zip_code = reorder(zip_code, n)) %>%
      ggplot(aes(zip_code, n)) +
      geom_col(fill = "darkgoldenrod1") +
      coord_flip() +
      labs(title = "Top 10 Zip Codes by Open Complaints with a Prediction Violation Count",
           x = NULL,
           y = "Open Complaints Count")
  })
  
  output$complaint_series <- renderPlot({
    open_building_complaints %>%
      filter(is.null(input$year) | year %in% input$year,
             is.null(input$month) | month %in% input$month,
             is.null(input$day_min) | day >= input$day_min,
             is.null(input$day_max) | day <= input$day_max) %>%
      count(status, created_date) %>%
      mutate(created_date = as.Date(created_date)) %>%
      ggplot(aes(created_date, n, colour = status)) +
      geom_line(size = 1.5) +
      scale_colour_manual(values = c("darkorchid", "darkorange")) +
      labs(title = "Time Series Plot for Building Complaints",
           y = "Open Complaints Count",
           x = NULL,
           colour = "Complaint Status")
  })
  
  output$open_count <- renderText({ 
    open_count <- open_building_complaints %>%
      filter(is.null(input$year) | year %in% input$year,
             is.null(input$month) | month %in% input$month,
             is.null(input$day_min) | day >= input$day_min,
             is.null(input$day_max) | day <= input$day_max) %>%
      count(status)
    
    return(open_count$n[2])
  })
  
  output$closed_count <- renderText({ 
    closed_count <- open_building_complaints %>%
      filter(is.null(input$year) | year %in% input$year,
             is.null(input$month) | month %in% input$month,
             is.null(input$day_min) | day >= input$day_min,
             is.null(input$day_max) | day <= input$day_max) %>%
      count(status)
    
    return(closed_count$n[1])
  })
  
  ### Creating download button for filtered data set

  output$download_set <- downloadHandler(
    filename = function() {
      paste("Building_complaints-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(x = updated_table()[,-length(updated_table())], 
                file = file, 
                row.names = FALSE)
    }
  )    

}

#runApp(list(ui = ui, server = server))

#use show logs function to see what issues might have come up
#rsconnect::showLogs()
