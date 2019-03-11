### Script builds the server necessary for the complaint inspections app

library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(tidyverse)
library(DT)
library(reticulate)
library(shinycssloaders)

source_python("OptimalRouter.py")

nominatim_osm <- function(address = NULL) {
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

complaints <- read.csv("Building_Complaints_Full.csv")

open_building_complaints <- readRDS(file = "open_building_complaints.rds")

effects_per_address <- readRDS(file = "address_coefficient_effects.rds")

average_glm_effect <- readRDS(file = "average_glm_effect.rds")

building_top_effects <- readRDS(file = "building_top_effects.rds")

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
                           "Model Probability Percentile:", open_building_complaints$model_probs)
    
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
    
    opt_schedule <-  eventReactive(input$optimal_route, {
      
      coords <- nominatim_osm(paste(input$opt_address, "Chicago, IL", collapse = ""))
      
      final_route <- TabuSearch(complaints, 
                                coords$lon, 
                                coords$lat) %>%
        mutate(group = "tour")
      
      leafletProxy("map", data = open_building_complaints) %>%
        addPolylines(data = final_route, lng = ~x, lat = ~y, group = ~group)
      
    })
    
    output$optimal_route <- renderPlot({
      withProgress(message = "Calculating Route...",{
        opt_schedule()
      })
    })
    
    #for connecting opt route
    #addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group)
  
  })
  
  ### Creating Generative Table
  
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
    
    DT::datatable(updated_table(), 
                  options = list(ajax = list(url = action)), 
                  escape = FALSE)
    
  })
  
    #### Creating download button for filtered data set
  
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
      theme(text = element_text(size = 16)) +
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
             is.null(input$day_max) | day <= input$day_max,
             model_probs == "80-100th Model Probability") %>%
      count(zip_code, model_probs, sort = T) %>%
      top_n(n = 10, wt = n) %>%
      mutate(zip_code = reorder(zip_code, n)) %>%
      ggplot(aes(zip_code, n)) +
      geom_col(fill = "darkgoldenrod1") +
      coord_flip() +
      theme(text = element_text(size = 16)) +
      labs(title = "Top 10 Zip Codes by 80-100th Percentile Model Probability Count",
           x = NULL,
           y = "80-100th Percentile Model Probability Count")
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
      theme(text = element_text(size = 16)) +
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
  
  ### Creating inferential tab
  
  top_coef_table <-  eventReactive(input$render_effect, {
    coef_table <- effects_per_address %>%
      filter(is.null(input$address_effect) | street_address %in% input$address_effect,
             rank < 5) %>%
      select(street_address, model_probs, variable, effect,
             change_from_avg, avg_comparison) %>%
      rename_all(function(x) str_replace_all(string = x, pattern = "_", replacement = " ")) %>%
      arrange(variable)
      
      
  })
  
  address_coef_plot <- eventReactive(input$render_effect, {
    
    building_example <- effects_per_address %>%
      filter(is.null(input$address_effect) | 
               street_address %in% input$address_effect,
             rank < 5) %>%
      arrange(variable)
    
    avg_line <- average_glm_effect %>%
      filter(variable %in% building_example$variable)
    
    building_top_effects %>%
      filter(variable %in% building_example$variable) %>%
      ggplot(aes(x_density, y_density)) +
      geom_line(size = 1.3, colour = "dodgerblue2", alpha = .5) +
      geom_vline(aes(xintercept = building_example$effect, colour = "Effect"), 
                 data = avg_line, size = 1.3) +
      geom_vline(aes(xintercept = avg_var_effect, colour = "Average Value"), 
                 data = avg_line, size = 1.3) +
      scale_colour_manual(values = c("darkorchid", "darkorange")) +
      facet_wrap(facets = "variable", scales = "free", nrow = 1) +
      theme(text = element_text(size = 16)) +
      labs(title = paste("Top 4 coefficient effects for building"),
           x = "Effect",
           y = "Density",
           colour = "Variable")
  })
  
  output$effect_facet <- renderPlot({
    address_coef_plot()
  })
  
  output$address_coef <- DT::renderDataTable({
    
    action <- DT::dataTableAjax(session, top_coef_table())
    
    DT::datatable(top_coef_table(), escape = FALSE)
    
  })

}

#runApp(list(ui = ui, server = server))

#use show logs function to see what issues might have come up
#rsconnect::showLogs()
