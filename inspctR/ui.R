library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(DT)
library(reticulate)

complaints <- read.csv("Building_Complaints_Full.csv") %>%
  filter(status != "completed")

open_building_complaints <- readRDS(file = "open_building_complaints.rds") %>%
  filter(status != "completed")

dashboard_complaints <- readRDS(file = "open_building_complaints.rds")

effects_per_address <- readRDS(file = "address_coefficient_effects.rds")

average_glm_effect <- readRDS(file = "average_glm_effect.rds")

building_top_effects <- readRDS(file = "building_top_effects.rds")

colour_vars <- c(
 "Past 21 Days" = "past_21_days",
 "Model Probability Percentile" = "model_probs"
)

ui <- navbarPage("inspctR", id = "nav", theme = shinytheme("cerulean"),

  ### Developing mapping panel               
                 
  tabPanel("Interactive Complaint Map",
           div(class = "outer",

   #### Utilizing custom css developed by Rstudio for mapping app
   tags$head(
     # Include our custom CSS
     includeCSS("styles.css"),
     includeScript("gomap.js")
   ),
   
   leafletOutput("map", width = "100%", height = "100%"),
   
   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                 draggable = TRUE, top = 60, left = "auto", 
                 right = 20, bottom = "auto",
                 width = 330, height = "auto",
                 
           h2("Complaint Explorer"),
                 
           selectInput("colour", "What to Highlight?", colour_vars),
           hr(),
           h2("Route Scheduler"),
           h4(span("Pick your prefered starting point and schedule an optimal building inspections route", style = "color:black")),
           selectInput("opt_address", "Starting Address", multiple = FALSE, 
                       choices = complaints$street_address),
           actionButton("optimal_route", "Schedule Optimal Route"),
           hr(),
           h5(em("Want to take your route to go? Download it below")),
           downloadButton("download_route", "Download Route")
           )
        ),
   plotOutput("optimal_route")
   ),
  
  ### Developing data exploration panel
  
  tabPanel("Open Complaint Explorer",
        
        fluidRow(
          column(3,
                 selectInput("zip_code", "Zip Code", multiple = TRUE, 
                             choices = open_building_complaints$zip_code
              )
            ),
          column(3,
              conditionalPanel("input.zip_code",
                 selectInput("address", "Address", multiple = TRUE, 
                             choices = c("All complaint addresses"="")
                 )
              )
            )
          ),
          hr(),
          fluidRow(
            column(3,
                 numericInput("complaint_min", "Min Days Open", 
                              min = 1, 
                              max = max(open_building_complaints$days_open), 
                              value = 1)
          ),
            column(3,
                 numericInput("complaint_max", "Max Days Open", 
                              min = 1, 
                              max = max(open_building_complaints$days_open), 
                              value = max(open_building_complaints$days_open))
          ),
          column(3,
                 selectInput("risk_category", "Model Probability Percentile", 
                             multiple = TRUE, 
                             choices = open_building_complaints$model_probs
                 )
          )
      ),
      fluidRow(
        h5(em("Done filtering? Export your customized set using the download button at the bottom of the page"))
      ),
    hr(),
    fluidRow(
      DT::dataTableOutput("complaints")
    ),
    hr(),
    fluidRow(
      column(1,
             downloadButton("download_set", "Download Data")
    )
  ),
  conditionalPanel("false", icon("crosshair"))
  ),
  
  ### Developing dashboard panel
  
  tabPanel("Complaints Inspections Dashboard",
           fluidRow(
             column(3,
                    selectInput("year", "Year", multiple = TRUE, 
                                choices = open_building_complaints$year
                    )
             ),
             column(3,
                    conditionalPanel("input.year",
                      selectInput("month", "Month", multiple = TRUE, 
                                  choices = c("All Months"="")
                    )
                  )
             ),
             column(3,
                    conditionalPanel("input.year",
                      numericInput("day_min", "First Day", 
                                 min = 1, 
                                 max = max(open_building_complaints$day), 
                                 value = 1)
                    )
             ),
             column(3,
                    conditionalPanel("input.year",
                      numericInput("day_max", "Last Day", 
                                 min = 1, 
                                 max = max(open_building_complaints$day), 
                                 value = max(open_building_complaints$day))
                    )
                )
             ),
           hr(),
           sidebarPanel(h1(
             strong(span("Complaints Overview", style = "color:black")
                           )
             ),
                        h2("Open Count", span(textOutput("open_count"), 
                                              style = "color:orange")),
                        h2("Completed Count", span(textOutput("closed_count"), 
                                                   style = "color:purple"))),
           mainPanel(
             splitLayout(cellWidths = c("50%", "50%"),
             plotOutput("zip_count"),
             plotOutput("zip_preds")
           ),
           plotOutput("complaint_series", height = "295")
        )
    ),
  
  ### Developing inferential panel
  
  tabPanel("What Effects Drive a Building Prediction?",
           fluidRow(
             column(3,
                    selectInput("address_effect", "Address", multiple = FALSE, 
                                choices = effects_per_address$street_address
                    )
             ),
             column(5, actionButton("render_effect", "Review Building Prediction"),
                    tags$style(type = "text/css", 
                               "#render_effect { padding-top: 15px; padding-bottom: 15px;}"))
            ),
           hr(),
           fluidRow(
             DT::dataTableOutput("address_coef")
           ),
           plotOutput("effect_facet")
  )
          
)

#rsconnect::deployApp(getwd())
