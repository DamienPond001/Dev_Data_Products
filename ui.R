#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(leaflet)
library(leaflet.extras)
library(shiny)
library(markdown)
library(ggplot2)
library(geosphere)

dataset <- read.csv("./Data/housing.csv")

locations <- data.frame(lng = round(runif(nrow(dataset), -71.085, -71.059),4), 
                        lat = round(runif(nrow(dataset), 42.294, 42.323),4))

dataset <- cbind(dataset, locations)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
  
  # Application title
  titlePanel("Boston Location Price Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        width = 6,
        h3("Your choices (vertical lines) against location data."),
        fluidRow(
            splitLayout(cellWidths = c("80%"),
                        plotOutput("plotRM", height = "170px")
            )),
        fluidRow(
            splitLayout(cellWidths = c("100%"),
                        sliderInput("RM", 
                                    "Average Number of Rooms:", 
                                    min = round(min(dataset$RM), 1),
                                    max = round(max(dataset$RM), 1),
                                    value = round(mean(dataset$RM), 1),
                                    step = 0.1)
            )),
        fluidRow(
            splitLayout(cellWidths = c("80%"),
                        plotOutput("plotLSTAT", height = "170px")
            )),
        fluidRow(
            splitLayout(cellWidths = c("100%"),
                       
                        sliderInput("LSTAT", 
                                    "Percentage of homeowners in the neighborhood considered 'lower class':",
                                    min = round(min(dataset$LSTAT), 1),
                                    max = round(max(dataset$LSTAT), 1),
                                    value = round(mean(dataset$LSTAT), 1))
            )),
        fluidRow(
            splitLayout(cellWidths = c("80%"),
                        
                        plotOutput("plotPT", height = "170px")
            )),
        fluidRow(
            splitLayout(cellWidths = c("100%"),
                        
                        sliderInput("PTRATIO", 
                                    "Ratio of Students to Teachers at Local Schools:",
                                    min = round(min(dataset$LSTAT), 1),
                                    max = round(max(dataset$LSTAT), 1),
                                    value = round(mean(dataset$LSTAT), 1))
            ))
        
        
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        width = 6, 
        leafletOutput("locationMap"),
        br(),
        h4("Set latitude, longitude and radius to get the average, max and min house value within an area:"),
        splitLayout(cellWidths = c("50%", "50%"),
                    sliderInput("lat", "Select Latitude:", value = 42.294, min = 42.294, max = 42.323, step = 0.005)),
        splitLayout(cellWidths = c("50%", "50%"),
                    sliderInput("long", "Select Longitude:", value = -71.085, min = -71.085, max = -71.059, step = 0.005),
                    tableOutput("Average")),
        splitLayout(cellWidths = c("50%", "50%"),
                    sliderInput("radius", "Select Radius:", value = 0, min = 0, max = 500, step = 50)),
        hr(),
        h4("Linear Model Prediction Based on Chosen Predictors:"),
        textOutput("prediction")
    )
  )
))
