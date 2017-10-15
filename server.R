#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

priceAve <- function(locations, radius, latitude, longitude)
{
    inRange <- distm(as.matrix(locations[, c("lng", "lat")]), c(longitude,latitude)) < radius
    subset <- locations[inRange,]
    data.frame(Mean = mean(subset$MEDV), Max = max(subset$MEDV), Min = min(subset$MEDV))
}

devModel <- function(locations, input1, input2, input3)
{
    model <- lm(MEDV ~ RM + LSTAT + PTRATIO, data = locations)
    predict(model, data.frame(RM = input1, LSTAT = input2, PTRATIO = input3))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    dataset <- read.csv("./Data/housing.csv")
    
    locations <- data.frame(lng = round(runif(nrow(dataset), -71.085, -71.059),4), 
                            lat = round(runif(nrow(dataset), 42.294, 42.323),4))
    
    dataset <- cbind(dataset, locations)
    
    pal <- colorNumeric(
        palette = "viridis",
        domain = dataset$MEDV
    )
   
    output$plotRM <- renderPlot({
    ggplot(dataset, aes(x = RM, y = MEDV)) + geom_point() + geom_vline(xintercept = input$RM,
                                                                       color = "blue",
                                                                       size = 1.5)
    })
        
    output$plotLSTAT <- renderPlot({
    ggplot(dataset, aes(x = LSTAT, y = MEDV)) + geom_point() + geom_vline(xintercept = input$LSTAT,
                                                                       color = "blue",
                                                                       size = 1.5)
    })
                 
    output$plotPT <- renderPlot({
    ggplot(dataset, aes(x = PTRATIO, y = MEDV)) + geom_point() + geom_vline(xintercept = input$PTRATIO,
                                                                       color = "blue",
                                                                       size = 1.5)
    
    })
    
    output$locationMap <- renderLeaflet({
        dataset %>% leaflet() %>% 
            addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Street") %>% 
            addHeatmap(lng = ~lng, lat = ~lat, 
                       intensity = ~MEDV,
                       gradient = "viridis",
                       group = "Heat Map",
                       blur = 40, max = max(dataset$MEDV), radius = 30) %>%
            addMarkers(clusterOptions = markerClusterOptions(),group = "Markers", popup = ~(paste("Location: <br> Latitude: ", as.character(lat), " Longitude: ", as.character(lng),"<br/>",  "Price: $",as.character(MEDV),sep = ""))) %>%
            addLayersControl(baseGroups = c("Street"),
                             overlayGroups = c("Markers", "Heat Map"))%>%
            addLegend("bottomright", pal = pal, values = ~MEDV,
                      title = "Property Price",
                      labFormat = labelFormat(prefix = "$"),
                      opacity = 1) %>%
            addCircles(lng = input$long, lat = input$lat, radius = input$radius, fillOpacity = 0.2, stroke = FALSE)
    })
    
    AverageReact <-reactive({
        radius <- input$radius
        lat <- input$lat
        long <- input$long
        priceAve(dataset, radius, lat, long)
    })
    
    output$Average <- renderTable({
        AverageReact()
    })
    
    model <- reactive({
        devModel(dataset, input$RM, input$LSTAT, input$PTRATIO)
    })
    

    output$prediction <- renderText({
        paste("For RM = ",input$RM ,", LSTAT = ", input$LSTAT," and PTRATIO = ", input$PTRATIO,
              " one can expect to pay around $", round(model(),2) ,sep = "")
    })
  
})
