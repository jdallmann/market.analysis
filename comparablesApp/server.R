################################################
# This Shiny server builds interactive real 
# estate comparaison classes.
#
# Coded by Justin M. Dallmann, (jdallmann.org)
# and inspired by the Shiny galery example:
# https://shiny.rstudio.com/gallery/bus-dashboard.html
################################################

library(shinydashboard)
library(leaflet)
library(dplyr)

# Colors for 12 clusters
pal <- colorFactor("Paired", 1:12)


# Load cleaned and imputed data frame for clustering 
# also contains unimputed values for reporting
reData <- read.csv("cleanFrameFull.csv", stringsAsFactors = FALSE)
reData$UI.year <- as.character(reData$UI.year)


# Weight the data for clustering using KRIGING?



# Clustering function taking a vector of variable names
# CLUSTVARS to cluster on with longitude and latitude.
clustify <- function(clustVars) {
  clustNum <- 12
  set.seed(1985)
  # Get comparables factor using Hartigan and Wong
  # (1979) K-means clustering
  if (length(clustVars)>0){
    km <- select(reData, long, lat, clustVars) %>%
      kmeans(centers = clustNum, nstart = 100)
  } else{
    km <- select(reData, long, lat) %>%
      kmeans(centers = clustNum, nstart = 100)
  }
  # Update cluster variable
  reDataClustered <- mutate(reData, comparables = km$cluster)
  reDataClustered
}

# Function from new test case to cluster
findClust <- function(newCase) {
  clustDist <- apply(km$centers, 1, 
                     function(y) {
                       sqrt(sum((newCase-y)^2))
                     })
  return(which.min(clustDist)[1])
}


function(input, output, session) {
  # OTHER IDEAS
  # 1. add comparable option and a button to predict on it.
  # 1.1 mark that separately with a marker (noting the class)
  # 1.2 zoom in on the class (perhaps with button). Use fitbounds
  # in a observer proxyleaflet call.
  # Do a more fine-grained clustering within the cluster?/
  # Do a prediction within the cluster?
  
  # Get most recent data
  propUpdate <- reactive({
    # Get most recent data
    clustify(input$compClass)
  })
  
  
  
  output$comparablesTable <- renderUI({
    reDataClust <- propUpdate()
    # Create a Bootstrap-styled table
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("Number of homes"),
                 tags$th("Standard deviation"),
                 tags$th("Median price")
               )),
               tags$tbody(
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(1)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 1))),
                   tags$td(round(sd(filter(reDataClust, comparables == 1)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 1)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(2)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 2))),
                   tags$td(round(sd(filter(reDataClust, comparables == 2)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 2)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(3)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 3))),
                   tags$td(round(sd(filter(reDataClust, comparables == 3)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 3)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(4)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 4))),
                   tags$td(round(sd(filter(reDataClust, comparables == 4)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 4)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(5)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 5))),
                   tags$td(round(sd(filter(reDataClust, comparables == 5)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 5)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(6)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 6))),
                   tags$td(round(sd(filter(reDataClust, comparables == 6)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 6)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(7)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 7))),
                   tags$td(round(sd(filter(reDataClust, comparables == 7)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 7)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(8)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 8))),
                   tags$td(round(sd(filter(reDataClust, comparables == 8)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 8)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(9)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 9))),
                   tags$td(round(sd(filter(reDataClust, comparables == 9)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 9)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(10)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 10))),
                   tags$td(round(sd(filter(reDataClust, comparables == 10)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 10)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(11)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 11))),
                   tags$td(round(sd(filter(reDataClust, comparables == 11)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 11)$UI.price))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     pal(12)
                   ))),
                   tags$td(nrow(filter(reDataClust, comparables == 12))),
                   tags$td(round(sd(filter(reDataClust, comparables == 12)$UI.price))),
                   tags$td(median(filter(reDataClust, comparables == 12)$UI.price))
                 ),
                 tags$tr(class = "active",
                         tags$td("Total"),
                         tags$td(nrow(reDataClust)),
                         tags$td(round(sd(reDataClust$UI.price))),
                         tags$td(median(reDataClust$UI.price))
                 )
               )
    )
  })
  
  
  output$comparablesMap <- renderLeaflet({
    compMap <- leaflet(reData) %>%
      addTiles() %>%
      addCircleMarkers(lng = reData$UI.long,
                       lat = reData$UI.lat,
                       popup = reData$UI.year,
                       color =  pal(reData$comparables))
    
    compMap
  })
  
  # Reactive proxy to update clusters.
  observe({
    reDataClustered <- propUpdate()
    leafletProxy("comparablesMap", data = reDataClustered) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = reDataClustered$UI.long,
                       lat = reDataClustered$UI.lat,
                       popup = reDataClustered$UI.year,
                       color =  pal(reDataClustered$comparables))
  })
  
}
