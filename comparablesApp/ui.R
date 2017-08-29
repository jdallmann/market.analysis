################################################
# This Shiny UI provides the input for an
# interactive real estate comparaison server.
#
# Coded by Justin M. Dallmann, (jdallmann.org)
# and inspired by the Shiny galery example:
# https://shiny.rstudio.com/gallery/bus-dashboard.html
################################################

library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "Real Estate Exploration"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("comparablesMap", height = 500)
           ),
           box(width = NULL,
               uiOutput("comparablesTable")
           )
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
               #        uiOutput("yearSelect"),
               checkboxGroupInput("compClass", "Use to make comparaison",
                                  choices = c(
                                    "Living area" = "living.area.M2",
                                    "Year" = "year.built",
                                    "Frontage" = "frontage.M",
                                    "Garage" = "garage"
                                  ),
                                  selected = c("living.area.M2")
               ),
               p(
                 class = "text-muted",
                 "Note: Location is always used."
               )
           ),
           box(width = NULL, status = "warning",
               strong(
                 "Documentation:"  
               ),
               p(
                 "This app classifies properties for comparaison.*"
               ),
               p(
                 "Instructions:",
                 tags$ol(
                  tags$li("Adjust the interactive map to zoom in or out."), 
                  tags$li("Use the checkboxes to select which features 
                          to use in the comparaison."), 
                  tags$li("Click on a property to view the year built.")
                 )
               ),
               p(
                 "Statistics on each class of comparables (featured below the
                 map) are dynamically adjusted with selection."
               ),
               p(
                 class = "text-muted",
                 em("*The clustering algorithm uses k-means clustering, with k=12,
                    on two months of centered and scaled real estate data from 
                    Winnipeg, Canada.")
                 )
                 )
           
               )
               )
  )

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)