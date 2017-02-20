library(shiny)
library(leaflet)
library(maps)

ui <- fluidPage(
  leafletOutput("map")
)
