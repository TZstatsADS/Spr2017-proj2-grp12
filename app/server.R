library(shiny)
library(leaflet)
library(maps)

d<-read.csv("newest_data.csv")
shinyServer(function(input, output) {
  Data <- reactive({
    d
  })
  
  mapStates = map("state", fill = TRUE, plot = FALSE) 
  output$map <- renderLeaflet({
    leaflet(data = mapStates) %>% addTiles() %>% setView(-94, 37, zoom = 4) %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
  })
  observe({
    leafletProxy("map", data = Data()) %>%
 #     clearShapes() %>%
      addCircleMarkers(lat = ~ LATITUDE, lng = ~ LONGITUDE, 
                       color='red', radius=3, stroke = FALSE, fillOpacity = 0.5, popup=~INSTNM, popupOptions(closeButton=FALSE))
  })
  
})
