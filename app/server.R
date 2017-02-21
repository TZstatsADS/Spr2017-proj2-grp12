library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Original template taken from: https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# - Original
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]

# - My data

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map (OK)
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  # dataInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(dataRecent[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  # 
  #   subset(dataRecent,
  #     Lat >= latRng[1] & Lat <= latRng[2] &
  #       Long >= lngRng[1] & Long <= lngRng[2])
  # })
  
  filteredData <- reactive({
    res <- dataRecent
    # apply in-state tuition if not international student
    if (input$stateFrom!=""){
      # if not international, then apply rate of state from
      res$Cost[res$State==input$stateFrom] <- res$TuitionIN[res$State==input$stateFrom]
    }
    # filter states of uni
    if (!is.null(input$stateTo)){
      res <- res %>% filter(State %in% input$stateTo)
    }
    # filter majors
    if (!is.null(input$majors)){
      maj <- apply(matrix(which(majors %in% input$majors)),1,function(x)paste("MAJPER",x))
      res <- res[res[,maj]!=0,]
    }
    
    # filter tuitions fees and admission rates 
    res %>% filter(Cost>input$tuitionsUser[1],Cost<input$tuitionsUser[2],
                   ((1 %in% input$admRates) & AdmRate<adm_rate_th[1]) | ((2 %in% input$admRates) & AdmRate<adm_rate_th[2] & AdmRate>=adm_rate_th[1]) | ((3 %in% input$admRates) & AdmRate<adm_rate_th[3] & AdmRate>=adm_rate_th[2]) | ((4 %in% input$admRates) & AdmRate<1 & AdmRate>=adm_rate_th[3]))
  })

  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  # 
  #   hist(zipsInBounds()$centile,
  #     breaks = centileBreaks,
  #     main = "SuperZIP score (visible zips)",
  #     xlab = "Percentile",
  #     xlim = range(allzips$centile),
  #     col = '#00DD00',
  #     border = 'white')
  # })

  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  # 
  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })

  #This observer is responsible for maintaining the circles and legend,
  #according to the variables the user has chosen to map to color and size.
  observe({
  #   I#nput modification for map (to do) 
  #   colorBy <- input$color 
  #   sizeBy <-  input$size
  # 
  #   if (colorBy == "superzip") { # Color and palette are treated specially in
  #   the "superzip" case, because # the values are categorical instead of
  #   continuous. colorData <- ifelse(zipdata$centile >= (100 -
  #   input$threshold), "yes", "no") pal <- colorFactor("Spectral", colorData) }
  #   else { colorData <- zipdata[[colorBy]] pal <- colorBin("Spectral",
  #   colorData, 7, pretty = FALSE) }
  # 
  #   if (sizeBy == "superzip") { # Radius is treated specially in the
  #   "superzip" case. radius <- ifelse(zipdata$centile >= (100 -
  #   input$threshold), 30000, 3000) } else { radius <- zipdata[[sizeBy]] /
  #   max(zipdata[[sizeBy]]) * 30000 }
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(~Long, ~Lat, radius=750, layerId=~UID,
        stroke=FALSE, fillOpacity=0.4)#, fillColor=pal(colorData)) %>%  (colors to do)
      #addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
      #  layerId="colorLegend")
  })
  
  # This observer allow to zoom to a specific city or a university
  observe(
    # if it is a city, zoom to view with all universities
    if ( input$zoom!="" && input$zoom %in% cities){
      coords <- filteredData()[filteredData()$City==input$zoom,c(1,5,6)]
      anti_coords <- filteredData()[filteredData()$City!=input$zoom,c(1,5,6)]
      # if group of universities
      if (dim(coords)[1]!=1){
        range <- as.numeric(apply(coords[2:3],2,FUN=function(x)diff(range(x))))/5
        leafletProxy("map", data = filteredData()) %>%
        fitBounds(min(coords$Long)-range[1], min(coords$Lat)-range[2], max(coords$Long)+range[1], max(coords$Lat)+range[2])  
      }
      # if one university in city
      else{
        leafletProxy("map", data = filteredData()) %>% 
        setView(lng=coords$Long, lat=coords$Lat, zoom=13)  
      }
      # update map
      leafletProxy("map", data = filteredData()) %>% clearShapes() %>%
      addCircles(~anti_coords$Long, ~anti_coords$Lat, radius=750, layerId=~anti_coords$UID, stroke=FALSE, fillOpacity=0.4) %>%
      addCircles(~coords$Long, ~coords$Lat, radius=750, layerId=coords$UID, stroke=FALSE, fillOpacity=0.4, color = "red")
    }
    # if a university is selected
    else if ( input$zoom!="" && input$zoom %in% universities){
      coords <- filteredData()[filteredData()$Name==input$zoom,c(1,5,6)]
      anti_coords <- filteredData()[filteredData()$Name!=input$zoom,c(1,5,6)]
      leafletProxy("map", data = filteredData())  %>% 
      setView(lng=coords$Long, lat=coords$Lat, zoom=13) %>% clearShapes() %>%
      addCircles(~anti_coords$Long, ~anti_coords$Lat, radius=750, layerId=~anti_coords$UID, stroke=FALSE, fillOpacity=0.4) %>%
      addCircles(~coords$Long, ~coords$Lat, radius=750, layerId=coords$UID, stroke=FALSE, fillOpacity=0.4, color = "red")
    }
  )

  # Show a popup at the given location (ORIGINAL)
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #       selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  
  # My own popup
  showCollegePopup <- function(uid, lat, lng) {
    selectedData <- filteredData()[filteredData()$UID == uid,]
    content <- as.character(tagList(
      tags$h4(selectedData$Name),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedData$City, selectedData$State, selectedData$Zip
      ))), tags$br(),
      tags$a(selectedData$Link), tags$br(),
      sprintf("Admission rate: %s%%", round(selectedData$AdmRate*100,2)), tags$br(),
      sprintf("Average tuition: %s", dollar(selectedData$Cost)), tags$br(),
      sprintf("Percentage international: %s%%", round(selectedData$International*100,2))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = uid)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showCollegePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################
# 
#   observe({
#     cities <- if (is.null(input$states)) character(0) else {
#       filter(cleantable, State %in% input$states) %>%
#         `$`('City') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$cities[input$cities %in% cities])
#     updateSelectInput(session, "cities", choices = cities,
#       selected = stillSelected)
#   })
# 
#   observe({
#     zipcodes <- if (is.null(input$states)) character(0) else {
#       cleantable %>%
#         filter(State %in% input$states,
#           is.null(input$cities) | City %in% input$cities) %>%
#         `$`('Zipcode') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#     updateSelectInput(session, "zipcodes", choices = zipcodes,
#       selected = stillSelected)
#   })
# 
#   observe({
#     if (is.null(input$goto))
#       return()
#     isolate({
#       map <- leafletProxy("map")
#       map %>% clearPopups()
#       dist <- 0.5
#       zip <- input$goto$zip
#       lat <- input$goto$lat
#       lng <- input$goto$lng
#       showZipcodePopup(zip, lat, lng)
#       map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#     })
#   })
# 
#   output$ziptable <- DT::renderDataTable({
#     df <- cleantable %>%
#       filter(
#         Score >= input$minScore,
#         Score <= input$maxScore,
#         is.null(input$states) | State %in% input$states,
#         is.null(input$cities) | City %in% input$cities,
#         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#       ) %>%
#       mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#     action <- DT::dataTableAjax(session, df)
# 
#     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#   })
}
