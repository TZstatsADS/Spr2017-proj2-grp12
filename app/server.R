library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny) 
library(plotly)
library(ggplot2)
library(fmsb)

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
      addCircles(~Long, ~Lat, radius=~RadiusPop, layerId=~UID,
        stroke=FALSE, fillOpacity=0.4)#, fillColor=pal(colorData)) %>%  (colors to do)
      #addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
      #  layerId="colorLegend")
  })
  
  # observeEvent(input$zoom, {
  #   updateNumericInput(session, "admRates", value = 1:4)
  #   updateNumericInput(session, "admRates", value = 1:4)
  #   updateNumericInput(session, "admRates", value = 1:4)
  # })
  
  output$spider<-renderPlot({
    if (input$college==""){
      return(NULL)
    }
    sub<-newtable[newtable$Name==input$college,]
    Selectivity<-1-sub$AdmRate
    Popularity<-sub$Popularity
    Median_Debt<-sub$Debt
    Value_added<-sub$ValueAddedByRatio
    Diversity<-sub$Diversity
    true_radar<-c(Selectivity, Popularity, Median_Debt, Value_added, Diversity)
    radar<-as.data.frame(rbind(max_radar, min_radar, true_radar))
    colnames(radar)<-c("Selectivity", "Popularity", "Median Debt", "Value Added", "Diveristy")
    radarchart(radar)
    
    # Custom the radarChart !
    radarchart( radar  , axistype=1 ,
                #custom polygon
                pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                #custom labels
                vlcex=0.8
    )
  })
  
  output$gender_bar<-renderPlotly({
    if (input$college==""){
      return(NULL)
    }
    sub<-filteredData()[filteredData()$Name==input$college,]
    gender<-gender
    male<-sub$Gender.Men
    female<-sub$Gender.Women
    gen<-data.frame(gender,male,female)
    plot_ly(gen, x = ~male, y = ~gender, type = 'bar', orientation = 'h', name = 'Male') %>%
      add_trace(x = ~female, name = 'Female') %>%
      layout(xaxis = list(title = 'Proportion'), yaxis = list(title =""), barmode = 'stack')
  })
  
  output$race_bar<-renderPlotly({
    if (input$college==""){
      return(NULL)
    }
    sub<-filteredData()[filteredData()$Name==input$college,]
    ethnicity<-ethnicity
    white<-sub$White
    black<-sub$Black
    asian<-sub$Asian
    hispanic<-sub$Hispanic
    other<-sub$Other
    race<-data.frame(ethnicity, white, black, asian, hispanic, other)
    
    plot_ly(race, x = ~white, y = ~ethnicity, type = 'bar', orientation = 'h', name = 'White') %>%
      add_trace(x = ~black, name = 'Black') %>%
      add_trace(x = ~asian, name = 'Asian') %>%
      add_trace(x = ~hispanic, name = 'Hispanic') %>%
      add_trace(x = ~other, name = 'Other') %>%
      layout(xaxis = list(title = 'Proportion'), yaxis = list(title =""), barmode = 'stack')
  })
  
  output$table <- renderTable({
    sub<-newtable[newtable$Name==input$college,]
    Features<-c("Locale of Institution","School Type", "Admission Rate(%)", "In State Tuition($)", "Out of State Tuition($)", "Popularity", "Diversity",
                   "Average Faculty Income($)", "Percentage of Loan(%)", "Median Debt($)", "Proportion of First Generation(%)")
    Value<-c(sub$Locale,sub$School, round(sub$AdmRate*100,2),round(sub$TuitionIN,2),round(sub$Cost,2),round(sub$Popularity,2),
             round(sub$Diversity,4), round(sub$FacultySalary,2), round(sub$PercentageOfLoan*100,2), round(sub$Debt,2),round(sub$FirstGeneration*100,2))
    data<-data.frame(cbind(Features, Value))
    data
  })
  # This observer allow to zoom to a specific city or a university
  observe({
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
      addCircles(~anti_coords$Long, ~anti_coords$Lat, radius=~RadiusPop, layerId=~anti_coords$UID, stroke=FALSE, fillOpacity=0.4) %>%
      addCircles(~coords$Long, ~coords$Lat, radius=~RadiusPop, layerId=coords$UID, stroke=FALSE, fillOpacity=0.4, color = "red")
    }
    # if a university is selected
    else if ( input$zoom!="" && input$zoom %in% universities){
      coords <- filteredData()[filteredData()$Name==input$zoom,c(1,5,6)]
      anti_coords <- filteredData()[filteredData()$Name!=input$zoom,c(1,5,6)]
      leafletProxy("map", data = filteredData())  %>% 
      setView(lng=coords$Long, lat=coords$Lat, zoom=13) %>% clearShapes() %>%
      addCircles(~anti_coords$Long, ~anti_coords$Lat, radius=~RadiusPop, layerId=~anti_coords$UID, stroke=FALSE, fillOpacity=0.4) %>%
      addCircles(~coords$Long, ~coords$Lat, radius=~RadiusPop, layerId=coords$UID, stroke=FALSE, fillOpacity=0.4, color = "red")
    }
  })

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
      tags$a(selectedData$Link), tags$br()
#      sprintf("Admission rate: %s%%", round(selectedData$AdmRate*100,2)), tags$br(),
#      sprintf("Average tuition: %s", dollar(selectedData$Cost)), tags$br(),
#      sprintf("Percentage international: %s%%", round(selectedData$International*100,2))
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

  observeEvent(input$map_shape_click, {
    
    shinyjs::show(id = "conditionalPanel")
    event <- input$map_shape_click
    uid <- event$id
    name_uni <- dataRecent[dataRecent$UID==uid,2]
    updateTextInput(session, "college", value=name_uni)
    
  })
  observeEvent(input$map_click, {
    
    shinyjs::hide(id = "conditionalPanel")
    
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
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click")
    if ((length(s))&(input$comparison==F)) {
      pred <- s[["x"]]
      d_name <- nms[c(7:15)][pred]
      pres <- c(d_name,"ValueAddedByDifference")
      
      if ((pred %in% c(1,3,4,6,7,9))){
        if (input$color=="None"){
          p <- ggplot(newtable, aes_string(x = pres[1], y = pres[2], size =input$size))
        } else {
          p <- ggplot(newtable, aes_string(x = pres[1], y = pres[2], color = input$color, size =input$size))
        }
        
        p <- p + 
          geom_point(aes(text=paste("University:", newtable$Name)),shape=1,alpha=0.95) +
          scale_x_continuous(breaks=number_ticks(10)) +
          scale_y_continuous(breaks=number_ticks(10)) +
          scale_shape(solid = F) + theme(axis.text.x=element_text(angle = 15))
        
        if (input$regressionLine==T){
          p <- p + geom_smooth(method = "lm",fullrange=TRUE,size=0.3,alpha=0.7)
        }
        
      } else {
        if (input$color=="None"){
          p <- ggplot(newtable, aes_string(x = pres[1], y = pres[2]))
        } else {
          p <- ggplot(newtable, aes_string(x = pres[1], y = pres[2], color = input$color)) 
        }
        p <- p + 
          geom_boxplot() +
          geom_point(aes(text=paste("University:", newtable$Name)),shape=95,alpha=0.5)+
          theme(axis.text.x=element_text(angle = 15))
      }
      
      
    } else {
      if (input$color=="None"){
        p <- ggplot(newtable, aes_string(x = "ValueAddedByRatio", y = "ValueAddedByDifference", size =input$size))
      } else {
        p <- ggplot(newtable, aes_string(x = "ValueAddedByRatio", y = "ValueAddedByDifference", color = input$color, size =input$size))
      }
      p <- p + 
        geom_point(aes(text=paste("University:", newtable$Name)),shape=1,alpha=0.95) +
        scale_x_continuous(breaks=number_ticks(10)) +
        scale_y_continuous(breaks=number_ticks(10)) +
        scale_shape(solid = F) + theme(axis.text.x=element_text(angle = 15))
      
      if (input$regressionLine==T){
        p <- p + geom_smooth(method = "lm",fullrange=TRUE,size=0.3)
      }
      
      
    }
    
    
    
    if (input$referenceLine==T){
      p <- p + geom_hline(yintercept=0,color='grey50')+
        geom_vline(xintercept=0,color="grey50")
    }
    
    
    ggplotly(p)
  })
  
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")
    # if (length(s)==0){
    #   "Click on a bar in the barplot to explore association between value added and this predictor."
    # } 
    # # else {
    # #   #cat("You selected: \n\n")
    # #   as.list(s)
    # # }
  })
  
  output$histogram <- renderPlotly({
    p <- ggplot(adjR.square, aes(x = predictor, y = adj.r.square)) +
      theme_bw() + geom_bar(stat = "identity",alpha=0.5,fill = "aquamarine3")
  })

  
  }

# function(input, output) {
#   
#   # You can access the value of the widget with input$checkbox, e.g.
#   output$value <- renderPrint({ input$checkbox })
#   
# }