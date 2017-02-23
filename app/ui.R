library(leaflet)
library(plotly)
library(shinyjs)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

navbarPage("Invest Your Education", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      fluidRow(style = "padding-bottom: 0px; margin-top: 20px; margin-left: 10px;margin-right: 10px;",
               column(1, selectizeInput('stateFrom', 'Original state',
                                     choices = c("",levels(factor(dataRecent$State))), selected="")),
               column(2, selectizeInput(
                 'stateTo', 'States accepted', choices = levels(factor(dataRecent$State)), multiple = TRUE)),
               column(2, sliderInput("tuitionsUser", label = "Tuitions (by term)", min = 0, 
                                     max = 60000, value = c(1, 60000)) ),
               column(3, selectizeInput("majors", "Majors", choices = majors, multiple=TRUE)),
               column(2, checkboxGroupInput("admRates", label = "Selectivity of schools", 
                                             choices = list("Elitist" = 1, "Competitive" = 2, "Midrange" = 3, "Last half" = 4),
                                             selected = 1:4 ) ),
               column(2, selectizeInput('zoom', 'Look for City or University',
                                     choices = c("",cities, universities), selected=""))
      ),

      leafletOutput("map", width="100%", height="80%"),
      
      useShinyjs(), 
      hidden( 
        div(id = "conditionalPanel",
            fluidRow(
              absolutePanel(id = "controls", class = "panel panel-default", style="overflow-y:scroll; margin-bottom: 0px;", fixed = TRUE,
                            draggable = TRUE, top = 180, right=0, bottom = 0,
                            width = 400,
                            
                            h2("Detailed Information"),
                            
                            selectizeInput("college", 'University',
                                           choices = c("",universities), selected=""),
                            plotOutput("spider", height = 350),
                            plotlyOutput("gender_bar", height = 110),
                            plotlyOutput("race_bar", height = 110),
                            tableOutput("table"),
                            fluidRow(
                              
                            )
              )
            )
        )
      ),

      tags$div(id="cite",
        'Data compiled from ', tags$em('https://collegescorecard.ed.gov/'), ' by Jingwen, Vic, Virgile, Zhishan and Ziwei  (Columbia University, 2017).'
      )
    )
  ),
  tabPanel("Data explorer",
          
        
           
    fluidRow(
      headerPanel("Linear Regression Model"),
      sidebarPanel(
        #sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(diamonds),
        #            value = 1000, step = 500, round = 0),
        selectInput('x', 'X', choices = nms, selected = "Popularity"),
        selectInput('y', 'Y', choices = nms, selected = "ValueAddedbyRatio"),
        # selectInput('color', 'Color', choices = nms, selected = "clarity"),
        # 
        # selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "clarity"),
        # selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
        #sliderInput('plotHeight', 'Height of plot (in pixels)', 
                    #min = 100, max = 2000, value = 1000)
        helpText("Value_added= "),
        helpText("Diversity= "),
        helpText("Popularity= Number of Admitted Undergraduates/Admisson Rate "),
        checkboxInput("checkbox", label = "Draw Axis", value = TRUE),
        fluidRow(column(3, verbatimTextOutput("value")))
        ),
      
      
      mainPanel(
        plotlyOutput('trendPlot', height = "900px"))),
    
 

  conditionalPanel("false", icon("crosshair"))
)
)
