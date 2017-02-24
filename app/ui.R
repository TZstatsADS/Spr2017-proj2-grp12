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
      
      fluidRow(style = "padding-top: 0px; padding-bottom: 0px; margin-top: 20px; margin-left: 10px;margin-right: 10px;",
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

      leafletOutput("map", width="100%", height = "80%"), #565
      
      useShinyjs(), 
      hidden( 
        div(id = "conditionalPanel",
            fluidRow(
              absolutePanel(id = "controls", class = "panel panel-default", style="overflow-y:scroll; margin-top: 20px; margin-bottom: 0px;", fixed = TRUE,
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
        'Data compiled from ', tags$em('https://collegescorecard.ed.gov/'), ' by Jingwen, Virgile, Zhishan and Ziwei  (Columbia University, 2017).'
      )
    )
  ),
  tabPanel("Data explorer",
           fluidRow(
             headerPanel("Predictor Analysis"),
             sidebarPanel(
               selectInput('color', 'Color', choices = c(nms[c(3,6,8,11,14)],"None"), selected = "FamilyIncomeGroup"),
               selectInput('size', 'Size', choices = nms[c(2,7,9,10,12,13,15,16)], selected = "AdmRate"),
               checkboxInput("regressionLine", label = "Regression Line", value = FALSE),
               checkboxInput("referenceLine", label = "Reference Line", value = TRUE),
               checkboxInput("comparison", label = "Compare two metrics", value = FALSE),
               fluidRow(column(10, verbatimTextOutput("selection"))),
               width = 3
             ),
             mainPanel(
               plotlyOutput('scatterplot'))),
           plotlyOutput('histogram',width="100%", height = 300),
           
           
           
           
           conditionalPanel("false", icon("crosshair"))
)
)
