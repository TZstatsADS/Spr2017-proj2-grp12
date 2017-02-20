library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage("Find your college", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      fluidRow(style = "padding-bottom: 0px; margin-top: 20px; margin-left: 10px;margin-right: 10px;",
               column(1, selectizeInput('stateFrom', 'Original state',
                                     choices = levels(factor(dataRecent$State)) )),
               column(1, selectizeInput(
                 'stateTo', 'States accepted', choices = levels(factor(dataRecent$State)), multiple = TRUE)),
               column(2, sliderInput("tuitionsUser", label = "Tuitions", min = 0, 
                                     max = 2000, value = c(10, 1500)) ),
               column(3, selectizeInput("majors", "Majors", choices = majors, multiple=TRUE)),
               column(2, checkboxGroupInput("admRates", label = "Selectivity of schools", 
                                             choices = list("Elistist" = 0.15, "Competitive" = 0.30, "Midrange" = 0.5, "Last half" = 1),
                                             selected = c(0.15,0.30)) ),
               column(3, selectizeInput('zoom', 'Look for City or University',
                                     choices = c(cities, universities)))
      ),

      leafletOutput("map", width="100%", height="100%"),
      
      # Absolute panel (to do)
      # Shiny versions prior to 0.11 should use class="modal" instead.
      # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      #   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
      #   width = 330, height = "auto",
      # 
      #   h2("ZIP explorer"),
      # 
      #   selectInput("color", "Color", vars),
      #   selectInput("size", "Size", vars, selected = "adultpop"),
      #   conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
      #     # Only prompt for threshold when coloring or sizing by superzip
      #     numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
      #   ),
      # 
      #   plotOutput("histCentile", height = 200),
      #   plotOutput("scatterCollegeIncome", height = 250)
      # ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960???2010'), ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),

  # tabPanel("Data explorer",
  #   fluidRow(
  #     column(3,
  #       selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
  #     ),
  #     column(3,
  #       conditionalPanel("input.states",
  #         selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
  #       )
  #     ),
  #     column(3,
  #       conditionalPanel("input.states",
  #         selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
  #       )
  #     )
  #   ),
  #   fluidRow(
  #     column(1,
  #       numericInput("minScore", "Min score", min=0, max=100, value=0)
  #     ),
  #     column(1,
  #       numericInput("maxScore", "Max score", min=0, max=100, value=100)
  #     )
  #   ),
  #   hr(),
  #   DT::dataTableOutput("ziptable")
  # ),

  conditionalPanel("false", icon("crosshair"))
)
