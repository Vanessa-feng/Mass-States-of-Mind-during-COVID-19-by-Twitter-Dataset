library(leaflet)

# Choices for drop-downs
vars <- c("Points", "Regions")

ui <- navbarPage("STAT209", id="nav",

  tabPanel("Interactive Map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),
      fluidRow(verbatimTextOutput("map_marker_click")),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Plot Type"),

        selectInput("type", "Type", vars, selected="Total"),
        sliderInput("slider", "Time", min=as.Date("2020-01-01"), max=as.Date("2021-05-01"),
        value=as.Date("2021-04-01"), step=16, timeFormat="%b %Y"),
        textOutput("SliderText"),
        plotOutput("histPo", height = 200)
        # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'"
          # Only prompt for threshold when coloring or sizing by superzip
          # numericInput("threshold", "Country", world_spdf@data$ISO3)
        )

        
        # plotOutput("scatterGDP", height = 250)
      )

    # tags$div(id=Pop$id,
    # 'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), 
    # ' by Charles Murray (Crown Forum, 2012).'
    # )
  # )
)
)
