library(shiny)
library(shiny.react)
library(jsonlite)


ui <- fluidPage(

    tags$head(
      tags$script(type = "module", src = "ballspinapp.es.js")
    
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("spinSpeed", "Spin Speed", min = 0, max = 100, value = 50),
      uiOutput("pitchSelectUI")
    ),
    mainPanel(
      reactOutput("ballSpin")
    )
  )
)

server <- function(input, output, session) {
  
  pitches <- reactive({
    jsonlite::fromJSON("www/gilbert_augEighth.json")
  })
  
  output$pitchSelectUI <- renderUI({
    pitch_data <- pitches()
    choices <- seq_along(pitch_data$PitchNumber) - 1
    names(choices) <- paste0("Pitch ", pitch_data$PitchNumber, ": ", pitch_data$PitchType)
    selectInput("pitchIndex", "Select Pitch", choices = choices, selected = 0)
    
  })
  
  output$ballSpin <- shiny.react::renderReact({
    shiny.react::reactElement(
      module = NULL,
      name = "BallSpinApp",
      # source = "ballspinapp.es.js",
      props = list(
        spinRate = 10,#input$spinSpeed,
        selectedPitchIdx = 0, #input$pitchIndex,
        playing = FALSE,
        userSpinAxis = NULL
      )
    )
  })
}

shinyApp(ui, server)



# tags$head(
#   tags$script(src = "https://unpkg.com/react@18/umd/react.production.min.js"),
#   tags$script(src = "https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"),
#   tags$script(src = "https://unpkg.com/three/build/three.min.js"),
#   tags$script(type = "module", src = "ballspinapp.es.js")
#   # 
#   # tags$script(src = "static/js/main.8d1d2d31.js"),
#   # tags$link(rel = "stylesheet", href = "static/css/main.e6c13ad2.css")
# ),
# tags$script(type = "module", src = "https://esm.sh/react@18"),
# tags$script(type = "module", src = "https://esm.sh/react-dom@18"),
# 
#       tags$script(src = "https://unpkg.com/react@18/umd/react.production.min.js"),
#       tags$script(src = "https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"),
#       tags$script(src = "https://unpkg.com/three/build/three.min.js"),
#       tags$script(src = "ballspinapp.umd.js"),



# tags$script(type = "module", src = "https://esm.sh/react"),
# tags$script(type = "module", src = "https://esm.sh/react-dom"),
# tags$script(type = "module", src = "https://esm.sh/three"),
# tags$script(type = "module", src = "ballspinapp.es.js"), 
