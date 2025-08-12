library(shiny)
library(shinyjs)
library(jsonlite)
library(here)

# Load pitch data
# pitch_data <- fromJSON(here("gilbert_augEighth.json"))
load_pitch_data <- function() {
  paths <- c("www/gilbert_augEighth.json", "gilbert_augEighth.json")
  
  for (path in paths) {
    if (file.exists(path)) {
      message("Loading pitch data from: ", path)
      return(fromJSON(readLines(path, warn = FALSE)))
    }
  }
  
  stop("Could not find gilbert_augEighth.json in any known location.")
}

pitch_data <- load_pitch_data()
pitch_labels <- paste0(pitch_data$PitchNumber, " ", pitch_data$PitchType)
pitch_choices <- setNames(seq_along(pitch_labels), pitch_labels)

# Immediate slider input wrapper  data-immediate="true"
immediateSlider <- function(inputId, label, min, max, value, step = NULL, ...) {
  sld <- sliderInput(inputId, label, min, max, value, step = step, ...)
  sld$children[[2]]$attribs$`data-immediate` <- "true"
  sld
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(type = "module", src = "ballspinapp.es.js"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('pitch_idx', function(selectedIdx) {
        window.postMessage({ type: 'pitch_idx', value: selectedIdx }, '*');
      });
      Shiny.addCustomMessageHandler('slider_update', function(sliderValues) {
        window.postMessage({ type: 'slider_update', ...sliderValues }, '*');
      });
      Shiny.addCustomMessageHandler('play_toggle', function(val) {
        window.postMessage({ type: 'play_toggle', value: val }, '*');
      });
    "))
  ),
  tags$div(id = "root", style = "width: 100vw; height: 100vh;"),
  
  absolutePanel(
    top = 10, left = 10, zIndex = 1000, draggable = FALSE,
    selectInput("pitch_idx", "Select Pitch:", choices = pitch_choices, selected = 1),
    immediateSlider("spinTilt", "Spin Axis Tilt (°)", min = 0, max = 360, value = 120, step = 1),
    immediateSlider("spinGyro", "Spin Axis Gyro (°)", min = 0, max = 360, value = 0, step = 1),
    immediateSlider("ballX", "Ball Orientation X (°)", min = -180, max = 180, value = 0, step = 1),
    immediateSlider("ballY", "Ball Orientation Y (°)", min = -180, max = 180, value = 0, step = 1),
    actionButton("play_pause_btn", "Pause")
  ),
  
  verbatimTextOutput("sliderDebug")  # For debugging 
)

server <- function(input, output, session) {
  # ReactiveValues to indicate playing state
  playing <- reactiveVal(TRUE)
  
  # Update pitch selection message
  observeEvent(input$pitch_idx, {
    session$sendCustomMessage("pitch_idx", as.numeric(input$pitch_idx))
    
    # On pitch change, reset playing and UI button
    playing(TRUE)
    updateActionButton(session, "play_pause_btn", label = "Pause")
    session$sendCustomMessage("play_toggle", TRUE)
  })
  
  observe({
    vals <- list(
      spinTilt = input$spinTilt,
      spinGyro = input$spinGyro,
      ballX = input$ballX,
      ballY = input$ballY
    )
    session$sendCustomMessage("slider_update", vals)
  })
  
  # Play/pause button toggling
  observeEvent(input$play_pause_btn, {
    new_state <- !playing()
    playing(new_state)
    updateActionButton(session, "play_pause_btn", label = ifelse(new_state, "Pause", "Play"))
    session$sendCustomMessage("play_toggle", new_state)
  })
  
  # Debug
  output$sliderDebug <- renderPrint({
    list(
      spinTilt = input$spinTilt,
      spinGyro = input$spinGyro,
      ballX = input$ballX,
      ballY = input$ballY,
      playing = playing()
    )
  })
}

shinyApp(ui, server)



