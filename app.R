library(shiny)
library(shinyjs)
library(jsonlite)
library(here)

# Load pitch data
load_pitch_data <- function() {
  paths <- c("www/gilbert_augEighth.json", "gilbert_augEighth.json")
  
  for (path in paths) {
    if (file.exists(path)) {
      message("Loading pitch data from: ", path)
      return(fromJSON(readLines(path, warn = FALSE)))
    }
  }
  stop("Could not find gilbert_augEighth.json ")
}

pitch_data <- load_pitch_data()

# Create choices + display label with PitchNumber and PitchType
pitch_labels <- paste0(pitch_data$PitchNumber, " ", pitch_data$PitchType)
pitch_choices <- setNames(pitch_data$PitchUID, pitch_labels)
# Immediate slider input wrapper data-immediate="true"
immediateSlider <- function(inputId, label, min, max, value, step = NULL, ...) {
  sld <- sliderInput(inputId, label, min, max, value, step = step, ...)
  sld$children[[2]]$attribs$`data-immediate` <- "true"
  sld
}

# Custom slider with input field
customSliderWithInput <- function(inputId, label, min, max, value, step = NULL, suffix = "", ...) {
  sliderInputId <- paste0(inputId, "_slider")
  textInputId <- paste0(inputId, "_text")
  div(class = "custom-slider-group",
      div(class = "slider-header",
          span(class = "slider-label", label)
      ),
      # Combined row for slider + input box
      div(class = "slider-row",
          div(class = "slider-container",
              style = "flex: 1 1 0;",
              immediateSlider(sliderInputId, NULL, min, max, value, step = step, ticks = FALSE, ...)
          ),
          div(class = "slider-input-container",
              numericInput(textInputId, NULL, value = value, min = min, max = max, step = step, width = "60px"),
              if(suffix != "") span(class = "slider-suffix", suffix)
          )
      )
  )
}


ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(type = "module", src = "ballspinapp.es.js"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('pitch_uid', function(selectedUID) {
        window.postMessage({ type: 'pitch_uid', value: selectedUID }, '*');
      });
      Shiny.addCustomMessageHandler('slider_update', function(sliderValues) {
        window.postMessage({ type: 'slider_update', ...sliderValues }, '*');
      });
      Shiny.addCustomMessageHandler('play_toggle', function(val) {
        window.postMessage({ type: 'play_toggle', value: val }, '*');
      });
    ")),
    
    # Custom CSS for horizontal layout
    tags$style(HTML("
      .control-panel {
        transform: scale(0.7);          /* Shrink to 80% size */
        transform-origin: top left;     /* Anchor shrinking from the top-left corner */

        background: #f5f5f5;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 15px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
      
        display: flex;
        flex-direction: column;
        gap: 20px;
        align-items: stretch;
        min-width: 270px;
      }

      
      .control-section {
        display: flex;
        flex-direction: column;
        gap: 15px;
      }
      
      .pitch-controls {
        min-width: 180px;
      }
      
      .spin-controls, .rotation-controls {
        min-width: 160px;
      }
      
      .section-title {
        font-size: 18px;
        font-weight: 600;
        color: #333;
        margin: 0 0 5px 0;
        text-align: center;
        border-bottom: 1px solid #ddd;
        padding-bottom: 8px;
      }
      
      .pitch-select {
        margin-bottom: 0;
      }
      
      .pitch-select .form-group {
        margin-bottom: 0;
      }
      
      .pitch-select select {
        background: white;
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 12px 12px;
        font-size: 14px;
        color: #333;
        width: 100%;
      }
      
      .custom-slider-group {
        margin-bottom: 10px;
      }
      
      .slider-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 3px;
      }
      
      .slider-label {
        font-size: 16px;
        font-weight: 500;
        color: #333;
        font-weight:bold;
      }
      
      .slider-input-container {
        display: flex;
        align-items: center;
        gap: 3px;
      }
      
      .slider-input-container .form-group {
        margin: 0;
      }
      
      .slider-input-container input {
        background: white;
        border: 1px solid #ccc;
        border-radius: 3px;
        padding: 4px 5px;
        font-size: 12px;
        text-align: center;
        color: #333;
        height: 25px;
      }
      
      .slider-suffix {
        font-size: 12px;
        color: #667;
        font-weight: 500;
      }
      
      .slider-container {
        margin-top: 5px;
        position: relative;
      }
      
      /* Customize the actual slider */
      .slider-container .irs {
        height: 15px;
      }
      
      .slider-container .irs-bar {
        background: #005c5c;
        height: 8px;
        top: 50%;
        transform: translateY(-50%);
      }
      
      .slider-container .irs-line {
        background: #ddd;
        height: 4px;
        top: 50%;
        transform: translateY(-50%);
      }
      
      .slider-container .irs-handle {
        background: #ffffff;
        border: 2px solid #005c5c;
        width: 18px;
        height: 18px;
        top: 50%;
        transform: translateY(-50%);
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
      }
      
      .slider-container .irs-handle:hover {
        background: #f9f9f9;
      }
      
      /* Fix slider value display - move it below the slider */
      .slider-container .irs-from,
      .slider-container .irs-to,
      .slider-container .irs-single {
        background: #333;
        color: white;
        font-size: 11px;
        padding: 2px 6px;
        border-radius: 3px;
        top: 25px !important;
      }
      
      /* NEW NE NE */
      .custom-slider-group .slider-row {
        display: flex;
        align-items: center;
        gap: 10px;      /* space between slider and input box */
        margin-bottom: 8px;
      }
      
      .custom-slider-group .slider-container {
        flex: 1 1 0;
        min-width: 200px;  /* controls slider width, adjust as needed */
        max-width: 220px;  /* optional, restrict max width */
      }
      
      .custom-slider-group .slider-input-container {
        display: flex;
        align-items: center;
        gap: 3px;
        margin-left: 4px;  /* space between slider and input */
      }

      .play-button {
        background: #005c5c;
        color: white;
        border: none;
        border-radius: 4px;
        padding: 10px 16px;
        font-size: 14px;
        font-weight: 500;
        cursor: pointer;
        width: 220px;       
        display: block;       
        margin: 0 auto;      
      }


      # .play-button:hover {
      #   background: #004545;
      # }
      # 
      # .play-button:active {
      #   transform: translateY(0);
      # }
      # 
      .play-button,
      .play-button:focus,
      .play-button:active,
      .play-button:visited {
        background: #005c5c !important;
        color: white !important;
        border: none;
        outline: none;
      }
      
      .play-button:hover {
        background: #004545 !important; /* darker teal on hover */
        color: white !important;        /* keep text readable */
      }
      
      .play-button:active {
        background: #003c3c !important; /* slightly darker when pressed */
        color: white !important;
      }

      
      /* Grid and tick styling */
      .slider-container .irs-grid {
        display: block;
      }
      
      .slider-container .irs-grid-text {
        color: #666;
        font-size: 10px;
        top: 25px !important;
      }
      
      .slider-container .irs-grid-pol {
        background: #ccc;
      }
      
      .slider-container .irs-min,
      .slider-container .irs-max {
        color: #666;
        font-size: 12px;
        background: none;
        padding: 2px 0;
        top: 15px !important;
      }
    "))
  ),
  
  tags$div(id = "root", style = "width: 100vw; height: 100vh;"),
  
  absolutePanel(
    top = 20, left = 20, zIndex = 1000, draggable = FALSE,
    class = "control-panel",
    
    # Pitch Controls Section
    div(class = "control-section pitch-controls",
        h5("Select Pitch", class = "section-title"),
        div(class = "pitch-select",
            selectInput("pitch_uid", NULL, choices = pitch_choices, selected = pitch_choices[1])
        ),
        actionButton("play_pause_btn", "Pause", class = "play-button")
    ),
    
    # Spin Axis Section
    div(class = "control-section spin-controls",
        h5("Spin Axis", class = "section-title"),
        customSliderWithInput("spinTilt", "Tilt", min = 0, max = 180, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("spinGyro", "Gyro", min = 0, max = 360, value = 0, step = 1, suffix = "°")
    ),
    
    # Seam Orientation Section
    div(class = "control-section rotation-controls",
        h5("Seam Orientation", class = "section-title"),
        customSliderWithInput("ballX", "Top", min = -180, max = 180, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("ballY", "Front", min = -90, max = 90, value = 0, step = 1, suffix = "°")
    )
  ),
  
  verbatimTextOutput("sliderDebug")  
)

server <- function(input, output, session) {
  # ReactiveValues to indicate playing state
  playing <- reactiveVal(FALSE)
  
  # Function to sync slider and text input
  sync_slider_text <- function(param_name) {
    slider_id <- paste0(param_name, "_slider")
    text_id <- paste0(param_name, "_text")
    
    # When slider changes, update text input
    observeEvent(input[[slider_id]], {
      updateNumericInput(session, text_id, value = input[[slider_id]])
    }, ignoreInit = TRUE)
    
    # When text input changes, update slider
    observeEvent(input[[text_id]], {
      val <- input[[text_id]]
      if (!is.null(val) && !is.na(val)) {
        updateSliderInput(session, slider_id, value = val)
      }
    }, ignoreInit = TRUE)
  }
  
  # Set up synchronization for all parameters
  sync_slider_text("spinTilt")
  sync_slider_text("spinGyro")
  sync_slider_text("ballX")
  sync_slider_text("ballY")
  
  # Function to find pitch data by UID
  get_pitch_by_uid <- function(pitch_uid) {
    row_index <- which(pitch_data$PitchUID == pitch_uid)
    if (length(row_index) == 0) return(NULL)
    return(pitch_data[row_index[1], ])
  }
  
  # Function to calculate and update sliders for a given pitch UID
  update_sliders_for_pitch <- function(pitch_uid) {
    selected_pitch <- get_pitch_by_uid(pitch_uid)
    pitch_number <- selected_pitch$PitchNumber
    
    # spin axis values 
    spin_x <- selected_pitch$spin_backspin
    spin_y <- selected_pitch$spin_sidespin
    spin_z <- -selected_pitch$spin_gyrospin  # keep negative
    
    # Normalize 
    magnitude <- sqrt(spin_x^2 + spin_y^2 + spin_z^2)
    if (magnitude > 0) {
      spin_x <- spin_x / magnitude
      spin_y <- spin_y / magnitude
      spin_z <- spin_z / magnitude
    }
    cat("Normalized spin vector (UID", pitch_uid, "): x =", spin_x, "y =", spin_y, "z =", spin_z, "\n")
    
    # Calculate tilt and gyro - MATCH REACT LOGIC
    tilt <- acos(spin_x) * 180 / pi
    gyro <- atan2(spin_y, spin_z) * 180 / pi
    
    # gyro is in 0-360 range
    gyro <- (gyro + 360) %% 360
    
    cat("Calculated for UID", pitch_uid, ": tilt =", tilt, "gyro =", gyro, "\n")
    
    # Update both sliders and text inputs
    updateSliderInput(session, "spinTilt_slider", value = round(tilt, 1))
    updateNumericInput(session, "spinTilt_text", value = round(tilt, 1))
    updateSliderInput(session, "spinGyro_slider", value = round(gyro, 1))
    updateNumericInput(session, "spinGyro_text", value = round(gyro, 1))
    updateSliderInput(session, "ballX_slider", value = 0)
    updateNumericInput(session, "ballX_text", value = 0)
    updateSliderInput(session, "ballY_slider", value = 0)
    updateNumericInput(session, "ballY_text", value = 0)
  }
  
  # Initialize sliders with first pitch data when app starts
  observe({
    if (length(pitch_data) > 0) {
      first_uid <- pitch_data$PitchUID[1]
      update_sliders_for_pitch(first_uid)
    }
  })
  
  observeEvent(input$pitch_uid, {
    pitch_uid <- input$pitch_uid
    selected_pitch <- get_pitch_by_uid(pitch_uid)
    
    if (!is.null(selected_pitch)) {
      pitch_number <- selected_pitch$PitchNumber
      session$sendCustomMessage("pitch_uid", pitch_uid)
      
      # Update sliders for the selected pitch
      update_sliders_for_pitch(pitch_uid)
      
      # On pitch change, reset playing and UI button
      playing(TRUE)
      updateActionButton(session, "play_pause_btn", label = "Pause")
      session$sendCustomMessage("play_toggle", TRUE)
    }
  })
  
  # Send slider values to JavaScript (using the slider inputs)
  observe({
    vals <- list(
      spinTilt = input$spinTilt_slider,
      spinGyro = input$spinGyro_slider,
      ballX = input$ballX_slider,
      ballY = input$ballY_slider
    )
    
    if (!is.null(vals$spinTilt)) {
      session$sendCustomMessage("slider_update", vals)
    }
  })
  
  # Play/pause button toggling
  observeEvent(input$play_pause_btn, {
    new_state <- !playing()
    playing(new_state)
    updateActionButton(session, "play_pause_btn", label = ifelse(new_state, "Pause", "Play"))
    session$sendCustomMessage("play_toggle", new_state)
  })
}

shinyApp(ui, server)





# # Debug
# output$sliderDebug <- renderPrint({
#   current_pitch_info <- if(!is.null(input$pitch_uid)) {
#     selected_pitch <- get_pitch_by_uid(input$pitch_uid)
#     if (!is.null(selected_pitch)) {
#       list(
#         pitchUID = input$pitch_uid,
#         pitchNumber = selected_pitch$PitchNumber,
#         pitchType = selected_pitch$PitchType
#       )
#     } else {
#       list(pitchUID = input$pitch_uid, status = "not found")
#     }
#   } else {
#     list(status = "no selection")
#   }
#   
#   list(
#     # pitchInfo = current_pitch_info,
#     spinTilt = input$spinTilt,
#     spinGyro = input$spinGyro,
#     ballX = input$ballX,
#     ballY = input$ballY,
#     playing = playing()
#   )
# })
# # Debug



# Get current pitch number for logging
# current_pitch_info <- if(!is.null(input$pitch_uid)) {
#   selected_pitch <- get_pitch_by_uid(input$pitch_uid)
#   if (!is.null(selected_pitch)) {
#     paste0("UID:", input$pitch_uid, " PitchNumber:", selected_pitch$PitchNumber)
#   } else {
#     paste0("UID:", input$pitch_uid, " (not found)")
#   }
# } else {
#   "Unknown"
# }
# cat(" SENDING slider update to React for", current_pitch_info, ":\n")
# cat("  spinTilt =", vals$spinTilt, "\n")
# cat("  spinGyro =", vals$spinGyro, "\n") 
# cat("  ballX =", vals$ballX, "\n")
# cat("  ballY =", vals$ballY, "\n")



##maye  delete - was 74
# Update sliders when React sends initial calculated values
# observeEvent(input$initSpinTilt, {
#   updateSliderInput(session, "spinTilt", value = input$initSpinTilt)
# })
# observeEvent(input$initSpinGyro, {
#   updateSliderInput(session, "spinGyro", value = input$initSpinGyro)
# })
# observeEvent(input$initBallX, {
#   updateSliderInput(session, "ballX", value = input$initBallX)
# })
# observeEvent(input$initBallY, {
#   updateSliderInput(session, "ballY", value = input$initBallY)
# })
# 

# was 84
# cat("Updating sliders for PitchUID:", pitch_uid, "PitchNumber:", pitch_number, "\n")
# cat("Pitch data: backspin =", selected_pitch$spin_backspin,
#     "sidespin =", selected_pitch$spin_sidespin,
#     "gyrospin =", selected_pitch$spin_gyrospin, "\n")