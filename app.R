library(shiny)
library(shinyjs)
library(jsonlite)
library(here)

source("www/helpers.R")

pitch_data <- load_pitch_data()
pitch_labels <- paste0(pitch_data$PitchNumber, " ", pitch_data$PitchType)
pitch_choices <- setNames(pitch_data$PitchUID, pitch_labels)


ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(type = "module", src = "ballspinapp.es.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styling.css"),
    
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
        customSliderWithInput("spinTilt", "Gyro", min = 0, max = 360, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("spinGyro", "Tilt", min = 0, max = 360, value = 0, step = 1, suffix = "°")
    ),
    
    # Seam Orientation Section
    div(class = "control-section rotation-controls",
        h5("Seam Orientation", class = "section-title"),
        customSliderWithInput("ballX", "Top", min = -180, max = 180, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("ballY", "Front", min = -90, max = 90, value = 0, step = 1, suffix = "°")
    )
  ),
  
  verbatimTextOutput("sliderDebug"),
  verbatimTextOutput("gyroDebug")
  
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

    # gyro 0-360 range
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
    cat( "------------- \n")
    cat("  spinTilt =", vals$spinTilt, "\n")
    cat("  spinGyro =", vals$spinGyro, "\n")
    cat("  ballX =", vals$ballX, "\n")
    cat("  ballY =", vals$ballY, "\n")
    
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
  
  output$gyroDebug <- renderPrint({
    pitch_uid <- input$pitch_uid
    selected_pitch <- get_pitch_by_uid(pitch_uid)
    
    if (is.null(selected_pitch)) return(NULL)
    
    wrap_signed <- function(angle) {
      ((angle + 180) %% 360) - 180
    }
    
    raw_val <- selected_pitch$spin_gyrospin
    wrapped_val <- wrap_signed(raw_val)
    
    sidespin <- selected_pitch$spin_sidespin
    side_wrapped_val <- wrap_signed(sidespin)
    
    backspin <- selected_pitch$spin_backspin
    back_wrapped_val <- wrap_signed(backspin)
    
    list(
      # raw_spin_gyrospin = raw_val,
      wrapped_spin_gyrospin = wrapped_val,
      # raw_spin_sidespin = sidespin,
      wrapped_spin_sidespin = side_wrapped_val,
      # raw_spin_backspin = backspin,
      wrapped_spin_backspin = back_wrapped_val
    )
  })
}
  

shinyApp(ui, server)


