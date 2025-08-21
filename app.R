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
        h5("Rotation Axis", class = "section-title"),
        # customSliderWithInput("spinTilt", "Tilt", min = 0, max = 360, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("spinTilt", "Tilt", min = 0, max = 360, value = 0, step = 1, suffix = "°", show_time = TRUE),
        customSliderWithInput("spinGyro", "Gyro", min = -90, max = 90, value = 0, step = 1, suffix = "°")
    ),
    
    # Seam Orientation Section
    div(class = "control-section rotation-controls",
        h5("Seam Orientation", class = "section-title"),
        customSliderWithInput("ballX", "Top", min = -180, max = 180, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("ballY", "Front", min = -90, max = 90, value = 0, step = 1, suffix = "°")
    )
  ),
  

)



server <- function(input, output, session) {
  # ReactiveValues to indicate playing state
  playing <- reactiveVal(FALSE)
  original_tilt <- reactiveVal(0)
  
  # }
  sync_slider_text <- function(param_name, has_time = FALSE) {
    slider_id <- paste0(param_name, "_slider")
    text_id <- paste0(param_name, "_text")  
    time_id <- paste0(param_name, "_time")
    
    # Make slider the single source of truth
    # Only update other inputs when slider changes
    observeEvent(input[[slider_id]], {
      new_value <- input[[slider_id]]
      
      # Update text (allow some tolerance for floating point)
      if (abs(input[[text_id]] - new_value) > 0.01) {
        updateNumericInput(session, text_id, value = new_value)
      }
      
      if (has_time) {
        time_vals <- axis_to_tilt_time_simple(new_value)
        time_string <- format_time(time_vals$hours, time_vals$minutes)
        updateTextInput(session, time_id, value = time_string)
      }
    }, ignoreInit = TRUE)
    
    # Text and time inputs update the slider, but don't update each other
    observeEvent(input[[text_id]], {
      val <- input[[text_id]]
      if (!is.null(val) && !is.na(val) && abs(input[[slider_id]] - val) > 0.01) {
        updateSliderInput(session, slider_id, value = val)
      }
    }, ignoreInit = TRUE)
    
    if (has_time) {
      observeEvent(input[[time_id]], {
        time_string <- input[[time_id]]
        if (!is.null(time_string) && nchar(time_string) > 0) {
          parsed_time <- parse_time(time_string)
          if (!is.null(parsed_time)) {
            degrees <- tilt_time_to_axis_deg_simple(parsed_time$hours, parsed_time$minutes)
            if (abs(input[[slider_id]] - degrees) > 0.01) {
              updateSliderInput(session, slider_id, value = degrees)
            }
          }
        }
      }, ignoreInit = TRUE)
    }
  }
  
  # Set up synchronization for all parameters
  sync_slider_text("spinTilt", has_time = TRUE)
  sync_slider_text("spinGyro")
  sync_slider_text("ballX")
  sync_slider_text("ballY")
  
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
  # Function to find pitch data by UID
  get_pitch_by_uid <- function(pitch_uid) {
    row_index <- which(pitch_data$PitchUID == pitch_uid)
    if (length(row_index) == 0) return(NULL)
    return(pitch_data[row_index[1], ])
  }
  
  # Function to calculate tilt and gyro from spin vector (for initialization only)
 
  calculate_tilt_gyro_from_vector <- function(spin_x, spin_y, spin_z) {
    # Normalize the vector
    magnitude <- sqrt(spin_x^2 + spin_y^2 + spin_z^2)
    if (magnitude > 0) {
      spin_x <- spin_x / magnitude
      spin_y <- spin_y / magnitude
      spin_z <- spin_z / magnitude
    }
    
    # TILT: angle in XY plane from X-axis toward Y-axis
    tilt <- atan2(spin_y, spin_x) * 180 / pi
    # Convert from arrow direction to spin direction by subtracting 90°
    tilt <- tilt - 90
    # Ensure tilt is in 0-360 range
    tilt <- (tilt + 360) %% 360
    
    # GYRO: elevation angle from XY plane toward Z-axis (unchanged)
    xy_magnitude <- sqrt(spin_x^2 + spin_y^2)
    if (xy_magnitude > 0) {
      gyro <- atan2(spin_z, xy_magnitude) * 180 / pi
    } else {
      gyro <- ifelse(spin_z > 0, 90, -90)
    }
    gyro <- max(-90, min(90, gyro))
    
    return(list(tilt = tilt, gyro = gyro))
  }
  # 
  # # # Function to calculate spin vector from tilt and gyro (for slider updates)
  calculate_vector_from_tilt_gyro <- function(tilt_deg, gyro_deg) {
    # Convert displayed spin direction back to arrow direction
    arrow_tilt_deg <- tilt_deg +90

    tilt_rad <- arrow_tilt_deg * pi / 180
    gyro_rad <- gyro_deg * pi / 180

    # Calculate components (rest unchanged)
    xy_magnitude <- cos(gyro_rad)
    x <- xy_magnitude * cos(tilt_rad)
    y <- xy_magnitude * sin(tilt_rad)
    z <- sin(gyro_rad)

    return(c(x, y, z))
  }

  
  # Updated function to calculate and update sliders for a given pitch UID
  update_sliders_for_pitch <- function(pitch_uid) {
    selected_pitch <- get_pitch_by_uid(pitch_uid)
    
    if (is.null(selected_pitch)) return()
    
    # Get original spin components
    spin_x <- selected_pitch$spin_backspin
    spin_y <- selected_pitch$spin_sidespin
    spin_z <- -selected_pitch$spin_gyrospin  # keep negative
    
    # Calculate initial tilt and gyro from the pitch data
    tilt_gyro <- calculate_tilt_gyro_from_vector(spin_x, spin_y, spin_z)
    
    # STORE the original tilt value (internal)
    original_tilt(tilt_gyro$tilt)
    
    # INVERT and ADD 90° offset so slider matches visual rod position
    display_tilt <- (360 - tilt_gyro$tilt + 90) %% 360
    
    cat("DEBUG - Raw spin data: backspin =", spin_x, "sidespin =", spin_y, "gyrospin =", spin_z, "\n")
    cat("Original internal tilt:", tilt_gyro$tilt, "Display tilt:", display_tilt, "\n")
    
    # Update sliders with DISPLAY values (what user expects to see)
    updateSliderInput(session, "spinTilt_slider", value = round(display_tilt, 1))
    updateNumericInput(session, "spinTilt_text", value = round(display_tilt, 1))
    updateSliderInput(session, "spinGyro_slider", value = round(tilt_gyro$gyro, 1))
    updateNumericInput(session, "spinGyro_text", value = round(tilt_gyro$gyro, 1))
    
    # Reset user adjustments
    updateSliderInput(session, "ballX_slider", value = 0)
    updateNumericInput(session, "ballX_text", value = 0)
    updateSliderInput(session, "ballY_slider", value = 0)
    updateNumericInput(session, "ballY_text", value = 0)
  }
  
  # Updated observer for slider values
  observe({
    vals <- list(
      spinTilt = input$spinTilt_slider,
      spinGyro = input$spinGyro_slider,
      ballX = input$ballX_slider,
      ballY = input$ballY_slider
    )
    
    # Calculate the new spin vector from tilt and gyro
    if (!is.null(vals$spinTilt) && !is.null(vals$spinGyro) && !is.null(original_tilt())) {
      
      # Convert user's display tilt back to internal tilt for calculation
      user_display_tilt <- vals$spinTilt
      # Reverse the transformation: subtract 90° offset then invert
      actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
      
      cat("------------- \n")
      cat("User display tilt:", user_display_tilt, "Converted to internal tilt:", actual_tilt, "\n")
      cat("Original internal tilt was:", original_tilt(), "\n")
      
      # Use the converted tilt for vector calculation
      new_vector <- calculate_vector_from_tilt_gyro(actual_tilt, vals$spinGyro)
      
      cat("Calculated spin vector: x =", new_vector[1], "y =", new_vector[2], "z =", new_vector[3], "\n")
      cat("ballX =", vals$ballX, "ballY =", vals$ballY, "\n")
      
      # Send the calculated vector components along with the slider values
      vals$spinVectorX <- new_vector[1]
      vals$spinVectorY <- new_vector[2]
      vals$spinVectorZ <- new_vector[3]
      
      session$sendCustomMessage("slider_update", vals)
    }
  })
  
  # # Play/pause button toggling
  observeEvent(input$play_pause_btn, {
    new_state <- !playing()
    playing(new_state)
    updateActionButton(session, "play_pause_btn", label = ifelse(new_state, "Pause", "Play"))
    session$sendCustomMessage("play_toggle", new_state)
  })
  
}
  

shinyApp(ui, server)

