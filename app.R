library(shiny)
library(shinyjs)
library(jsonlite)
library(here)

source("www/helpers.R")

pitch_data <- load_pitch_data()

abbrevs <- list(
  "CH" = "Changeup",
  "SI" = "2-Seam Fastball",
  "FA" = "4-Seam Fastball",
  "CU" = "Curveball",
  "SW" = "Sweeper",
  "SL" = "Slider",
  "FC" = "Cutter",
  "FS" = "Splitter"
)
  
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
      Shiny.addCustomMessageHandler('updateStatCard', function(data) {
        var element = document.getElementById(data.id);
        if (element) {
          element.textContent = data.value;
        }
      });
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
            selectInput("pitcher", "Pitcher:",
                        choices = unique(pitch_data$Pitcher),
                        selected = unique(pitch_data$Pitcher)[1]),
            uiOutput("pitchTypeUI")
        ),
        actionButton("play_pause_btn", "Pause", class = "play-button")
    ),
    
    # Spin Axis Section
    div(class = "control-section spin-controls",
        h5("Axis of Rotation", class = "section-title"),
        customSliderWithInput("spinGyro", "Gyro", min = -90, max = 90, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("spinTilt", "Spin Axis", min = 0, max = 360, value = 0, step = 1, suffix = "°", show_time = TRUE)

    ),
    
    # Seam Orientation Section
    div(class = "control-section rotation-controls",
        h5("Seam Orientation", class = "section-title"),
        customSliderWithInput("ballX", "Front", min = -180, max = 180, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("ballY", "Top", min = -90, max = 90, value = 0, step = 1, suffix = "°")
    ),
  ),

  # Stats panel
    absolutePanel(
      top = 30, right = 20, zIndex = 1000, draggable = FALSE,
      class = "stats-panel",
      
      div(class = "stats-section",
          h5("Pitch Statistics", class = "section-title"),
          div(class = "stats-cards",
              div(class = "stat-card",
                  div(class = "stat-label", "Velo"),
                  div(class = "stat-value", id = "velo_value", "0.0 mph")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Spin Rate"),
                  div(class = "stat-value", id = "spin_rate_value", "0 rpm")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Induced Vert Break"),
                  div(class = "stat-value", id = "vert_break_value", "0.0\"")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Horz Break"),
                  div(class = "stat-value", id = "horz_break_value", "0.0\"")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Inferred Spin Direction"),
                  div(class = "stat-value", id = "spin_direction_value", "0°")
              )
          )
      )
      
      
      
  )
)


server <- function(input, output, session) {
  playing <- reactiveVal(FALSE)
  original_tilt <- reactiveVal(0)
  current_pitch <- reactiveVal(NULL)
  
  
  output$pitchTypeUI <- renderUI({
    req(input$pitcher)
    
    pitch_rows <- pitch_data[pitch_data$Pitcher == input$pitcher, ]
    
    # Map abbreviations to full names
    pitch_labels <- sapply(pitch_rows$PitchType, function(pt) abbrevs[[pt]])
    
    div(style = "margin-top: 10px;",   
        selectInput("pitch_type", "Pitch Type:",
                    choices = setNames(pitch_rows$PitchUID, pitch_labels),
                    selected = pitch_rows$PitchUID[1])
    )
  })
  

  # Update individual stat values for card display
  observe({
    pitch <- current_pitch()

    if (!is.null(pitch)) {
      tilt <- axis_to_tilt_time_simple(pitch$SpinAxis_inf)
      session$sendCustomMessage("updateStatCard", list(
        id = "velo_value", 
        value = paste0(round(pitch$start_speed, 1), " mph")
      ))
      session$sendCustomMessage("updateStatCard", list(
        id = "spin_rate_value", 
        value = paste0(round(pitch$SpinRate, 1), " rpm")
      ))
      session$sendCustomMessage("updateStatCard", list(
        id = "vert_break_value", 
        value = paste0(round(pitch$break_z,1), "\"")
      ))
      session$sendCustomMessage("updateStatCard", list(
        id = "horz_break_value", 
        value = paste0(round(pitch$break_x, 1), "\"")
      ))
      session$sendCustomMessage("updateStatCard", list(
        id = "spin_direction_value", 
        value = paste0(tilt$hours, ":", sprintf("%02d", round(tilt$minutes)))
      ))
    }
  })
  
  
  # Helper to sync sliders with numeric inputs
  sync_slider_text <- function(param_name, has_time = FALSE) {
    slider_id <- paste0(param_name, "_slider")
    text_id <- paste0(param_name, "_text")  
    time_id <- paste0(param_name, "_time")
    
    observeEvent(input[[slider_id]], {
      new_value <- input[[slider_id]]
      if (abs(input[[text_id]] - new_value) > 0.01) {
        updateNumericInput(session, text_id, value = new_value)
      }
      if (has_time) {
        time_vals <- axis_to_tilt_time_simple(new_value)
        time_string <- format_time(time_vals$hours, time_vals$minutes)
        updateTextInput(session, time_id, value = time_string)
      }
    }, ignoreInit = TRUE)
    
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
  
  sync_slider_text("spinTilt", has_time = TRUE)
  sync_slider_text("spinGyro")
  sync_slider_text("ballX")
  sync_slider_text("ballY")
  
  # Initialize sliders with first pitch data
  observe({
    if (nrow(pitch_data) > 0) {
      first_uid <- pitch_data$PitchUID[1]
      update_sliders_for_pitch(first_uid)
    }
  })
  
  # Handle pitch selection
  observeEvent(input$pitch_type, {
    req(input$pitch_type)
    pitch_uid <- input$pitch_type
    selected_pitch <- get_pitch_by_uid(pitch_uid)
    
    if (!is.null(selected_pitch)) {
      current_pitch(selected_pitch)
      session$sendCustomMessage("pitch_uid", pitch_uid)
      update_sliders_for_pitch(pitch_uid)
      playing(TRUE)
      updateActionButton(session, "play_pause_btn", label = "Pause")
      session$sendCustomMessage("play_toggle", TRUE)
    }
  })
  
  # Send slider values to JS
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
  
  # Helpers
  get_pitch_by_uid <- function(pitch_uid) {
    row_index <- which(pitch_data$PitchUID == pitch_uid)
    if (length(row_index) == 0) return(NULL)
    return(pitch_data[row_index[1], ])
  }
  
  calculate_tilt_gyro_from_vector <- function(spin_x, spin_y, spin_z) {
    magnitude <- sqrt(spin_x^2 + spin_y^2 + spin_z^2)
    if (magnitude > 0) {
      spin_x <- spin_x / magnitude
      spin_y <- spin_y / magnitude
      spin_z <- spin_z / magnitude
    }
    tilt <- atan2(spin_y, spin_x) * 180 / pi
    tilt <- (tilt - 90 + 360) %% 360
    xy_magnitude <- sqrt(spin_x^2 + spin_y^2)
    gyro <- if (xy_magnitude > 0) atan2(spin_z, xy_magnitude) * 180 / pi else ifelse(spin_z > 0, 90, -90)
    gyro <- max(-90, min(90, gyro))
    return(list(tilt = tilt, gyro = gyro))
  }
  
  calculate_vector_from_tilt_gyro <- function(tilt_deg, gyro_deg) {
    arrow_tilt_deg <- tilt_deg + 90
    tilt_rad <- arrow_tilt_deg * pi / 180
    gyro_rad <- gyro_deg * pi / 180
    xy_magnitude <- cos(gyro_rad)
    x <- xy_magnitude * cos(tilt_rad)
    y <- xy_magnitude * sin(tilt_rad)
    z <- sin(gyro_rad)
    return(c(x, y, z))
  }
  
  update_sliders_for_pitch <- function(pitch_uid) {
    selected_pitch <- get_pitch_by_uid(pitch_uid)
    if (is.null(selected_pitch)) return()
    
    current_pitch(selected_pitch)
    
    spin_x <- selected_pitch$spin_backspin
    spin_y <- selected_pitch$spin_sidespin
    spin_z <- -selected_pitch$spin_gyrospin
    tilt_gyro <- calculate_tilt_gyro_from_vector(spin_x, spin_y, spin_z)
    original_tilt(tilt_gyro$tilt)
    display_tilt <- (360 - tilt_gyro$tilt + 90) %% 360
    updateSliderInput(session, "spinTilt_slider", value = round(display_tilt, 1))
    updateNumericInput(session, "spinTilt_text", value = round(display_tilt, 1))
    updateSliderInput(session, "spinGyro_slider", value = round(tilt_gyro$gyro, 1))
    updateNumericInput(session, "spinGyro_text", value = round(tilt_gyro$gyro, 1))
    updateSliderInput(session, "ballX_slider", value = 0)
    updateNumericInput(session, "ballX_text", value = 0)
    updateSliderInput(session, "ballY_slider", value = 0)
    updateNumericInput(session, "ballY_text", value = 0)
  }
  
  # Tilt + gyro updates
  observe({
    vals <- list(
      spinTilt = input$spinTilt_slider,
      spinGyro = input$spinGyro_slider,
      ballX = input$ballX_slider,
      ballY = input$ballY_slider
    )
    if (!is.null(vals$spinTilt) && !is.null(vals$spinGyro) && !is.null(original_tilt())) {
      user_display_tilt <- vals$spinTilt
      actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
      new_vector <- calculate_vector_from_tilt_gyro(actual_tilt, vals$spinGyro)
      vals$spinVectorX <- new_vector[1]
      vals$spinVectorY <- new_vector[2]
      vals$spinVectorZ <- new_vector[3]
      session$sendCustomMessage("slider_update", vals)
    }
  })
  
  # Play/pause button
  observeEvent(input$play_pause_btn, {
    new_state <- !playing()
    playing(new_state)
    updateActionButton(session, "play_pause_btn", label = ifelse(new_state, "Pause", "Play"))
    session$sendCustomMessage("play_toggle", new_state)
  })
}

shinyApp(ui, server)
