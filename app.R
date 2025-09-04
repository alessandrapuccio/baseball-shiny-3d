library(shiny)
library(shinyjs)
library(jsonlite)
library(here)

source("www/helpers.R")

pitch_data <<- load_pitch_data()

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
      Shiny.addCustomMessageHandler('reset_spin_rotation', function(val) {
        window.postMessage({ type: 'reset_spin_rotation', value: val }, '*');
      });
      Shiny.addCustomMessageHandler('clock_toggle', function(showClock) {
        window.postMessage({ type: 'clock_toggle', value: showClock }, '*');
      });
      Shiny.addCustomMessageHandler('field_toggle', function(showField) {
        window.postMessage({ type: 'field_toggle', value: showField }, '*');
      });
    ")),
    
  ),
  
  tags$div(id = "root", style = "width: 100vw; height: 100vh;"),
  
  absolutePanel(
    top = 20, left = 25, zIndex = 1000, draggable = FALSE,
    class = "control-panel",
    
    # Pitch Controls Section
    div(class = "control-section pitch-controls",
        h5("SELECT PITCH", class = "section-title"),
        div(class = "pitch-select",
            selectInput("pitcher", "Select Pitcher and Pitch Type:",
                        choices = unique(pitch_data$Pitcher),
                        selected = unique(pitch_data$Pitcher)[1]),
            uiOutput("pitchTypeUI")
        ),
        div(class = "button-group",
            actionButton("play_pause_btn", "Play", class = "play-button"),
            actionButton("reset_btn", "Reset", class = "reset-button")
        )
    ),
    
    
    # Spin Axis Section
    div(class = "control-section spin-controls",
        h5("ADJUST AXIS OF ROTATION", class = "section-title"),
        customSliderWithInput("spinGyro", "Gyro", min = -90, max = 90, value = 0, step = 1, suffix = "°"),
        customSpinAxisSlider("spinTilt", "Spin Axis", min = 0, max = 360, value = 0, step = 1, suffix = "°", show_slider = FALSE, show_time = TRUE)        
    ),
    
    # show clock button
    div(class = "compact-checkbox", style = "text-align: center;",
        checkboxInput("show_clock", "Show Clock", value = TRUE),
        checkboxInput("show_field", "Show Field", value = TRUE)
        
    ),
    
    
    div(class = "control-section rotation-controls",
        h5("ADJUST SEAM ORIENTATION", class = "section-title"),
        div(style = "text-align: center;",
            actionButton("pause_for_orientation", "Press Before Reorientation", 
                         class = "pause-orientation-button", 
                         style = "width: 85%;")
        ),
        customSliderWithInput("ballX", "Longitude / Top", min = -180, max = 180, value = 0, step = 1, suffix = "°"),
        customSliderWithInput("ballY", "Latitude / Front", min = -90, max = 90, value = 0, step = 1, suffix = "°")
    )
  ),
  
  
  # Stats panel - now fully reactive
  absolutePanel(
    top = 30, right = 20, zIndex = 1000, draggable = FALSE,
    class = "stats-panel",
    
    div(class = "stats-section",
        h5(textOutput("stats_title", inline = TRUE), class = "section-title"),
        div(class = "stats-cards",
            div(class = "stat-card",
                div(class = "stat-label", "Velo"),
                div(class = "stat-value", textOutput("velo_value", inline = TRUE))
            ),
            div(class = "stat-card",
                div(class = "stat-label", "Spin Rate"),
                div(class = "stat-value", textOutput("spin_rate_value", inline = TRUE))
            ),
            div(class = "stat-card",
                div(class = "stat-label", "Induced Vert Break"),
                div(class = "stat-value", textOutput("vert_break_value", inline = TRUE))
            ),
            div(class = "stat-card",
                div(class = "stat-label", "Horz Break"),
                div(class = "stat-value", textOutput("horz_break_value", inline = TRUE))
            ),
            div(class = "stat-card",
                div(class = "stat-label", "Inferred Spin Direction"),
                div(class = "stat-value", textOutput("spin_direction_value", inline = TRUE))
            ),
            div(class = "stat-card",
                div(class = "stat-label", "Spin Efficiency"),
                div(class = "stat-value", textOutput("spin_efficiency_value", inline = TRUE))
            )
        )
    )
  )
)


server <- function(input, output, session) {
  playing <- reactiveVal(FALSE)
  original_tilt <- reactiveVal(0)
  current_pitch <- reactiveVal(NULL)
  spinTilt_timer <- reactiveTimer(Inf) 
  
  
  ## expected pitch shape
  user_modified <- reactiveVal(FALSE)
  original_values <- reactiveVal(NULL)
  resetting <- reactiveVal(FALSE)  # so it doesnt detect the changes happening when reset is hit and go back to expected panel 
  
  
  calculate_expected_vert_break <- function(spin_vector_x, spin_vector_y, spin_vector_z, ball_x, ball_y, pitch_data) {
    return("")
  }
  calculate_expected_horz_break <- function(spin_vector_x, spin_vector_y, spin_vector_z, ball_x, ball_y, pitch_data) {
    return("")
  }
  calculate_expected_spin_direction <- function(spin_vector_x, spin_vector_y, spin_vector_z, ball_x, ball_y, pitch_data) {
    return("")
  }
  calculate_expected_spin_efficiency <- function(spin_vector_x, spin_vector_y, spin_vector_z, ball_x, ball_y, pitch_data) {
    return("")
  }  
  
  
  output$pitchTypeUI <- renderUI({
    req(input$pitcher != "")
    
    pitch_rows <- pitch_data[pitch_data$Pitcher == input$pitcher, ]
    
    # Map abbreviations to full names
    pitch_labels <- sapply(pitch_rows$PitchType, function(pt) abbrevs[[pt]])
    
    div(style = "margin-top: 10px;",   
        selectInput("pitch_type", NULL,#"Pitch Type:",
                    choices = setNames(pitch_rows$PitchUID, pitch_labels),
                    selected = pitch_rows$PitchUID[1])
    )
  })
  
  
  # Reactive stats title
  output$stats_title <- renderText({
    if (user_modified()) {
      "Expected Pitch Shape"
    } else {
      "Pitch Shape"
    }
  })
  
  # Reactive stat values 
  output$velo_value <- renderText({
    pitch <- current_pitch()
    if (!is.null(pitch)) {
      paste0(round(pitch$start_speed, 1), " mph")
    } else {
      "0.0 mph"
    }
  })
  
  output$spin_rate_value <- renderText({
    pitch <- current_pitch()
    if (!is.null(pitch)) {
      paste0(round(pitch$SpinRate, 1), " rpm")
    } else {
      "0 rpm"
    }
  })
  
  output$vert_break_value <- renderText({
    pitch <- current_pitch()
    
    if (!is.null(pitch)) {
      if (user_modified() && !is.null(input$spinTilt_slider) && !is.null(input$spinGyro_slider) && 
          !is.null(input$ballX_slider) && !is.null(input$ballY_slider)) {
        # USER HAS MODIFIED VALUES - SHOW EXPECTED
        user_display_tilt <- input$spinTilt_slider
        actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
        spin_vector <- calculate_vector_from_tilt_gyro(actual_tilt, input$spinGyro_slider)
        
        exp_vert <- calculate_expected_vert_break(spin_vector[1], spin_vector[2], spin_vector[3], 
                                                  input$ballX_slider, input$ballY_slider, pitch)
        exp_vert
      } else {
        # SHOW ORIGINAL
        paste0(round(pitch$break_z, 1), "\"")
      }
    } else {
      "0.0\""
    }
  })
  
  output$horz_break_value <- renderText({
    pitch <- current_pitch()
    
    if (!is.null(pitch)) {
      if (user_modified() && !is.null(input$spinTilt_slider) && !is.null(input$spinGyro_slider) && 
          !is.null(input$ballX_slider) && !is.null(input$ballY_slider)) {
        # USER HAS MODIFIED VALUES - SHOW EXPECTED
        user_display_tilt <- input$spinTilt_slider
        actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
        spin_vector <- calculate_vector_from_tilt_gyro(actual_tilt, input$spinGyro_slider)
        
        exp_horz <- calculate_expected_horz_break(spin_vector[1], spin_vector[2], spin_vector[3], 
                                                  input$ballX_slider, input$ballY_slider, pitch)
        exp_horz
      } else {
        # SHOW ORIGINAL
        paste0(round(pitch$break_x, 1), "\"")
      }
    } else {
      "0.0\""
    }
  })
  
  output$spin_direction_value <- renderText({
    pitch <- current_pitch()
    
    if (!is.null(pitch)) {
      if (user_modified() && !is.null(input$spinTilt_slider) && !is.null(input$spinGyro_slider) && 
          !is.null(input$ballX_slider) && !is.null(input$ballY_slider)) {
        # USER HAS MODIFIED VALUES - SHOW EXPECTED
        user_display_tilt <- input$spinTilt_slider
        actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
        spin_vector <- calculate_vector_from_tilt_gyro(actual_tilt, input$spinGyro_slider)
        
        exp_spin_dir <- calculate_expected_spin_direction(spin_vector[1], spin_vector[2], spin_vector[3], 
                                                          input$ballX_slider, input$ballY_slider, pitch)
        exp_spin_dir
      } else {
        # SHOW ORIGINAL
        tilt <- axis_to_tilt_time_simple(pitch$SpinAxis_inf)
        paste0(tilt$hours, ":", sprintf("%02d", round(tilt$minutes)))
      }
    } else {
      "0°"
    }
  })
  
  output$spin_efficiency_value <- renderText({
    pitch <- current_pitch()
    
    if (!is.null(pitch)) {
      if (user_modified() && !is.null(input$spinTilt_slider) && !is.null(input$spinGyro_slider) && 
          !is.null(input$ballX_slider) && !is.null(input$ballY_slider)) {
        # USER HAS MODIFIED VALUES - SHOW EXPECTED
        user_display_tilt <- input$spinTilt_slider
        actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
        spin_vector <- calculate_vector_from_tilt_gyro(actual_tilt, input$spinGyro_slider)
        
        exp_spin_eff <- calculate_expected_spin_efficiency(spin_vector[1], spin_vector[2], spin_vector[3], 
                                                           input$ballX_slider, input$ballY_slider, pitch)
        exp_spin_eff
      } else {
        # SHOW ORIGINAL
        paste0(round((pitch$spin_efficiency)*100, 1), "%")
      }
    } else {
      "NA"
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
    
    if (has_time && param_name == "spinTilt") {
      observeEvent(input[[time_id]], {
        time_string <- input[[time_id]]
        if (!is.null(time_string) && nchar(time_string) > 0) {
          
          # Check if hour part is missing or invalid
          parts <- strsplit(time_string, ":")[[1]]
          hour_part <- if(length(parts) > 0) parts[1] else ""
          
          if (is.na(suppressWarnings(as.numeric(hour_part))) || nchar(trimws(hour_part)) == 0) {
            # Hour is missing or invalid, start the timer
            spinTilt_timer <<- reactiveTimer(500, session)  # 0.5 second timer
          } else {
            # Valid input, process normally
            parsed_time <- parse_time(time_string)
            if (!is.null(parsed_time)) {
              degrees <- tilt_time_to_axis_deg_simple(parsed_time$hours, parsed_time$minutes)
              if (abs(input[[slider_id]] - degrees) > 0.01) {
                updateSliderInput(session, slider_id, value = degrees)
              }
            }
          }
        }
      }, ignoreInit = TRUE)
      
      # Timer observer to apply fallback after delay
      observeEvent(spinTilt_timer(), {
        current_value <- input[[time_id]]
        if (!is.null(current_value)) {
          parts <- strsplit(current_value, ":")[[1]]
          hour_part <- if(length(parts) > 0) parts[1] else ""
          
          if (is.na(suppressWarnings(as.numeric(hour_part))) || nchar(trimws(hour_part)) == 0) {
            # Apply fallback: set hour to 0, keep minutes if valid
            minute_part <- if(length(parts) > 1) parts[2] else "00"
            if (is.na(suppressWarnings(as.numeric(minute_part)))) minute_part <- "00"
            
            new_time_string <- paste0("0:", minute_part)
            updateTextInput(session, time_id, value = new_time_string)
            
            # Update slider accordingly
            parsed_time <- parse_time(new_time_string)
            if (!is.null(parsed_time)) {
              degrees <- tilt_time_to_axis_deg_simple(parsed_time$hours, parsed_time$minutes)
              updateSliderInput(session, slider_id, value = degrees)
            }
          }
          
          # Deactivate timer
          spinTilt_timer <<- reactiveTimer(Inf, session)
        }
      }, ignoreInit = TRUE)
    }
  }
  
  sync_slider_text("spinTilt", has_time = TRUE)
  sync_slider_text("spinGyro")
  sync_slider_text("ballX")
  sync_slider_text("ballY")
  
  # expected pitch shape - USER MODIFICATION DETECTORS
  observe({
    if (resetting()) return()
    # Only proceed if we have original values stored AND all sliders exist
    req(original_values())
    req(input$spinTilt_slider, input$spinGyro_slider, input$ballX_slider, input$ballY_slider)
    
    orig_vals <- original_values()
    # Make sure original values have all required fields
    if (is.null(orig_vals$spinTilt) || is.null(orig_vals$spinGyro) || 
        is.null(orig_vals$ballX) || is.null(orig_vals$ballY)) {
      return()
    }
    
    current_vals <- list(
      spinTilt = input$spinTilt_slider,
      spinGyro = input$spinGyro_slider,
      ballX = input$ballX_slider,
      ballY = input$ballY_slider
    )
    
    # Check if any value has changed from original (with tolerance for rounding)
    changed <- abs(current_vals$spinTilt - orig_vals$spinTilt) > 0.51 ||
      abs(current_vals$spinGyro - orig_vals$spinGyro) > 0.51 ||
      abs(current_vals$ballX - orig_vals$ballX) > 0.51 ||
      abs(current_vals$ballY - orig_vals$ballY) > 0.51
    
    if (changed && !user_modified()) {
      user_modified(TRUE)
    }
  })
  
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
      
      # Reset user modification flag on pitch change
      shinyjs::delay(900, {
        user_modified(FALSE)
      })
      
      playing(FALSE)
      updateActionButton(session, "play_pause_btn", label = "Play")
      session$sendCustomMessage("play_toggle", FALSE)
    }
  })
  
  
  # Handle clock and field visibility checkboxes
  observeEvent(input$show_clock, {
    session$sendCustomMessage("clock_toggle", input$show_clock)
  })
  observeEvent(input$show_field, {
    session$sendCustomMessage("field_toggle", input$show_field)
  })
  
  # Handle reset button
  observeEvent(input$reset_btn, {
    req(input$pitch_type)
    pitch_uid <- input$pitch_type
    update_sliders_for_pitch(pitch_uid)
    
    # Force the modification flag to FALSE after reset
    shinyjs::delay(950, {
      user_modified(FALSE)
    })
  })
  
  update_sliders_for_pitch <- function(pitch_uid) {
    resetting(TRUE)
    
    selected_pitch <- get_pitch_by_uid(pitch_uid)
    if (is.null(selected_pitch)) return()
    
    current_pitch(selected_pitch)
    
    spin_x <- selected_pitch$spin_backspin
    spin_y <- selected_pitch$spin_sidespin
    spin_z <- -selected_pitch$spin_gyrospin
    tilt_gyro <- calculate_tilt_gyro_from_vector(spin_x, spin_y, spin_z)
    original_tilt(tilt_gyro$tilt)
    display_tilt <- (360 - tilt_gyro$tilt + 90) %% 360
    
    seam_lat <- if(!is.null(selected_pitch$seam_orientation_lat) && !is.na(selected_pitch$seam_orientation_lat)) {
      selected_pitch$seam_orientation_lat
    } else {
      0
    }
    seam_lon <- if(!is.null(selected_pitch$seam_orientation_lon) && !is.na(selected_pitch$seam_orientation_lon)) {
      selected_pitch$seam_orientation_lon
    } else {
      0
    }
    display_lon <- seam_lon
    display_lat <- seam_lat
    
    # STORE ORIGINAL VALUES (ROUNDED AS THEY APPEAR ON SLIDERS)
    original_values(list(
      spinTilt = round(display_tilt, 1),
      spinGyro = round(tilt_gyro$gyro, 1),
      ballX = round(display_lon, 1),
      ballY = round(display_lat, 1)
    ))
    
    # RESET USER MODIFICATION FLAG
    user_modified(FALSE)
    
    # Update spin controls
    updateSliderInput(session, "spinTilt_slider", value = round(display_tilt, 1))
    updateNumericInput(session, "spinTilt_text", value = round(display_tilt, 1))
    updateSliderInput(session, "spinGyro_slider", value = round(tilt_gyro$gyro, 1))
    updateNumericInput(session, "spinGyro_text", value = round(tilt_gyro$gyro, 1))
    
    updateSliderInput(session, "ballX_slider", value = round(display_lon, 1))
    updateNumericInput(session, "ballX_text", value = round(display_lon, 1))
    updateSliderInput(session, "ballY_slider", value = round(display_lat, 1))
    updateNumericInput(session, "ballY_text", value = round(display_lat, 1))
    
    
    # small delay to ensure all slider updates are complete before re-enabling change detection that would flip shape panel back to expected
    reset_timer <- reactiveTimer(950, session)
    observe({
      reset_timer()
      resetting(FALSE)
    })
  }
  
  # Tilt + gyro updates
  observe({
    vals <- list(
      spinTilt = input$spinTilt_slider,
      spinGyro = input$spinGyro_slider,
      ballX = input$ballX_slider,#((input$ballX_slider - 90 + 180) %% 360) - 180,  # Convert display longitude back to data longitude with wrapping
      ballY = input$ballY_slider#-input$ballY_slider       # Convert display latitude back to data latitude
      
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
  
  
  # Handle "Pause for orientation" button
  observeEvent(input$pause_for_orientation, {
    # Pause the animation
    playing(FALSE)
    updateActionButton(session, "play_pause_btn", label = "Play")
    session$sendCustomMessage("play_toggle", FALSE)
    
    # Reset spin rotations to identity
    session$sendCustomMessage("reset_spin_rotation", TRUE)
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