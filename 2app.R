library(shiny)
library(shinyjs)
library(jsonlite)
library(here)
library(dplyr)

source("www/helpers.R")
source("www/helpers_goal_two.R")

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
            # Pitcher selector
            selectInput("pitcher", "Select Pitcher and Pitch Type:",
                        choices = unique(pitch_data$Pitcher),
                        selected = NULL),  #unique(pitch_data$Pitcher)[1]),
            
            # Pitch type selector (dynamic based on pitcher)
            uiOutput("pitchTypeUI"),
            
            div(
              style = "margin-top: 10px;",  # Adjust the value as needed
              dateRangeInput(
                "dateRange", 
                "Select Date Range:",
                start = min(as.Date(pitch_data$Date)),
                end   = max(as.Date(pitch_data$Date)),
                format = "yyyy-mm-dd",
                startview = "month",
                separator = " to "
              )
            )
        ),
        
        # Buttons
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
  # absolutePanel(
  #   bottom = 10, left = 10, right = 10, zIndex = 500,
  #   style = "background-color: #f0f0f0; padding: 10px; font-family: monospace; font-size: 12px; border: 1px solid #ccc;",
  #   h5("DEBUG: Current Averaged Data", style = "margin: 0 0 5px 0;"),
  #   verbatimTextOutput("debug_avg_data")
  # ) 
)


server <- function(input, output, session) {
  playing <- reactiveVal(FALSE)
  original_tilt <- reactiveVal(0)
  current_pitch <- reactiveVal(NULL)
  spinTilt_timer <- reactiveTimer(Inf) 
  
  updating_sliders <- reactiveVal(FALSE)
  last_update_time <- reactiveVal(Sys.time())
  
  ## expected pitch shape
  user_modified <- reactiveVal(FALSE)
  original_values <- reactiveVal(NULL)
  resetting <- reactiveVal(FALSE)  # so it doesnt detect the changes happening when reset is hit and go back to expected panel 
  
  sliders_ready <- reactiveVal(FALSE)
  
  
  
  pitch_selection <- reactive({
    list(
      pitcher = input$pitcher,
      pitchType = input$pitchType,
      dateRange = input$dateRange
    )
  }) %>% debounce(400) 
  
  # NEW: Reactive for averaged pitch data based on user selections
  averaged_pitch_data <- reactive({
    req(input$pitcher, input$pitchType, input$dateRange)
    
    # Validate that the selected pitch type exists for the current pitcher
    valid_pitch_types <- unique(pitch_data[pitch_data$Pitcher == input$pitcher, "PitchType"])
    
    if (!input$pitchType %in% valid_pitch_types) {
      cat("=== INVALID COMBINATION ===\n")
      cat("Pitcher:", input$pitcher, "does not throw pitch type:", input$pitchType, "\n")
      cat("Valid pitch types:", paste(valid_pitch_types, collapse = ", "), "\n")
      cat("Returning NULL until dropdown updates\n")
      cat("=== END DEBUG ===\n")
      return(NULL)
    }
    
    avg_data <- get_average_spin_info(
      pitch_data, 
      input$pitcher, 
      input$pitchType, 
      input$dateRange
    )
    # Debug print
    cat("=== AVERAGED PITCH DATA DEBUG ===\n")
    cat("Pitcher:", input$pitcher, "\n")
    cat("Pitch Type:", input$pitchType, "\n") 
    cat("Date Range:", as.character(input$dateRange[1]), "to", as.character(input$dateRange[2]), "\n")
    cat("Averaged data structure:\n")
    print(str(avg_data))
    # cat("Averaged data values:\n")
    # print(avg_data)
    # cat("=== END DEBUG ===\n")
    
    return(avg_data)
  })
  
        
  filteredData <- reactive({
    req(input$pitcher, input$pitchType, input$dateRange)
    
    pitch_data %>%
      dplyr::filter(
        Pitcher == input$pitcher,
        PitchType == input$pitchType,
        as.Date(Date) >= input$dateRange[1],
        as.Date(Date) <= input$dateRange[2]
      )
    
  })
  
  cat("Filt Data Structure:\n")
  print(str(filteredData))
  
  # observeEvent(input$pitcher, {
  #   updateSelectInput(session, "pitchType", selected = character(0))
  #   user_modified(FALSE)
  #   playing(FALSE)
  #   updateActionButton(session, "play_pause_btn", label = "Play")
  #   session$sendCustomMessage("play_toggle", FALSE)
  # }, priority = 100)
  # 
  # observeEvent(input$pitcher, {
  #   updateSelectInput(session, "pitchType", selected = NULL)
  #   pitch_data$PitchType = NULL;
  # })
  
  observeEvent(input$pitcher, {
    # Clear current data first to prevent invalid combinations
    current_pitch(NULL)
    
    # Reset UI elements
    updateSelectInput(session, "pitchType", selected = character(0))
    user_modified(FALSE)
    playing(FALSE)
    updateActionButton(session, "play_pause_btn", label = "Play")
    session$sendCustomMessage("play_toggle", FALSE)
    
    # Clear sliders to prevent stale data from showing
    sliders_ready(FALSE)
  }, priority = 100)
  
  # Upitch type dropdown to show unique pitch types only
  output$pitchTypeUI <- renderUI({
    req(input$pitcher)  # Make sure a pitcher is selected
  
    # Optional: add a brief placeholder or blank so it "disappears"
    if (is.null(input$pitcher)) {
      return(NULL)
    }
  
    # Get pitch types for the selected pitcher
    pitch_types <- unique(pitch_data$PitchType[pitch_data$Pitcher == input$pitcher])
    
    # Return selectInput with no default selection (so user has to pick)
    selectInput(
      "pitchType",
      "Select Pitch Type:",
      choices = pitch_types,
      selected = NULL
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
  
  # UPDATED: Use averaged pitch data for stats
  output$velo_value <- renderText({
    pitch <- averaged_pitch_data()
    if (!is.null(pitch)) {
      paste0(round(pitch$start_speed, 1), " mph")
    } else {
      "0.0 mph"
    }
  })
  
  output$spin_rate_value <- renderText({
    pitch <- averaged_pitch_data()
    if (!is.null(pitch)) {
      paste0(round(pitch$SpinRate, 1), " rpm")
    } else {
      "0 rpm"
    }
  })
  
  output$vert_break_value <- renderText({
    pitch <- averaged_pitch_data()
    
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
    pitch <- averaged_pitch_data()
    
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
    pitch <- averaged_pitch_data()
    
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
    pitch <- averaged_pitch_data()
    
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
            spinTilt_timer <<- reactiveTimer(650, session)  # 0.65 second timer
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
  # observe({
  #   if (resetting()) return()
  #   # Only proceed if we have original values stored AND all sliders exist
  #   req(original_values())
  #   req(input$spinTilt_slider, input$spinGyro_slider, input$ballX_slider, input$ballY_slider)
  #   
  #   orig_vals <- original_values()
  #   # Make sure original values have all required fields
  #   if (is.null(orig_vals$spinTilt) || is.null(orig_vals$spinGyro) || 
  #       is.null(orig_vals$ballX) || is.null(orig_vals$ballY)) {
  #     return()
  #   }
  #   
  #   current_vals <- list(
  #     spinTilt = input$spinTilt_slider,
  #     spinGyro = input$spinGyro_slider,
  #     ballX = input$ballX_slider,
  #     ballY = input$ballY_slider
  #   )
  #   
  #   # Check if any value has changed from original (with tolerance for rounding)
  #   changed <- abs(current_vals$spinTilt - orig_vals$spinTilt) > 0.51 ||
  #     abs(current_vals$spinGyro - orig_vals$spinGyro) > 0.51 ||
  #     abs(current_vals$ballX - orig_vals$ballX) > 0.51 ||
  #     abs(current_vals$ballY - orig_vals$ballY) > 0.51
  #   
  #   if (changed && !user_modified()) {
  #     
  #     cat("[user_mod] detected change\n")
  #     cat("[user_mod] orig:", str(orig_vals), "\n")
  #     cat("[user_mod] curr:", str(current_vals), "\n")
  #     
  #     user_modified(TRUE)
  #   }
  # })
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
    
    # Only mark as user-modified if sliders are "ready"
    if (changed && !user_modified() && sliders_ready()) {
      cat("[user_mod] detected change\n")
      cat("[user_mod] orig:", str(orig_vals), "\n")
      cat("[user_mod] curr:", str(current_vals), "\n")
      
      user_modified(TRUE)
    }
  })
  
  
  # UPDATED: Initialize sliders with averaged data
  observe({
    pitch_avg <- averaged_pitch_data()
    if (!is.null(pitch_avg)) {
      sliders_ready(FALSE)                  # 🔒
      
      update_sliders_for_averaged_pitch(pitch_avg)
      current_pitch(pitch_avg)
    }
  })
  
  # UPDATED: Handle pitch/date selection changes
  observeEvent(list(input$pitchType, input$dateRange), {
    req(input$pitcher, input$pitchType, input$dateRange)
    
    pitch_avg <- averaged_pitch_data()
    if (!is.null(pitch_avg)) {
      sliders_ready(FALSE)                  # 🔒
      
      current_pitch(pitch_avg)
      # Send a dummy UID for JavaScript compatibility
      session$sendCustomMessage("pitch_uid", "averaged_pitch")
      update_sliders_for_averaged_pitch(pitch_avg)
      
      # Reset user modification flag on selection change
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
  
  # UPDATED: Handle reset button with averaged data
  observeEvent(input$reset_btn, {
    req(input$pitcher, input$pitchType, input$dateRange)
    
    pitch_avg <- averaged_pitch_data()
    if (!is.null(pitch_avg)) {
      sliders_ready(FALSE)                  # 🔒
      
      update_sliders_for_averaged_pitch(pitch_avg)
      
      # Force the modification flag to FALSE after reset
      shinyjs::delay(950, {
        user_modified(FALSE)
      })
    }
  })
  
  update_sliders_for_averaged_pitch <- function(pitch_avg) {
    sliders_ready(FALSE)        # 🔒 block user-mod detection immediately
    resetting(TRUE)
    
    if (is.null(pitch_avg)) return()
    
    current_pitch(pitch_avg)
    
    spin_x <- pitch_avg$spin_backspin
    spin_y <- pitch_avg$spin_sidespin
    spin_z <- -pitch_avg$spin_gyrospin
    tilt_gyro <- calculate_tilt_gyro_from_vector(spin_x, spin_y, spin_z)
    original_tilt(tilt_gyro$tilt)
    display_tilt <- (360 - tilt_gyro$tilt + 90) %% 360
    
    seam_lat <- if(!is.null(pitch_avg$seam_orientation_lat) && !is.na(pitch_avg$seam_orientation_lat)) pitch_avg$seam_orientation_lat else 0
    seam_lon <- if(!is.null(pitch_avg$seam_orientation_lon) && !is.na(pitch_avg$seam_orientation_lon)) pitch_avg$seam_orientation_lon else 0
    display_lon <- seam_lon
    display_lat <- seam_lat
    
    # Store ORIGINALS before updating inputs
    original_values(list(
      spinTilt = round(display_tilt, 1),
      spinGyro = round(tilt_gyro$gyro, 1),
      ballX    = round(display_lon, 1),
      ballY    = round(display_lat, 1)
    ))
    
    user_modified(FALSE)
    
    # Programmatic updates
    updateSliderInput(session, "spinTilt_slider", value = round(display_tilt, 1))
    updateNumericInput(session, "spinTilt_text",  value = round(display_tilt, 1))
    updateSliderInput(session, "spinGyro_slider", value = round(tilt_gyro$gyro, 1))
    updateNumericInput(session, "spinGyro_text",  value = round(tilt_gyro$gyro, 1))
    updateSliderInput(session, "ballX_slider",    value = round(display_lon, 1))
    updateNumericInput(session, "ballX_text",     value = round(display_lon, 1))
    updateSliderInput(session, "ballY_slider",    value = round(display_lat, 1))
    updateNumericInput(session, "ballY_text",     value = round(display_lat, 1))
    
    # After a brief settle period, re-enable
    reset_timer <- reactiveTimer(950, session)
    observe({
      reset_timer()
      resetting(FALSE)
      sliders_ready(TRUE)       # ✅ only now allow user-mod detection
    })
  }
  
  
  # # UPDATED: New function to update sliders with averaged pitch data
  # update_sliders_for_averaged_pitch <- function(pitch_avg) {
  #   
  #   resetting(TRUE)
  #   
  #   if (is.null(pitch_avg)) return()
  #   
  #   current_pitch(pitch_avg)
  #   
  #   spin_x <- pitch_avg$spin_backspin
  #   spin_y <- pitch_avg$spin_sidespin
  #   spin_z <- -pitch_avg$spin_gyrospin
  #   tilt_gyro <- calculate_tilt_gyro_from_vector(spin_x, spin_y, spin_z)
  #   original_tilt(tilt_gyro$tilt)
  #   display_tilt <- (360 - tilt_gyro$tilt + 90) %% 360
  #   
  #   seam_lat <- if(!is.null(pitch_avg$seam_orientation_lat) && !is.na(pitch_avg$seam_orientation_lat)) {
  #     pitch_avg$seam_orientation_lat
  #   } else {
  #     0
  #   }
  #   seam_lon <- if(!is.null(pitch_avg$seam_orientation_lon) && !is.na(pitch_avg$seam_orientation_lon)) {
  #     pitch_avg$seam_orientation_lon
  #   } else {
  #     0
  #   }
  #   display_lon <- seam_lon
  #   display_lat <- seam_lat
  #   
  #   # cat("[update_sliders] called\n")
  #   # cat("[update_sliders] display_tilt:", display_tilt, 
  #   #     "gyro:", tilt_gyro$gyro, 
  #   #     "lat/lon:", display_lat, display_lon, "\n")
  #   # 
  #   # cat("[update_sliders] spinTilt set to", round(display_tilt, 1), "\n")
  #   # cat("[update_sliders] spinGyro set to", round(tilt_gyro$gyro, 1), "\n")
  #   # 
  #   
  #   # STORE ORIGINAL VALUES (ROUNDED AS THEY APPEAR ON SLIDERS)
  #   original_values(list(
  #     spinTilt = round(display_tilt, 1),
  #     spinGyro = round(tilt_gyro$gyro, 1),
  #     ballX = round(display_lon, 1),
  #     ballY = round(display_lat, 1)
  #   ))
  #   
  #   # RESET USER MODIFICATION FLAG
  #   user_modified(FALSE)
  #   
  #   # Update spin controls
  #   updateSliderInput(session, "spinTilt_slider", value = round(display_tilt, 1))
  #   updateNumericInput(session, "spinTilt_text", value = round(display_tilt, 1))
  #   updateSliderInput(session, "spinGyro_slider", value = round(tilt_gyro$gyro, 1))
  #   updateNumericInput(session, "spinGyro_text", value = round(tilt_gyro$gyro, 1))
  #   
  #   updateSliderInput(session, "ballX_slider", value = round(display_lon, 1))
  #   updateNumericInput(session, "ballX_text", value = round(display_lon, 1))
  #   updateSliderInput(session, "ballY_slider", value = round(display_lat, 1))
  #   updateNumericInput(session, "ballY_text", value = round(display_lat, 1))
  #   
  #   
  #   # small delay to ensure all slider updates are complete before re-enabling change detection that would flip shape panel back to expected
  #   reset_timer <- reactiveTimer(950, session)
  #   observe({
  #     reset_timer()
  #     resetting(FALSE)
  #   })
  #   sliders_ready(TRUE)
  # }
  
  # Tilt + gyro updates
  # observe({
  #   
  #   if (!user_modified()) {
  #     # skip sending until user touches
  #     return()
  #   }
  #   
  #   vals <- list(
  #     spinTilt = input$spinTilt_slider,
  #     spinGyro = input$spinGyro_slider,
  #     ballX = input$ballX_slider,
  #     ballY = input$ballY_slider
  #     
  #   )
  #   if (!is.null(vals$spinTilt) && !is.null(vals$spinGyro) && !is.null(original_tilt())) {
  #     user_display_tilt <- vals$spinTilt
  #     actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
  #     new_vector <- calculate_vector_from_tilt_gyro(actual_tilt, vals$spinGyro)
  #     vals$spinVectorX <- new_vector[1]
  #     vals$spinVectorY <- new_vector[2]
  #     vals$spinVectorZ <- new_vector[3]
  #     
  #     
  #     cat("[slider_observer] sending slider_update\n")
  #     cat("[slider_observer] spinTilt:", vals$spinTilt, 
  #         "spinGyro:", vals$spinGyro, "\n")
  #     cat("[slider_observer] vector:", vals$spinVectorX, 
  #         vals$spinVectorY, vals$spinVectorZ, "\n")
  #     
  #     
  #     session$sendCustomMessage("slider_update", vals)
  #   }
  # })
  
  # keep last sent values
  last_sent <- reactiveVal(NULL)
  
  observe({
    if (!user_modified()) {
      # skip sending until user touches
      return()
    }
    
    vals <- list(
      spinTilt = input$spinTilt_slider,
      spinGyro = input$spinGyro_slider,
      ballX    = input$ballX_slider,
      ballY    = input$ballY_slider
    )
    
    if (!is.null(vals$spinTilt) && !is.null(vals$spinGyro) && !is.null(original_tilt())) {
      user_display_tilt <- vals$spinTilt
      actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
      new_vector <- calculate_vector_from_tilt_gyro(actual_tilt, vals$spinGyro)
      vals$spinVectorX <- new_vector[1]
      vals$spinVectorY <- new_vector[2]
      vals$spinVectorZ <- new_vector[3]
      
      # ---- NEW: de-dupe with tolerance ----
      prev <- last_sent()
      tol <- 0.52
      should_send <- is.null(prev) ||
        abs(vals$spinTilt - prev$spinTilt) > tol ||
        abs(vals$spinGyro - prev$spinGyro) > tol ||
        abs(vals$ballX    - prev$ballX)    > tol ||
        abs(vals$ballY    - prev$ballY)    > tol
      
      if (should_send) {
        cat("[slider_observer] sending slider_update\n")
        cat("[slider_observer] spinTilt:", vals$spinTilt, 
            "spinGyro:", vals$spinGyro, "\n")
        cat("[slider_observer] vector:", vals$spinVectorX, 
            vals$spinVectorY, vals$spinVectorZ, "\n")
        
        session$sendCustomMessage("slider_update", vals)
        last_sent(vals)   # update cache
      } else {
        cat("[slider_observer] skipped (within tolerance)\n")
      }
    }
  })
  

  # pitch type selector
  output$pitchTypeUI <- renderUI({
    req(input$pitcher)  # Wait until pitcher is selected
    
    # Get valid pitch types for this pitcher
    pitch_types <- unique(pitch_data$PitchType[pitch_data$Pitcher == input$pitcher])
    
    # Return the selectInput
    selectInput("pitchType", "Select Pitch Type:",
                choices = pitch_types,
                selected = NULL)  # NULL ensures it starts empty until user chooses
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
  # 
  # output$debug_avg_data <- renderText({
  #   pitch <- averaged_pitch_data()
  #   if (!is.null(pitch)) {
  #     # Create horizontal display of key values
  #     values <- c(
  #       paste0("avg spin_backspin: ", round(pitch$spin_backspin, 2)), paste0("avg spin_sidespin: ", round(pitch$spin_sidespin, 2)),
  #       paste0("avg spin_gyrospin: ", round(pitch$spin_gyrospin, 2)),  paste0("avg start_speed: ", round(pitch$start_speed, 1)),
  #       paste0("avg SpinRate: ", round(pitch$SpinRate, 0)), paste0("avg break_z: ", round(pitch$break_z, 2)),
  #       paste0("avg break_x: ", round(pitch$break_x, 2)),  paste0("avg SpinAxis_inf: ", round(pitch$SpinAxis_inf, 1)),
  #       paste0("avg spin_efficiency: ", round(pitch$spin_efficiency, 3)), paste0("avg seam_lat: ", round(pitch$seam_orientation_lat, 1)),
  #       paste0("avg seam_lon: ", round(pitch$seam_orientation_lon, 1))
  #     )
  #     paste(values, collapse = " | ")
  #   } else {
  #     "No data available (invalid pitcher/pitch type combination)"
  #   }
  # })
}

shinyApp(ui, server)