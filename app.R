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
      Shiny.addCustomMessageHandler('rod_toggle', function(showRod) {
        window.postMessage({ type: 'rod_toggle', value: showRod }, '*');
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
                        selected = unique(pitch_data$Pitcher)[1]),
            
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
        checkboxInput("show_field", "Show Field", value = TRUE),
        checkboxInput("show_rod", "Show Rod", value = TRUE)
        
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
  # Reactive values
  playing <- reactiveVal(FALSE)
  original_tilt <- reactiveVal(0)
  current_pitch <- reactiveVal(NULL)
  user_modified <- reactiveVal(FALSE)
  original_values <- reactiveVal(NULL)
  resetting <- reactiveVal(FALSE)
  
  # Main data reactive - this replaces both averaged_pitch_data and filteredData
  pitch_analysis_data <- reactive({
    req(input$pitcher, input$pitchType, input$dateRange)
    
    # Validate pitch type exists for pitcher
    valid_pitch_types <- unique(pitch_data[pitch_data$Pitcher == input$pitcher, "PitchType"])
    if (!input$pitchType %in% valid_pitch_types) return(NULL)
    
    # Get averaged data (this was your averaged_pitch_data)
    avg_data <- get_average_spin_info(
      pitch_data, 
      input$pitcher, 
      input$pitchType, 
      input$dateRange
    )
    
    return(avg_data)
  })
  
  # Dynamic pitch type UI
  output$pitchTypeUI <- renderUI({
    req(input$pitcher != "")
    
    pitcher_data <- pitch_data[pitch_data$Pitcher == input$pitcher, ]
    unique_pitch_types <- unique(pitcher_data$PitchType)
    
    pitch_labels <- sapply(unique_pitch_types, function(pt) {
      abbrev_name <- abbrevs[[pt]]
      if (is.null(abbrev_name)) pt else abbrev_name
    })
    
    div(style = "margin-top: 10px;",   
        selectInput("pitchType", NULL,
                    choices = setNames(unique_pitch_types, pitch_labels),
                    selected = unique_pitch_types[1])
    )
  })
  
  # CONSOLIDATED: Stats outputs using a helper function
  create_stat_output <- function(stat_name, calculation_func) {
    renderText({
      pitch <- pitch_analysis_data()
      if (is.null(pitch)) return("N/A")
      
      if (user_modified() && all(sapply(c("spinTilt_slider", "spinGyro_slider", "ballX_slider", "ballY_slider"), 
                                        function(x) !is.null(input[[x]])))) {
        # Calculate expected values
        user_display_tilt <- input$spinTilt_slider
        actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
        spin_vector <- calculate_vector_from_tilt_gyro(actual_tilt, input$spinGyro_slider)
        
        calculation_func$expected(spin_vector, input$ballX_slider, input$ballY_slider, pitch)
      } else {
        # Show original values
        calculation_func$original(pitch)
      }
    })
  }
  
  # Stats outputs - consolidated into one block
  output$stats_title <- renderText(if (user_modified()) "Expected Pitch Shape" else "Pitch Shape")
  
  output$velo_value <- create_stat_output("velo", list(
    expected = function(spin_vector, ballX, ballY, pitch) paste0(round(pitch$start_speed, 1), " mph"),
    original = function(pitch) paste0(round(pitch$start_speed, 1), " mph")
  ))
  
  output$spin_rate_value <- create_stat_output("spin_rate", list(
    expected = function(spin_vector, ballX, ballY, pitch) paste0(round(pitch$SpinRate, 1), " rpm"),
    original = function(pitch) paste0(round(pitch$SpinRate, 1), " rpm")
  ))
  
  output$vert_break_value <- create_stat_output("vert_break", list(
    expected = function(spin_vector, ballX, ballY, pitch) {
      calculate_expected_vert_break(spin_vector[1], spin_vector[2], spin_vector[3], ballX, ballY, pitch)
    },
    original = function(pitch) paste0(round(pitch$break_z, 1), "\"")
  ))
  
  output$horz_break_value <- create_stat_output("horz_break", list(
    expected = function(spin_vector, ballX, ballY, pitch) {
      calculate_expected_horz_break(spin_vector[1], spin_vector[2], spin_vector[3], ballX, ballY, pitch)
    },
    original = function(pitch) paste0(round(pitch$break_x, 1), "\"")
  ))
  
  output$spin_direction_value <- create_stat_output("spin_direction", list(
    expected = function(spin_vector, ballX, ballY, pitch) {
      calculate_expected_spin_direction(spin_vector[1], spin_vector[2], spin_vector[3], ballX, ballY, pitch)
    },
    original = function(pitch) {
      tilt <- axis_to_tilt_time_simple(pitch$SpinAxis_inf)
      paste0(tilt$hours, ":", sprintf("%02d", round(tilt$minutes)))
    }
  ))
  
  output$spin_efficiency_value <- create_stat_output("spin_efficiency", list(
    expected = function(spin_vector, ballX, ballY, pitch) {
      calculate_expected_spin_efficiency(spin_vector[1], spin_vector[2], spin_vector[3], ballX, ballY, pitch)
    },
    original = function(pitch) paste0(round((pitch$spin_efficiency)*100, 1), "%")
  ))
  
  # CONSOLIDATED: Slider synchronization with helper function
  create_slider_sync <- function(param_name, has_time = FALSE) {
    slider_id <- paste0(param_name, "_slider")
    text_id <- paste0(param_name, "_text")
    time_id <- paste0(param_name, "_time")
    
    # Slider to text sync
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
    
    # Text to slider sync
    observeEvent(input[[text_id]], {
      val <- input[[text_id]]
      if (!is.null(val) && !is.na(val) && abs(input[[slider_id]] - val) > 0.01) {
        updateSliderInput(session, slider_id, value = val)
      }
    }, ignoreInit = TRUE)
    
    # Time input handling (only for spinTilt)
    if (has_time && param_name == "spinTilt") {
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
  
  # Create all slider syncs
  create_slider_sync("spinTilt", has_time = TRUE)
  create_slider_sync("spinGyro")
  create_slider_sync("ballX")
  create_slider_sync("ballY")
  
  # CONSOLIDATED: Main pitch data change handler
  # Handle pitcher changes separately to ensure clean state
  observeEvent(input$pitcher, {
    # updateSelectInput(session, "pitchType", choices = character(0), selected = NULL)
    
    # Reset states immediately when pitcher changes
    playing(FALSE)
    updateActionButton(session, "play_pause_btn", label = "Play")
    session$sendCustomMessage("play_toggle", FALSE)
    user_modified(FALSE)
  }, ignoreInit = TRUE)
  
  # Handle pitch type and date range changes
  observeEvent(list(input$pitchType, input$dateRange), {
    req(input$pitcher, input$pitchType, input$dateRange)
    
    pitch_data <- pitch_analysis_data()
    if (!is.null(pitch_data)) {
      current_pitch(pitch_data)
      session$sendCustomMessage("pitch_uid", "averaged_pitch")
      update_sliders_for_pitch(pitch_data)
      
      # Reset states
      shinyjs::delay(900, { user_modified(FALSE) })
      playing(FALSE)
      updateActionButton(session, "play_pause_btn", label = "Play")
      session$sendCustomMessage("play_toggle", FALSE)
    }
  })
  
  # User modification detection
  observe({
    if (resetting()) return()
    req(original_values(), input$spinTilt_slider, input$spinGyro_slider, input$ballX_slider, input$ballY_slider)
    
    orig_vals <- original_values()
    if (any(sapply(c("spinTilt", "spinGyro", "ballX", "ballY"), function(x) is.null(orig_vals[[x]])))) return()
    
    current_vals <- list(
      spinTilt = input$spinTilt_slider,
      spinGyro = input$spinGyro_slider,
      ballX = input$ballX_slider,
      ballY = input$ballY_slider
    )
    
    changed <- any(sapply(names(current_vals), function(param) {
      abs(current_vals[[param]] - orig_vals[[param]]) > 0.51
    }))
    
    if (changed && !user_modified()) {
      user_modified(TRUE)
    }
  })
  
  # CONSOLIDATED: Slider value updates to JavaScript
  observe({
    vals <- list(
      spinTilt = input$spinTilt_slider,
      spinGyro = input$spinGyro_slider,
      ballX = input$ballX_slider,
      ballY = input$ballY_slider
    )
    
    if (all(sapply(vals[c("spinTilt", "spinGyro")], function(x) !is.null(x))) && !is.null(original_tilt())) {
      user_display_tilt <- vals$spinTilt
      actual_tilt <- (360 - (user_display_tilt - 90)) %% 360
      new_vector <- calculate_vector_from_tilt_gyro(actual_tilt, vals$spinGyro)
      vals$spinVectorX <- new_vector[1]
      vals$spinVectorY <- new_vector[2]
      vals$spinVectorZ <- new_vector[3]
      session$sendCustomMessage("slider_update", vals)
    }
  })
  
  # SIMPLIFIED: Update sliders function
  update_sliders_for_pitch <- function(pitch_data) {
    resetting(TRUE)
    if (is.null(pitch_data)) return()
    
    current_pitch(pitch_data)
    
    # Calculate display values
    spin_x <- pitch_data$spin_backspin
    spin_y <- pitch_data$spin_sidespin
    spin_z <- -pitch_data$spin_gyrospin
    tilt_gyro <- calculate_tilt_gyro_from_vector(spin_x, spin_y, spin_z)
    original_tilt(tilt_gyro$tilt)
    display_tilt <- (360 - tilt_gyro$tilt + 90) %% 360
    
    seam_lat <- if(!is.null(pitch_data$seam_orientation_lat) && !is.na(pitch_data$seam_orientation_lat)) {
      pitch_data$seam_orientation_lat
    } else { 0 }
    seam_lon <- if(!is.null(pitch_data$seam_orientation_lon) && !is.na(pitch_data$seam_orientation_lon)) {
      pitch_data$seam_orientation_lon
    } else { 0 }
    
    # Store and update values
    slider_values <- list(
      spinTilt = round(display_tilt, 1),
      spinGyro = round(tilt_gyro$gyro, 1),
      ballX = round(seam_lon, 1),
      ballY = round(seam_lat, 1)
    )
    
    original_values(slider_values)
    user_modified(FALSE)
    
    # Update all sliders and inputs
    mapply(function(param, value) {
      updateSliderInput(session, paste0(param, "_slider"), value = value)
      updateNumericInput(session, paste0(param, "_text"), value = value)
    }, names(slider_values), slider_values)
    
    # Reset after delay
    shinyjs::delay(950, { resetting(FALSE) })
  }
  
  # Simple event handlers
  observeEvent(input$reset_btn, {
    pitch_data <- pitch_analysis_data()
    if (!is.null(pitch_data)) {
      update_sliders_for_pitch(pitch_data)
      shinyjs::delay(950, { user_modified(FALSE) })
    }
  })
  
  observeEvent(input$pause_for_orientation, {
    playing(FALSE)
    updateActionButton(session, "play_pause_btn", label = "Play")
    session$sendCustomMessage("play_toggle", FALSE)
    session$sendCustomMessage("reset_spin_rotation", TRUE)
  })
  
  observeEvent(input$play_pause_btn, {
    new_state <- !playing()
    playing(new_state)
    updateActionButton(session, "play_pause_btn", label = ifelse(new_state, "Pause", "Play"))
    session$sendCustomMessage("play_toggle", new_state)
  })
  
  observeEvent(input$show_clock, {
    session$sendCustomMessage("clock_toggle", input$show_clock)
  })
  
  observeEvent(input$show_field, {
    session$sendCustomMessage("field_toggle", input$show_field)
  })
  
  observeEvent(input$show_rod, {
    session$sendCustomMessage("rod_toggle", input$show_rod)
  })
}

shinyApp(ui, server)