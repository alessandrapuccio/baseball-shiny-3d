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

# Immediate slider input wrapper data-immediate="true"
immediateSlider <- function(inputId, label, min, max, value, step = NULL, ...) {
  sld <- sliderInput(inputId, label, min, max, value, step = step, ...)
  sld$children[[2]]$attribs$`data-immediate` <- "true"
  sld
}

# Custom slider with input field
customSpinAxisSlider <- function(inputId, label, min, max, value, step = NULL, suffix = "", 
                                 show_slider = TRUE, show_time = FALSE, ...) {
  sliderInputId <- paste0(inputId, "_slider")
  textInputId <- paste0(inputId, "_text")
  timeInputId <- paste0(inputId, "_time")
  
  div(class = "custom-slider-group",
      
      
      # Conditionally show slider + numeric input row
      if (show_slider) {
        div(class = "slider-header",
            span(class = "slider-label", label)
        )
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
      } else {
        # Hidden inputs to maintain reactivity even when slider is hidden
        div(style = "display: none;",
            sliderInput(sliderInputId, NULL, min, max, value, step = step, ...),
            numericInput(textInputId, NULL, value = value, min = min, max = max, step = step)
        )
      },
      
      # Time input section
      if (show_time) {
        div(
          class = "slider-time-container",
          div(
            span(class = "time-label", "Measured Spin Direction:"),
            textInput(
              timeInputId, NULL,
              value = format_time(0, 0),
              placeholder = "HH:MM",
              width = "100px"
            )
          )
        )
      }
  )
}

customSliderWithInput <- function(inputId, label, min, max, value, step = NULL, suffix = "", show_time = FALSE, ...) {
  sliderInputId <- paste0(inputId, "_slider")
  textInputId <- paste0(inputId, "_text")
  timeInputId <- paste0(inputId, "_time")
  
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
      ),
      # Time input on separate line if enabled
      if (show_time) {
        div(
          class = "slider-time-container",
          # style = "display: flex; flex-direction: column; align-items: center;",
          
          # Wrap label + input in one container
          div(
            # style = "display: flex; flex-direction: column; align-items: center;",
            span(class = "time-label", "Tilt Clock Input:"),
            textInput(
              "spinTilt_time", NULL,
              value = format_time(0, 0),
              placeholder = "HH:MM",
              width = "100px"
            )
          )
        )
      }
  )
}

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


axis_to_tilt_time_simple <- function(degrees) {
  
  clock_hour_float <- (degrees / 30 + 6) %% 12
  
  hours <- floor(clock_hour_float)
  if (hours == 0) hours <- 12
  
  # Calculate minutes from fractional part
  minutes_exact <- (clock_hour_float - floor(clock_hour_float)) * 60
  minutes <- round(minutes_exact, 2)
  
  return(list(hours = hours, minutes = minutes))
}

tilt_time_to_axis_deg_simple <- function(hours=0, minutes=0) {
  if (missing(hours) || is.null(hours)) hours <- 0
  if (missing(minutes) || is.null(minutes)) minutes <- 0
  hours <- suppressWarnings(as.numeric(hours))
  minutes <- suppressWarnings(as.numeric(minutes))
  if (is.na(hours)) hours <- 0
  if (is.na(minutes)) minutes <- 0
  
  # Convert clock time back to display degrees
  hour_value <- (hours %% 12) + (minutes / 60.0)
  if (hours == 12) hour_value <- minutes / 60.0
  
  # Convert back to display tilt using NEW mapping
  # Reverse of: clock_hour_float = (degrees / 30 + 6) % 12
  display_degrees <- ((hour_value - 6) * 30 + 360) %% 360
  
  return(display_degrees)
}


# Format time for display (handle decimal minutes)
format_time <- function(hours=0, minutes=0, show_seconds = FALSE) {
  if (show_seconds && minutes != floor(minutes)) {
    mins <- floor(minutes)
    secs <- round((minutes - mins) * 60)
    sprintf("%02d:%02d:%02d", hours, mins, secs)
  } else {
    sprintf("%02d:%02d", hours, floor(minutes))
  }
}

parse_time <- function(time_string) {
  if (grepl(":", time_string)) {
    parts <- strsplit(time_string, ":")[[1]]
    if (length(parts) >= 2) {
      hours <- as.numeric(parts[1])
      minutes <- as.numeric(parts[2])
      seconds <- if(length(parts) >= 3) as.numeric(parts[3]) else 0
      
      if (is.na(hours)) hours <- 0
      if (is.na(minutes)) minutes <- 0
      if (is.na(seconds)) seconds <- 0
      
      # Validate ranges
      if (hours >= 1 && hours <= 12 && minutes >= 0 && minutes < 60) {
        return(list(hours = hours, minutes = minutes + seconds/60))
      }
    }
  }
  return(list(0,0,0))
}