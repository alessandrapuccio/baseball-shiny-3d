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