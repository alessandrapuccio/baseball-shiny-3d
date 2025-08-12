library(shiny)
library(shinyjs)

# Wrapper for sliderInput that adds 'data-immediate="true"' attribute
mySliderInput <- function(inputId, label, min, max, value, step = NULL, ...) {
  sld <- sliderInput(inputId, label, min, max, value, step = step, ...)
  sld$children[[2]]$attribs$`data-immediate` <- "true"
  sld
}

# Example app to test
ui <- fluidPage(
  useShinyjs(),
  mySliderInput("sld1", "Immediate slider", 0, 100, 50, step = 1),
  verbatimTextOutput("val1")
)

server <- function(input, output, session) {
  output$val1 <- renderPrint({
    input$sld1
  })
}

shinyApp(ui, server)
