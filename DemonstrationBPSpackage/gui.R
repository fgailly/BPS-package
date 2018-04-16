library(shiny)
library(miniUI)
library('BPS')

get_BPMN <- function(processmodel) {

  ui <- miniPage(
    gadgetTitleBar("Load BPMN file"),
    miniContentPanel(
      textInput("filename", "")
    )
  )

  server <- function(input, output) {

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      processmodel <- import_BPMN(filepath = input$filename)
      stopApp(input$filename)
    })
  }

  runGadget(ui, server, viewer= dialogViewer("ggbrush"))
}
