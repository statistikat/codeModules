#' Download ggplots with a modal window
#'
#' this module invokes a modal window, where parameters for the function
#' `ggsave()` can be specified`
#' @param id module identifier
#' @param label label of the button
#'
#' @rdname ggdownload_popup
ggdownload_popup_ui <- function(id, label = "download...") {
  ns <- NS(id)
  actionButton(ns("show_dialog"), label)
}

two_cols <- function(x, y) {
  fluidRow(
    column(6, x),
    column(6, y)
  )
}

#' @examples
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     ggdownload_popup_ui("gg_popup")
#'   ),
#'   server = function(input, output, session) {
#'     plot <- reactive({
#'       library(ggplot2)
#'       ggplot(mtcars) + aes(wt, mpg) + geom_point()
#'     })
#'     callModule(ggdownload_popup_server, "gg_popup", plot)
#'   }
#' )
#' }
#' @rdname ggdownload_popup
#' @param input,output,session Standard module parameters
#' @param plotObj A reactive ggplot object
ggdownload_popup_server <- function(input, output, session, plotObj) {
  ns <- session$ns
  observeEvent(input$show_dialog, {
    showModal(modalDialog(
      title = "Download plot",
      two_cols(
        selectInput(ns("format"), "format",
                    c("png", "pdf", "jpeg", "bmp", "svg",
                      "eps", "ps", "tex", "emf")),
        sliderInput(ns("width"), label = "width", min = 1, max = 30,
                              value = 10, step = .1, post = " in")
      ),
      two_cols(
        sliderInput(
          ns("height"), label = "height", min = 1, max = 30,
          value = 8, step = .1, post = " in"),
        sliderInput(
          ns("dpi"), label = strong("resolution"), min = 10, max = 2000,
          value = 300, step = 10, post = " dpi")
      ),
      downloadButton(ns("download")),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("plot_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".", input$format)
    },
    content = function(file) {
      device <- input$format
      if (device == "emf")
        device <- dev_emf
      ggsave(filename = file, plot = plotObj(), width = input$width,
             height = input$height, device = device, dpi = input$dpi)
      shiny::removeModal()
    }
  )
}
