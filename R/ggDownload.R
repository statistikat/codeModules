##' Download a ggplot object as an image file.
##'
##' Create a ui that contains controls for the height and width of the plot as well as a
##' `downloadButton`. The module uses [ggsave] to convert the plot into an image file.
##'
##' @param input,output,session Standard module parameters
##' @param plotObj A reactive `ggplot` object
##' @param plotObjName The name of the object in the resulting code
##'
##' @return A reactive string that represents the code for downloading the plot.
##'
##' @examples
##' \dontrun{
##' library(ggplot2)
##'
##' gg <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
##'
##' shinyApp(
##'   fluidPage(
##'     ggDownloadUI("ggd"),
##'     codeOutput("code")
##'   ),
##'   function(input, output, session){
##'     code <- callModule(ggDownload, "ggd", reactive({gg}))
##'     output$code <- renderCode({
##'       code()
##'     })
##'   }
##' )
##' }
##' @importFrom grDevices dev.off
##' @importFrom ggplot2 ggsave
##' @export
ggDownload <- function(input, output, session, plotObj, plotObjName = "ggObj"){
  code_save <- reactiveValues()

  output$download <- downloadHandler(
    filename = function() {
      paste0("grafik_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".", input$format)
    },
    content = function(file) {
      code_save$code <- paste0(
        funCode("ggsave", list(filename = paste0("grafik.", input$format), width= input$width, height = input$height)), "\n"
      )

      ggsave(filename = file, plot = plotObj(), width = input$width, height = input$height,
             device = input$format, dpi = input$dpi)
    }
  )

  ## For some reason, the observer pattern of shiny fails if reactives are set inside a downloadHandler
  ## Therefore, enforce evaluation of the code output periodically to update all references.
  autoInvalidate <- reactiveTimer(2000)
  observeEvent(autoInvalidate(), code_save$code)

  return(reactive({code_save$code}))
}

#' @rdname ggDownload
#' @param id The module identifier.
#' @param buttonText Text do display on the `downloadButton`.
#' @export
ggDownloadUI <- function(id, buttonText = "Download image file"){
  ns <- NS(id)
  tagList(
    div(align = "center", selectInput(ns("format"), "Format", c("png", "pdf", "jpeg", "bmp", "svg", "eps",
                                                                "ps", "tex"))),
    div(align = "center", sliderInput(ns("width"), label = strong("width"), min = 1, max = 30,
                                           value = 10, step = .1, width = "90%", post = " in")),
    div(align = "center", sliderInput(ns("height"), label = strong("height"), min = 1, max = 30,
                                           value = 8, step = .1, width = "90%", post = " in")),
    div(align = "center", sliderInput(ns("dpi"), label = strong("resolution"), min = 10, max = 2000,
                                      value = 300, step = 10, width = "90%", post = " dpi")),
    div(align = "center", downloadButton(ns("download"), strong(buttonText), class = "btn-primary"))
  )
}
