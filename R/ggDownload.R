##' Download a ggplot object as an image file.
##'
##' Create a ui that contains controls for the height and width of the plot as well as a
##' `downloadButton`.
##'
##' @param input,output,session Standard module parameters
##' @param plotObj A reactive `ggplot` object
##' @param objName The name of the object in the resulting code
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
##' @export
ggDownload <- function(input, output, session, plotObj, plotObjName = "ggObj"){
  code_save <- reactiveValues()

  output$download <- downloadHandler(
    filename = function() {
      paste0("grafik_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".", input$format)
    },
    content = function(file) {
      code_save$code <- paste0(
        input$format, "(file=", shQuote(paste0("grafik.", input$format)), ", width=", input$width,
        ", height=", input$height, ")\n",
        plotObjName, "\ndev.off()\n"
      )

      message(code_save$code)

      deviceFun <- get(input$format)
      deviceFun(file = file, width = input$width, height = input$height)
      print(plotObj())
      dev.off()
    }
  )

  ## For some reason, the observer pattern of shiny fails if reactives are set inside a downloadHandler
  ## Therefore, enforce evaluation of the code output periodically to update all references.
  autoInvalidate <- reactiveTimer(2000)
  observeEvent(autoInvalidate(), code_save$code)

  return(reactive({code_save$code}))
}

#' @rdname ggDownload
#' @param id The module identifier
#' @export
ggDownloadUI <- function(id, buttonImg = "Download image file"){
  ns <- NS(id)
  tagList(
    div(align = "center", selectInput(ns("format"), "Format", c("png", "jpeg", "bmp", "tiff"))),
    div(align = "center", sliderInput(ns("width"), label = strong("width"), min = 200, max = 2000,
                                           value = 600, step = 10, width = "90%", post = " Pixel")),
    div(align = "center", sliderInput(ns("height"), label = strong("height"), min = 200, max = 2000,
                                           value = 400, step = 10, width = "90%", post = " Pixel")),
    div(align = "center", downloadButton(ns("download"), strong(buttonImg), class = "btn-primary"))
  )
}
