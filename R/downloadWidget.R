#' Download widgets as html
#'
#' Shiny module to save shiny widgets as `html` files. Internally, the
#' conversion is done with [`htmlwidgets::saveWidget`].
#'
#' @param input,output,session Standard shiny module parameters
#' @param rWidget A reactive returning a shiny widget
#' @param filename Argument to be passed to [shiny::downloadHandler]
#' @examples
#' \dontrun{
#' shinyApp(
#'   fluidPage(widgetDownloadUI("wd")),
#'   function(input, output, session) {
#'     callModule(widgetDownload, "wd", reactive({ DT::datatable(mtcars) }))
#'   }
#' )
#' }
#' @export
widgetDownload <- function(input, output, session, rWidget,
                           filename = "widget.html") {
  output$download <- downloadHandler(
    filename = filename,
    content = function(file) {
      htmlwidgets::saveWidget(rWidget(), file, title = input$title)
    }
  )
}

#' @rdname widgetDownload
#' @param id The id of the module
#' @param btnText The text appearing on the download button
#' @export
widgetDownloadUI <- function(id, btnText = "Download widget") {
  ns <- NS(id)
  tagList(
    textInput(ns("title"), "Title", "Widget"),
    downloadButton(ns("download"), btnText)
  )
}
