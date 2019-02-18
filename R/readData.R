#' Read data in different formats (xlsx, rds, csv, ...)
#'
#' Shiny module for reading data into R. currently, the formats `.csv`, `.sav`,
#' `.xlsx`, `.rds` and `.Rdata` are supported. the following formats will be
#' mapped `.txt -> .csv`, `.xls -> .xlsx`.
#'
#' @export
#'
#' @param input,output,session Standard module parameters.
#' @param path A reactive value representing the path of the file to be read.
#' @param callback A function that will be called in case of unsupported
#'   formats. Should have no arguments.
#' @param symbol Optional: A symbol to use in the import code in place of the
#'   path. This is useful if the path has previously been saved as a variable.
#' @return A reactive string representing the code to read the dataset.
#'
#' @examples
#' \dontrun{
#'
#' shinyApp(
#'   fluidPage(
#'     textInput("path", "choose a path", value = "mtcars.csv"),
#'     readDataUI("readDataId"),
#'     codeOutput("code")
#'   ),
#'   function(input, output, session){
#'     code <- callModule(readData, "readDataId", reactive(input$path))
#'     output$code <- renderCode({code()})
#'   }
#' )
#'
#' }
readData <- function(input, output, session, path,
                     callback = function() {
                     },
                     symbol = NULL) {
  fileType <- reactive({
    ending <- tools::file_ext(req(path()))
    switch(ending, txt = "csv", xls = "xlsx", rdata = "Rdata", ending)
  })

  output$controls <- renderUI({
    ns <- session$ns
    switch(
      fileType(),
      csv = tagList(
        checkboxInput(ns("header"), "First row contains variable names", TRUE),
        radioButtons(ns("sep"), "Seperator",
                     c(Semicolon = ";", Tab = "\t", Comma = ","), inline = TRUE)
      ),
      xlsx = checkboxInput(
        ns("header"), "First row contains variable names", TRUE),
      NULL
    )
  })

  code <- eventReactive(input$read, {
    if (is.null(symbol))
      fp <- list(file = path())
    else
      fp <- list(file = as.symbol(symbol))

    paste0(
      switch(
        fileType(),
        csv = funCode(funName = "read.table",
                      c(fp, header = input$header, sep = input$sep)),
        Rdata = paste("get(", funCode("load", fp), ")"),
        xlsx = funCode("readWorksheetFromFile",
                       c(fp, header = input$header, sheet = 1)),
        sav = funCode("read_spss", fp),
        rds = funCode("readRDS", fp), {
          callback()
          "NULL"
        }
      )
    )
  })

  return(code)
}

#' @rdname readData
#' @param id The module identifier.
#' @export
readDataUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("controls")),
    actionButton(ns("read"), "read")
  )
}
