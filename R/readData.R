#' Read data in different formats (xlsx, rds, csv, ...)
#'
#' Shiny module for reading data into R. currently, the formats `.csv`, `.sav`, `.xlsx`, `.rds`
#' and `.Rdata` are supported. the following formats will be mapped `.txt -> .csv`, `.xls -> .xlsx`.
#'
#' @export
#'
#' @param path A reactive value representing the path of the file to be read
#' @param callback A function that will be called in case of unsupported formats. Should have no arguments.
#' @return A reactive string representing the code to read the dataset.
#'
#' @examples
#' \dontrun{
#'
#' shinyApp(
#'   fluidPage(
#'     textInput("path", "choose a path", value = "mtcars.csv"),
#'     readDataUI("readDataId"),
#'     verbatimTextOutput("code")
#'   ),
#'   function(input, output, session){
#'     code <- callModule(readData, "readDataId", reactive(input$path))
#'     output$code <- renderText({code()})
#'   }
#' )
#'
#' }
readData <- function(input, output, session, path, callback = function(){}){
  fileType <- reactive({
    ending <- tools::file_ext(req(path()))
    switch(ending, txt = "csv", xls = "xlsx", rdata = "Rdata", ending)
  })

  output$controls <- renderUI({
    ns <- session$ns
    switch(
      fileType(),
      csv = tagList(
        checkboxInput(ns("header"), "Erste Zeile sind Variablennamen", TRUE),
        radioButtons(ns("sep"), "Trennzeichen", c(Semicolon = ';', Tab = '\t', Comma = ','),
                     inline = TRUE)
      ),
      xlsx = checkboxInput(ns("header"), "Erste Zeile sind Variablennamen", TRUE),
      NULL
    )
  })

  code <- eventReactive(input$read, {
    qp <- shQuote(path())

    paste0(
      switch(
        fileType(),
        csv = paste0('read.table(', qp ,', header=', input$header, ', sep="', input$sep, '")'),
        Rdata = paste0('get(load(file = ', qp, '))'),
        xlsx = paste0('readWorksheetFromFile(', qp, ', header=', input$header, ', sheet=1)'),
        sav = paste0('read_spss(file = ', qp,')'),
        rds = paste0('readRDS(', qp,')'),
        {
          callback()
          "NULL"
        }
      )
    )
  })

  return(code)
}

#' @rdname readData
#' @export
readDataUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("controls")),
    actionButton(ns("read"), "read")
  )
}
