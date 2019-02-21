#' @importFrom utils write.table
dtCsv <- function(input, output, session, rTable, fileName = "tableFile"){
  output$download <- downloadHandler(
    filename = function() {
      paste0(fileName, "_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".", "csv")
    },
    content = function(file){
      write.table(rTable(), file, row.names = input$row.names,
                  col.names = input$col.names, sep = input$sep, dec = input$dec)
    }
  )
}

dtCsvUI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("row.names"), "save row names", FALSE),
    checkboxInput(ns("col.names"), "Save column names", TRUE),
    selectInput(ns("dec"), "Decimal seperator", c(",", ".")),
    selectInput(ns("sep"), "Seperator", list(";", "space" = " ")),
    downloadButton(ns("download"))
  )
}

dtRds <- function(input, output, session, rTable, fileName = "tableFile"){
  output$download <- downloadHandler(
    filename = function() {
      paste0(fileName, "_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".", "rds")
    },
    content = function(file) {
      saveRDS(rTable(), file)
    }
  )
}

dtRdsUI <- function(id) {
  ns <- NS(id)
  tagList(downloadButton(ns("download")))
}

dtXlsx <- function(input, output, session, rTable, fileName = "tableFile"){
  output$download <- downloadHandler(
    filename = function() {
      paste0(fileName, "_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".", "xlsx")
    },
    content = function(file) {
      xlsx::write.xlsx(
        rTable(), file, row.names = input$row.names, col.names = input$col.names
      )
    }
  )
}

dtXlsxUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("sheetName"), "Name of the excel sheet", "Sheet1"),
    checkboxInput(ns("row.names"), "Save row names", FALSE),
    checkboxInput(ns("col.names"), "Save column names", TRUE),
    downloadButton(ns("download"))
  )
}

#' Download tables in different formats
#'
#' This module uses `downloadHandler` to allow downloads of tables in different
#' formats (`xlsx`, `rds`, and `csv`).
#'
#' @param input,output,session Standard module parameters
#' @param rTable A reactive table (for example a `data.frame`)
#' @param fileName A prefix for the default name in the download handler.
#'
#' @return Currently, no reactive code is returned due to issues with
#'   `downloadTable`. See [here](https://stackoverflow.com/questions/45458348/)
#' @examples
#' \dontrun{
#' shinyApp(
#'   fluidPage(downloadTableUI("downloadTable")),
#'   function(input, output, session) {
#'     callModule(downloadTable, "downloadTable", reactive({mtcars}),
#'                fileName = "mtcars")
#'   }
#' )
#' }
#' @export
downloadTable <- function(input, output, session, rTable,
                          fileName = "tableFile") {
  callModule(dtCsv, "csv", rTable, fileName)
  callModule(dtRds, "rds", rTable, fileName)
  callModule(dtXlsx, "xlsx", rTable, fileName)

  output$submodule <- renderUI({
    ns <- session$ns
    switch(
      req(input$type),
      rds = dtRdsUI(ns("rds")),
      csv = dtCsvUI(ns("csv")),
      xlsx = dtXlsxUI(ns("xlsx"))
    )
  })
}

#' @param id The module identifier
#' @rdname downloadTable
#' @export
downloadTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("type"), "choose a file type", c("rds", "csv", "xlsx")),
    uiOutput(ns("submodule"))
  )
}
