datasets <- as.data.table(suppressWarnings(data(package = .packages(all.available = TRUE))$results))
# do some string splitting to convert for exmple "fdeaths (UKLungDeaths)" into two columns
ObjName <- Package <- NULL
datasets[, ObjName := Item]
datasets[, SourceName := Item]
spl <- strsplit(datasets$Item, ' (', fixed = TRUE)
for(i in 1:nrow(datasets)){
  splc <- spl[[i]]
  if(length(splc) == 2){
    datasets[i, ObjName := splc[1]]
    datasets[i, SourceName := sub(")", "", splc[2], fixed = TRUE) ]
  }
}

#' Read data from installed packages
#'
#' Read data via `utils::data` and import it into R. The ui includes a "show documentation" button
#' to give info about the datasets.
#'
#' @param assignTo A character vector of length one. The symbol to assign the data to in the code
#'
#' @return A reactive string representing the import of the dataset. for example
#' \preformatted{
#' "data('Arthritis', package = 'vcd')\ndt <- Arthritis"
#' }
#'
#' @examples
#' \dontrun{
#'
#' shinyApp(
#'   ui = fluidPage(
#'     libDataUI("import"),
#'     verbatimTextOutput("code"),
#'     dataTableOutput("table")
#'   ),
#'   server = function(input, output, session){
#'     code <- callModule(libData, "import")
#'     output$code <- renderText(code())
#'     output$table <- renderDataTable({
#'       eval(parse(text = code()))
#'       dt
#'     })
#'   }
#' )
#'
#' }
#' @export
libData <- function(input, output, session, assignTo = "dt"){
  observeEvent(
    input$Package,
    updateSelectInput(
      session, "ObjName", choices = datasets[input$Package == Package]$ObjName
    )
  )
  output$description <- renderText(
    datasets[Package == input$Package & ObjName == input$ObjName, ]$Title
  )

  tmp <- tempfile()
  observeEvent(input$documentation,{
    row <- datasets[Package == input$Package & ObjName == input$ObjName, ]
    topic <- row$SourceName[1]
    db <- tools::Rd_db(input$Package)
    rdfile <- paste0(topic, ".Rd")
    req(rdfile %in% names(db))
    tools::Rd2HTML(db[[rdfile]], tmp, no_links = TRUE, package = input$Package)
    showModal(modalDialog(
      title = p("Documentation of", code(input$ObjName)),
      includeHTML(tmp), size = "l", easyClose = TRUE
    ))
  })

  return(eventReactive(input$select, {
    row <- datasets[Package == input$Package & ObjName == input$ObjName, ]
    paste0(
      funCode("data", list(row$SourceName[1], package = input$Package)), "\n",
      assignTo, " <- ", input$ObjName
    )
  }))
}

#' @rdname libData
#' @export
libDataUI <- function(id, selected = "graphSTAT"){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("Package"), "Choose a library", unique(datasets$Package),
                   selected = selected),
    selectInput(ns("ObjName"), "Choose a dataset", NULL),
    strong("Description"),
    textOutput(ns("description")),
    actionButton(ns("select"), "Import"),
    actionButton(ns("documentation"), "Show documentation")
  )
}
