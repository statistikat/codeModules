get_datasets <- function() {
  Item <- Package <- ObjName <- Title <- SourceName <- NULL
  datasets <- as.data.table(suppressWarnings(utils::data(
    package = .packages(all.available = TRUE))$results))
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
  datasets %>% dplyr::select(Package, ObjName, Title, SourceName)
}

#' Read data from installed packages
#'
#' Read data via `utils::data` and import it into R. The ui includes a "show documentation" button
#' to give info about the datasets.
#'
#' @param assignTo A character vector of length one. The symbol to assign the data to in the code.
#' @param input,output,session Standard module parameters.
#' @param ignoreStyleSheet By default, the R documentation pages use a special `css` sheet for styling.
#'                         Set this option to `FALSE` to load this stylesheet.
#'
#' @return A reactive string representing the import of the dataset. for example.
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
#'     codeOutput("code"),
#'     dataTableOutput("table")
#'   ),
#'   server = function(input, output, session){
#'     code <- callModule(libData, "import")
#'     output$code <- renderCode(code())
#'     output$table <- renderDataTable({
#'       eval(parse(text = code()))
#'       dt
#'     })
#'   }
#' )
#'
#' }
#' @export
libData <- function(input, output, session, assignTo = "dt", ignoreStyleSheet = TRUE){
  Item <- Package <- ObjName <- NULL

  datasets <- get_datasets()

  shiny::addResourcePath("rcss", system.file("html", package = "codeModules"))

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
    tools::Rd2HTML(db[[rdfile]], tmp, no_links = TRUE, package = input$Package, stylesheet = "rcss/R.css")

    ## TODO; Find a proper way of removing the css inclusion or load the css file locally somehow.
    if(ignoreStyleSheet){
      xx <- readLines(tmp)
      xx <- xx[-3]
      writeLines(xx, tmp)
    }

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
#' @param selected The R package initially selected in the GUI.
#' @param id The module identifier.
#' @export
libDataUI <- function(id, selected = "datasets"){
  ns <- NS(id)

  datasets <- get_datasets()

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
