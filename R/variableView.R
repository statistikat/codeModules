plot_vec_numeric <- function(vec, limits) {
  x <- NULL
  req(is.numeric(limits))
  ggplot2::ggplot(data.frame(x = vec), ggplot2::aes(x)) +
    ggplot2::geom_histogram(
      breaks = hist(vec)$breaks,
      col = "red",
      fill = "green",
      alpha = .2
    ) +
    ggplot2::geom_vline(xintercept = limits[1]) +
    ggplot2::geom_vline(xintercept = limits[2])
}

plot_vec_factor <- function(vec, kept_levels) {
  req(is.character(kept_levels))
  cols <- c("#ED0959", "#3AD142")[1 + levels(vec) %in% kept_levels]
  plot(vec, col = cols)
}

#' SPSS like variable view
#'
#' This module gives a way to recode dataframes by changing column names and column classes. Also,
#' datasets can be filtered based on UI inputs.
#'
#' `selectedVar` is an optional ui element which will show a summary of the variable selected in
#' `variableViewUI`.
#'
#' @param input,output,session Standard module parameters.
#' @param dataset A `reactive` table (for example a `data.frame`)
#' @param dataName A `character` of length one giving the name of the dataset.
#'
#' @return A reactive string representing the code to transform the dataset.
#' @importFrom htmlwidgets JS
#' @importFrom shinyWidgets pickerInput
#' @importFrom graphics hist plot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(DT)
#' data("tips", package = "reshape2")
#' ## no proper handling for integers yet
#' shinyApp(
#'   fluidPage(
#'     column(6, selectInput("dataset", "choose dataset", choices = c("mtcars", "tips")),
#'               variableViewUI("vv")),
#'     column(6, codeOutput("code"), selectedVar("vv"), DTOutput("filtered"))
#'   ),
#'   function(input, output, session){
#'     dataset <- reactive({get(input$dataset)})
#'
#'     code <- callModule(variableView, "vv", dataset)
#'     output$code <- renderCode({ code() })
#'     output$filtered <- renderDT({
#'       dat <- isolate(dataset())
#'       eval(parse(text = code()))
#'       dat
#'    })
#'   },
#'   options = list(launch.browser = TRUE)
#' )
#' }
variableView <- function(input, output, session, dataset, dataName = "dat"){
  ns <- session$ns
  initialized <- reactiveVal(FALSE)

  ds <- reactive({
    if(isolate(initialized()))
      session$sendCustomMessage('unbind-DT', 'table')
    initialized(TRUE)
    dataset()
  })

  shinyInput = function(FUN, len, id, values, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(textInput(ns(
        paste0(id, i)), label = NULL,
        value = values[i]))
    }
    inputs
  }

  selectInputVec <- function(FUN, len, id, selecteds, ...){
    ## FUN: `radioButtons` or `selectInput(..., multiple = TRUE)`
    unlist(lapply(seq_len(len), function(i){
      class <- switch(selecteds[i], integer = "numeric", labelled = "factor", selecteds[i])
      choices <- switch(
        class,
        factor = c("factor", "character"),
        numeric = c("numeric", "factor", "character"),
        character = c("factor", "character"),
        class
      )
      as.character(FUN(ns(paste0(id, i)), label = NULL, selected = class, choices = choices, ...))
    }))
  }

  filterInputs <- function(len, id){
    inputs <- character(len)
    inputs <- unlist(lapply(seq_len(len), function(i){
      output[[paste0(id,"output", i)]] <- renderUI({
        ds <- dataset()
        req(ncol(ds) >= i)
        switch(
          input[[paste0("class", i)]],
          numeric = {
            req(is.numeric(ds[[i]]))
            mins <- min(ds[[i]])
            maxs <- max(ds[[i]])
            div(style = "height: 45px;", sliderInput(
              inputId = ns(paste0(id, i)), label = NULL, min = mins, max = maxs,
              value = c(mins, maxs)))
          },
          factor = {
            pickerInput(ns(paste0(id, i)), label = NULL, choices = as.character(sort(unique(ds[[i]]))),
                        selected = unique(ds[[i]]), multiple = TRUE,
                        options = list(`selected-text-format` = "count > 3"))
          },
          p("no contols available for this class")
        )
      })

      as.character(uiOutput(ns(paste0(id, "output", i))))
    }))
    inputs
  }

  # obtain the values of inputs
  shinyValue = function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }

  output$table <- renderDT({
    ds <- ds()

    df <- data.frame(
      name = shinyInput(textInput, ncol(ds), "name", names(ds)),
      class = selectInputVec(
        radioButtons, ncol(ds), "class", selecteds = as.character(vapply(ds, class, "")),
        inline = TRUE
      ),
      filter = filterInputs(ncol(ds), id = "filter"),
      stringsAsFactors = FALSE
    )
    rownames(df) <- NULL
    datatable(
      df, rownames = FALSE, escape = FALSE, selection = "single",
      options = list(
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() {  Shiny.bindAll(this.api().table().node()); } '),
        pageLength = -1, dom = "t"
      ))
  }, server = FALSE)

  last_row_selected <- reactiveVal()
  observeEvent(input$table_rows_selected, last_row_selected(input$table_rows_selected))

  output$selected_var <- renderPlot({
    selected_var <- req(last_row_selected())
    ds <- dataset()
    vec <- ds[[selected_var]]
    filter <- input[[paste0("filter", selected_var)]]
    switch(
      shinyValue("class", selected_var)[selected_var],
      numeric = plot_vec_numeric(as.numeric(as.character(vec)), filter),
      factor = plot_vec_factor(as.factor(vec), filter),
      character = plot(as.factor(vec)),
      NULL
    )
  })

  code <- eventReactive(input$button, {
    ds <- dataset()
    classes <- shinyValue("class", ncol(ds))
    names_mod <- shinyValue("name", ncol(ds))
    classes_orig <- as.character(vapply(ds, class, ""))
    names_orig <- names(ds)

    modify_names <- paste(unlist(lapply(seq_len(ncol(ds)), function(i){
     if(names_mod[i] != names_orig[i])
       paste0("names(", dataName, ")[",i,"] <- ", shQuote(names_mod[i]))
    })), collapse = "\n")

    modify_classes <- paste(unlist(
      lapply(seq_len(ncol(ds)), function(i){
        if(classes[i] != classes_orig[i])
          paste0(dataName, "$", names_mod[i], " <- ", "as.", classes[i], "(", dataName, "$", names_mod[i], ")")
      })
    ), collapse = "\n")

    filter_values <- paste(unlist(
      lapply(seq_len(ncol(ds)), function(i){
        switch(
          classes[i],
          factor = {
            filter_vals <- input[[paste0("filter", i)]]
            if (length(setdiff(as.character(unique(ds[[i]])), filter_vals)) != 0){
              paste0(
                dataName, " <- subset(", dataName, ", ", names_mod[i], " %in% ", "c(",
                paste(shQuote(filter_vals), collapse = ", "), "))\n"
              )
            }
          },
          numeric = {
            filter_vals <- input[[paste0("filter", i)]]
            paste0(
              if(filter_vals[2] != max(ds[[i]]))
                paste0(dataName, " <- subset(", dataName, ", ", names_mod[i], "<=", filter_vals[2], ")\n"),
              if(filter_vals[1] != min(ds[[i]]))
                paste0(dataName, " <- subset(", dataName, ", ", names_mod[i], ">=", filter_vals[1], ")\n")
            )
          }
        )
      })
    ), collapse = "")

    if(modify_names == "") modify_names <- NULL
    if(modify_classes == "") modify_classes <- NULL
    paste0(unlist(list(modify_names, modify_classes, filter_values)), collapse = "\n")
  })

  return(code)
}

#' @rdname variableView
#' @param id The id of the module.
#' @export
variableViewUI <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("table")),
    tags$script(HTML(paste0("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
          Shiny.unbindAll($('#", id,"-'+id).find('table').DataTable().table().node());
        })"))),
    actionButton(ns("button"), "apply changes")
  )
}

#' @rdname variableView
#' @export
selectedVar <- function(id){
  plotOutput(NS(id, "selected_var"))
}

