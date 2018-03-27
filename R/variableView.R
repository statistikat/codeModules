#' SPSS like variable view
#'
#' This module gives a way to recode dataframes by changing column names and column classes. Also,
#' datasets can be filtered based on UI inputs.
#'
#' @param input,output,session Standard module parameters.
#' @param dataset A `reactive` table (for example a `data.frame`)
#' @param dataName A `character` of length one giving the name of the dataset.
#'
#' @return A reactive string representing the code to transform the dataset.
#'
#' @export
#'
#' @examples
#' data("tips", package = reshape2)
#' ## no proper handling for integers yet
#' tips$size <- as.numeric(tips$size)
#' shinyApp(
#'   fluidPage(
#'     column(6, variableViewUI("vv")),
#'     column(6, verbatimTextOutput("code"), DTOutput("filtered"))
#'   ),
#'   function(input, output, session){
#'     code <- callModule(variableView, "vv", reactive({tips}))
#'     output$code <- renderText({ code() })
#'     output$filtered <- renderDT({
#'       dat <- tips
#'       eval(parse(text = code()))
#'       dat
#'    })
#'   }
#' )
variableView <- function(input, output, session, dataset, dataName = "dat"){
  ns <- session$ns

  shinyInput = function(FUN, len, id, values, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(ns(paste0(id, i)), label = NULL, value = values[i], ...))
    }
    inputs
  }

  selectInputVec <- function(FUN, len, id, selecteds, ...){
    inputs <- character(len)
    for (i in seq_len(len)){
      inputs[i] <- as.character(FUN(ns(paste0(id, i)), label = NULL, selected = selecteds[i], ...))
    }
    inputs
  }

  filterInputs <- function(len, id){
    inputs <- character(len)
    inputs <- unlist(lapply(seq_len(len), function(i){
      output[[paste(id, i)]] <- renderUI({
        ds <- dataset()
        switch(
          input[[paste0("class", i)]],
          numeric = {
            mins <- min(ds[[i]])
            maxs <- max(ds[[i]])
            div(style = "height: 45px;", sliderInput(
              inputId = ns(paste0(id, i)), label = NULL, min = mins, max = maxs,
              value = c(mins, maxs)))
          },
          factor = {
            checkboxGroupInput(ns(paste0(id, i)), label = NULL, choices = unique(ds[[i]]),
                               inline = TRUE, selected = unique(ds[[i]]))
          },
          p("no contols available for this class")
        )
      })

      as.character(uiOutput(ns(paste(id, i))))
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
    ds <- dataset()
    df <- data.frame(
      name = shinyInput(textInput, ncol(ds), "name", names(ds)),
      class = selectInputVec(
        radioButtons, ncol(ds), "class", selecteds = as.character(vapply(ds, class, "")),
        choices = c("numeric", "factor", "character"), inline = TRUE
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
    actionButton(ns("button"), "apply changes")
  )
}
