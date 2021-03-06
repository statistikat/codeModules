% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variableView.R
\name{variableView}
\alias{variableView}
\alias{variableViewUI}
\alias{selectedVar}
\title{SPSS like variable view}
\usage{
variableView(input, output, session, dataset, dataName = "dat")

variableViewUI(id)

selectedVar(id)
}
\arguments{
\item{input, output, session}{Standard module parameters.}

\item{dataset}{A \code{reactive} table (for example a \code{data.frame})}

\item{dataName}{A \code{character} of length one giving the name of the dataset.}

\item{id}{The id of the module.}
}
\value{
A list with the following entries
\itemize{
\item \code{code} a reactive string representing the code to transform the dataset.
\item \code{get_state()} A function to export the state of the widget into an
R object
\item \code{get_state(state)} A function to restore a saved state. The dataset
provided in \code{dataset} must be the same as when \code{get_state()} was called
in order to ensure a proper restore
}
}
\description{
This module gives a way to manipulate dataframes by changing column names and
column classes. Also, datasets can be filtered based on user inputs.
}
\details{
\code{selectedVar} is an optional ui element which will show a summary of the
variable selected in
\code{variableViewUI}.
}
\examples{
\dontrun{

library(DT)
data("tips", package = "reshape2")
data("diamonds", package = "ggplot2")

shinyApp(
  fluidPage(
    column(6, selectInput("dataset", "choose dataset",
           choices = c("mtcars", "tips", "diamonds")),
              variableViewUI("vv"), actionButton("save", "save"),
              actionButton("load", "load")),
    column(6, codeOutput("code"), selectedVar("vv"), DTOutput("filtered"))
  ),
  function(input, output, session){
    dataset <- reactive({get(input$dataset)})

    varView <- callModule(variableView, "vv", dataset)
    state <- NULL
    observeEvent(input$save, {
       state <<- isolate(varView$get_state())
    })
    observeEvent(input$load, {
      varView$set_state(state)
    })
    output$code <- renderCode({ varView$code() })
    output$filtered <- renderDT({
      dat <- isolate(dataset())
      eval(parse(text = varView$code()))
      dat
   })
  },
  options = list(launch.browser = TRUE)
)
}
}
