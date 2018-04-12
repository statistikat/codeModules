#' Evaluate strings with error popups
#'
#' `evalCode` tries to evaluate code given as a sting. In case of an error, a modal pops
#' up showing the resulting error message.
#'
#' @param code A `character` of length one giving the code to be evaluated.
#' @param env  The environment where the code should be evaluated. Defaults to the
#'             calling environment.
#'
#' @return The result of the specified expression or `NULL` in case of an error
#'
#' @examples
#' \dontrun{
#' shinyApp(
#'   fluidPage(
#'     textAreaInput("code", "code", "# intended typo\nrnrm(100)"),
#'     actionButton("evaluate", "evaluate"),
#'     verbatimTextOutput("result")
#'   ),
#'   function(input, output, session){
#'     res <- eventReactive(
#'       input$evaluate,
#'       evalCode(input$code)
#'     )
#'     output$result <- renderPrint(res())
#'   }
#' )
#' }
#' @export
evalCode <- function(code, env = parent.frame()){
  arg <- deparse(substitute(code))
  tryCatch(
    eval(parse(text = code), envir = env),
    error = function(e){
      showModal(modalDialog(
        title = p("Error while executing code", code(arg)),
        "Call:",
        renderCode(code),
        "Message:",
        renderCode(e$message),
        easyClose = TRUE
      ))
    }
  )
}
