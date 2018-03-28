rCodeContainer <- function(...){
  code <- HTML(as.character(tags$code(class = "language-r", ...)))
  div(pre(code))
}

injectHighlightHandler <- function () {
  code <- "
    Shiny.addCustomMessageHandler(
      'highlight-code',
      function(message) {
        var id = message['id'];
        var delay = message['delay'];
        setTimeout(
          function() {
            var el = document.getElementById(id);
            hljs.highlightBlock(el);
          },
          delay
        );
      }
    );"
  tags$script(code)
}

includeHighlightJs <- function(){
  resources <- system.file("www/shared/highlight", package = "shiny")
  singleton(list(
    includeScript(file.path(resources, "highlight.pack.js")),
    includeCSS(file.path(resources, "rstudio.css")),
    injectHighlightHandler()
  ))
}

#' Render code with syntax highlighting
#'
#' These functions work similar to `renderText` and `verbatimTextOutput` but will include highlighting
#' with `highlight.js` and `rstudio.css`.
#'
#' @export
#' @inheritParams shiny::renderText
#' @param outputArgs A list of arguments to be passed through to the implicit call to [codeOutput]
#'                   when `renderCode` is used in an interactive R Markdown document.
#' @param delay How much delay to use (in ms) between rendering and firing the syntax highlighting
#'              script. For `length(delay) > 1` the script will fire multiple times.
#'
#' @examples
#' \dontrun{
#' shinyApp(
#'   fluidPage(
#'     textAreaInput("code_in", NULL, width = "1000px", height = "200px",
#'       paste("f <- function(x) {2*x + 3}", "f(1)", "#> 5", sep = "\n")),
#'     codeOutput("code_out")
#'   ),
#'   function(input, output, session){
#'     output$code_out <- renderCode({
#'       paste(input$code_in)
#'     })
#'   }
#' )
#' }
renderCode <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list(), delay = 100){
  installExprFunction(expr, "func", env, quoted)
  renderFunc <- function(shinysession, name, ...) {
    value <- func()
    for(d in delay)
      shinysession$sendCustomMessage("highlight-code", list(id = name, delay = d))
    return(paste(utils::capture.output(cat(value)), collapse = "\n"))
  }
  markRenderFunction(codeOutput, renderFunc, outputArgs = outputArgs)
}

#' @rdname renderCode
#'
#' @param outputId output variable to read the code from.
#' @export
codeOutput <- function(outputId){
  tagList(
    includeHighlightJs(),
    uiOutput(outputId, container = rCodeContainer)
  )
}
