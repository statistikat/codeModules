#' Generate code for function call
#'
#' Creates a string that represents a function call based on an argument list.
#'
#' @param args a named, partly named or unnamed list containing the parameters
#'   for the function. Supported types are `logical`, `numeric` and `character`.
#'   Entries of `NULL`, `NA`, `""` and `"NA"` will be ignored.
#' @param funName Name of the function to call in the string.
#' @export
#' @return A character vector of length one
#' @examples
#' args <- list(dat = "dt", m = "u", xlab = c("kja", "jsak", "saj"), z = 1,
#'              u = NULL, r = TRUE)
#' funCode("myFunction", args)
funCode <- function(funName, args){
  args <- args[!vapply(
    args,
    function(x) {
      is.null(x) || x == "NA" || x == ""
    },
    TRUE
  )]

  args_str <- lapply(seq_along(args), function(i){
    arg <- args[[i]]
    if (is.null(arg))
      return(NULL)
    if (is.character(arg))
      arg <- shQuote(arg)
    if (length(arg) > 1)
      arg <- paste0("c(", paste(arg, collapse = ","), ")")
    ifelse(
      is.null(names(args)) || names(args)[i] == "",
      arg,
      paste0(names(args)[i], "=", arg)
    )
  }) %>% paste(collapse = ", ")

  paste0(funName, "(", args_str, ")")
}
