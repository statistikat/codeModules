codeModules
================

About
-----

This R package consists of several [shiny modules](https://shiny.rstudio.com/articles/modules.html) that return R code in the form of `reactive` characters. All those modules represent common operations regarding

-   Import of data (`read.csv`, `read.xlsx`, `data()`, ...)
-   Manipulation of data (rename columns, change column classes, filtering)
-   ~~export of data (`write.csv`, `write.xlsx`, ...)~~

Usage
-----

Always save the output from `callModule` into a variable when working with modules from this package. The outputs can then be parsed as R code using `eval(parse(text = code()))`.

``` r
## context: server.R
code <- callModule(libData, id = "some_id", assignTo = "dt")
output$table <- renderTable({
  eval(parse(text = code()))
  return(dt)
})
```

Implemented Modules
-------------------

-   **libData**: read data from `R` packages using `utils::data`
-   **readData**: read data from a file using `read.csv`, `read.xlsx` or others depending on the fileextension.
-   **variableView** Rename columns, filter data and change column types. The ui is oriented on the "variable view" in SPSS
