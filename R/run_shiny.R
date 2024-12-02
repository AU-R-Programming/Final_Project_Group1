#' Launch Shiny App
#'
#' @description Launch the Shiny app included in the package.
#' @export
run_app <- function() {
  app_dir <- system.file("shiny", package = "Optimization")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}