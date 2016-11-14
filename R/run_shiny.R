#' Run the granular shiny app locally
#'
#' @export
#'
run_granular <- function() {
  appDist <- system.file("shiny", "granular", package = "granular")
  if(appDit == "") {
    stop("Could not find example directory. Try re-installing 'granular'.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}