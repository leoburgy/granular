#' Run the granular shiny app locally
#'
#' @param port The TCP port that the application should listen on. Defaults to choosing a random port. 
#' @param launch.browser If true, the system's default web browser will be launched automatically after the app is started. defaults to true in interactive sessions only.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the \code{shiny.host} option, if set, or \code{"127.0.0.1"} if not.
#'
#' @export
run_granular <- function(port = NULL, 
                         launch.browser = TRUE,
                         host=getOption('shiny.host', '127.0.0.1')) {
  appDir <- system.file("shiny", "granular", package = "granular")
  if(appDir == "") {
    stop("Could not find example directory. Try re-installing 'granular'.", call. = FALSE)
  } else {
    shiny::runApp(appDir, port = port, host = host,launch.browser = launch.browser,  display.mode = "normal")
  }
  
}