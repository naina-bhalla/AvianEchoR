#' Launch the AvianEchoR Interactive Dashboard
#'
#' @description Initializes and opens the bundled Shiny web application, providing a graphical user interface (GUI) for acoustic space visualization, model evaluation, and live audio inference.
#'
#' @details
#' This function searches the internal package directory for the `shiny` app folder. It allows end-users to interact with the underlying Machine Learning pipelines without needing to write any R code.
#'
#' @return Opens the Shiny application in the user's default web browser or RStudio viewer.
#' @export
launch_avian_dashboard <- function() {
  app_dir <- system.file("shiny", package = "AvianEchoR")
  if (app_dir == "") {
    stop("Could not find shiny directory. Try re-installing AvianEchoR.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}


