
#' Open GUI
#'
#' Opens an interactive interface.
#'
#' @returns nothing
#' @export
#'
#' @import shiny
#'
#' @examples
#'
#' \dontrun{
#' open_gui()
#' }
#'
#'
open_gui <- function() {
  dir <- system.file("shiny", package = "deliberr")
  if (dir == "") {
    stop("Could not find example directory. Try re-installing `deliberr`.", call. = FALSE)
  }

  shiny::runApp(dir, launch.browser = TRUE, display.mode = "normal")
}
