#' Launch radiant.basics in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.basics()
#' }
#' @export
radiant.basics <- function(state, ...) radiant.data::launch(package = "radiant.basics", run = "browser", state, ...)

#' Launch radiant.basics in an Rstudio window
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.basics_window()
#' }
#' @export
radiant.basics_window <- function(state, ...) radiant.data::launch(package = "radiant.basics", run = "window", state, ...)

#' Launch radiant.basics in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.basics_viewer()
#' }
#' @export
radiant.basics_viewer <- function(state, ...) radiant.data::launch(package = "radiant.basics", run = "viewer", state, ...)
