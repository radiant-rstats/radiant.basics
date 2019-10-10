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

#' Convert a string of numbers into a vector
#'
#' @param x A string of numbers that may include fractions
#' @param char If string contains fractions, return as a character vector
#'
#' @importFrom radiant.data is_empty
#' @importFrom MASS fractions
#'
#' @examples
#' make_vec("1 2 4")
#' make_vec("1/2 2/3 4/5")
#' make_vec("1/2 2/3 4/5", char = TRUE)
#' make_vec("1/2 2/3 1", char = TRUE)
#' @export
make_vec <- function(x, char = FALSE) {
  if (is_empty(x)) {
    return(NULL)
  }
  any_frac <- FALSE
  check_frac <- function(x) {
    if (length(x) == 2) {
      any_frac <<- TRUE
      as.numeric(x[1]) / as.numeric(x[2])
    } else {
      as.numeric(x)
    }
  }
  x <- strsplit(x, "(\\s*,\\s*|\\s*;\\s*|\\s+)") %>%
    unlist() %>%
    strsplit("\\s*/\\s*") %>%
    sapply(check_frac)

  if (any_frac) {
    x <- MASS::fractions(x)
  }
  x
}
