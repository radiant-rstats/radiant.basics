#' Launch radiant.basics in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.basics()
#' }
#' @export
radiant.basics <- function() radiant.data::launch(package = "radiant.basics", run = "browser")

#' Launch radiant.basics in an Rstudio window
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.basics_window()
#' }
#' @export
radiant.basics_window <- function() radiant.data::launch(package = "radiant.basics", run = "window")

#' Launch radiant.basics in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.basics_viewer()
#' }
#' @export
radiant.basics_viewer <- function() radiant.data::launch(package = "radiant.basics", run = "viewer")
