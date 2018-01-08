#' Launch radiant.basics in default browser
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
