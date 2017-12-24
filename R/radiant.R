#' Launch radiant.basics in default browser or Rstudio Viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param run Run radiant.basics in an external browser ("browser") or in the Rstudio viewer ("viewer")
#'
#' @importFrom rstudioapi viewer
#'
#' @examples
#' \dontrun{
#' radiant.basics::radiant.basics()
#' radiant.basics::radiant.basics("viewer")
#' }
#'
#' @export
radiant.basics <- function(run = "browser") {
  if (!"package:radiant.basics" %in% search()) {
    if (!sshhr(require(radiant.basics))) {
      stop("\nCalling radiant.basics start function but radiant.basics is not installed.")
    }
  }
  run <- if (run == "viewer") {
    message("\nStarting radiant.basics in Rstudio Viewer ...")
    rstudioapi::viewer
  } else {
    message("\nStarting radiant.basics in default browser ...\n\nUse radiant.basics::radiant.basics(\"viewer\") to open radiant.basics in Rstudio Viewer")
    TRUE
  }
  suppressPackageStartupMessages(
    shiny::runApp(system.file("app", package = "radiant.basics"), launch.browser = run)
  )
}
