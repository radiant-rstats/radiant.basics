#' Launch Radiant in the default browser
#'
#' @details See \url{http://vnijs.github.io/radiant} for documentation and tutorials
#'
#' @export
radiant.basics <- function() {
  if (!"package:radiant.basics" %in% search())
    if (!require(radiant.basics)) stop("Calling radiant.basics start function but radiant.basics is not installed.")
  runApp(system.file("app", package = "radiant.basics"), launch.browser = TRUE)
}
