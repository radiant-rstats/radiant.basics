#' Launch Radiant in the default browser
#'
#' @details See \url{http://vnijs.github.io/radiant} for documentation and tutorials
#'
#' @export
radiant.basic <- function() {
  if (!"package:radiant.basic" %in% search())
    if (!require(radiant.basic)) stop("Calling radiant.basic start function but radiant.basic is not installed.")
  runApp(system.file("app", package = "radiant.basic"), launch.browser = TRUE)
}
