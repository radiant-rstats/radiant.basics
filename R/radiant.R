#' Launch Radiant in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @export
radiant.basics <- function() {
  if (!"package:radiant.basics" %in% search())
    if (!require(radiant.basics)) stop("Calling radiant.basics start function but radiant.basics is not installed.")
  runApp(system.file("app", package = "radiant.basics"), launch.browser = TRUE)
}
