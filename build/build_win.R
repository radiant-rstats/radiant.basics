## build for windows
build_app <- function(app) {
	devtools::install(file.path("..",app))
	devtools::build(file.path("..",app), binary = TRUE)
}

sapply("radiant.basics", build_app)
