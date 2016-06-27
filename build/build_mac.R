## build for mac
build_app <- function(app) {
	devtools::build(file.path("..",app))
	devtools::build(file.path("..",app), binary = TRUE)
}
sapply("radiant.basics", build_app)
