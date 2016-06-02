## ui for design menu in radiant
do.call(navbarPage,
  c("Radiant", getOption("radiant.nav_ui"), basic_ui, getOption("radiant.shared_ui"),
    help_menu("help_basic_ui"))
)
