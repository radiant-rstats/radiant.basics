## ui for basics menu in radiant
navbar_proj(
  do.call(
    navbarPage,
    c(
      "Radiant",
      getOption("radiant.nav_ui"),
      getOption("radiant.basics_ui"),
      getOption("radiant.shared_ui"),
      help_menu("help_basics_ui")
    )
  )
)
