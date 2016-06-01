basic_ui <-
	tagList(
	  navbarMenu("Basic",
	    "----", "Probability",
	    tabPanel("Probability calculator", uiOutput("prob_calc")),
	    tabPanel("Central Limit Theorem", uiOutput("clt")),
	    "----", "Means",
	    tabPanel("Single mean", uiOutput("single_mean")),
	    tabPanel("Compare means", uiOutput("compare_means")),
	    "----", "Proportions",
	    tabPanel("Single proportion", uiOutput("single_prop")),
	    tabPanel("Compare proportions", uiOutput("compare_props")),
	    "----", "Tables",
	    tabPanel("Goodness of fit", uiOutput("goodness")),
	    tabPanel("Cross-tabs", uiOutput("cross_tabs")),
	    tabPanel("Correlation", uiOutput("correlation"))
    )
  )

## ui for design menu in radiant
do.call(navbarPage,
  c("Radiant", getOption("radiant.nav_ui"), basic_ui, getOption("radiant.shared_ui"),
    help_menu("help_basic_ui"))
)
