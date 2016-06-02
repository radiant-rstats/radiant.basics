## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["Single mean"]] <-
  list("tabs_single_mean" = list("Summary" = "basic/single-mean/", "Plot" = "basic/single-mean/plot/"))
r_url_list[["Compare means"]] <-
  list("tabs_compare_means" = list("Summary" = "basic/compare-means/", "Plot" = "basic/compare-means/plot/"))
r_url_list[["Single proportion"]] <-
  list("tabs_single_prop" = list("Summary" = "basic/single-prop/","Plot" = "basic/single-prop/plot/"))
r_url_list[["Compare proportions"]] <-
  list("tabs_compare_props" = list("Summary" = "basic/compare-props/", "Plot" = "basic/compare-props/plot/"))
r_url_list[["Cross-tabs"]] <-
  list("tabs_cross_tabs" = list("Summary" = "basic/cross-tabs/", "Plot" = "basic/cross-tabs/plot/"))
r_url_list[["Correlation"]] <-
  list("tabs_correlation" = list("Summary" = "basic/correlation/", "Plot" = "basic`/correlation/plot/"))
options(radiant.url.list = r_url_list); rm(r_url_list)

## design menu
basic_ui <-
	tagList(
	  navbarMenu("Basic",
	    "Probability",
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
