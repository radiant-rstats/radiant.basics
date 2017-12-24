## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["Single mean"]] <-
  list("tabs_single_mean" = list("Summary" = "basics/single-mean/", "Plot" = "basics/single-mean/plot/"))
r_url_list[["Compare means"]] <-
  list("tabs_compare_means" = list("Summary" = "basics/compare-means/", "Plot" = "basics/compare-means/plot/"))
r_url_list[["Single proportion"]] <-
  list("tabs_single_prop" = list("Summary" = "basics/single-prop/", "Plot" = "basics/single-prop/plot/"))
r_url_list[["Compare proportions"]] <-
  list("tabs_compare_props" = list("Summary" = "basics/compare-props/", "Plot" = "basics/compare-props/plot/"))
r_url_list[["Goodness of fit"]] <-
  list("tabs_goodness" = list("Summary" = "basics/goodness/", "Plot" = "basics/goodness/plot/"))
r_url_list[["Cross-tabs"]] <-
  list("tabs_cross_tabs" = list("Summary" = "basics/cross-tabs/", "Plot" = "basics/cross-tabs/plot/"))
r_url_list[["Correlation"]] <-
  list("tabs_correlation" = list("Summary" = "basics/correlation/", "Plot" = "basics`/correlation/plot/"))
options(radiant.url.list = r_url_list)
rm(r_url_list)

## try http://127.0.0.1:3174/?url=basics/goodness/plot/&SSUID=local
## http://127.0.0.1:7407/?url=basics/compare-means/plot/&SSUID=local-a82049

## design menu
options(
  radiant.basics_ui =
    tagList(
      navbarMenu(
        "Basics",
        tags$head(
          tags$script(src = "www_basics/js/run_return.js")
        ),
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
)
