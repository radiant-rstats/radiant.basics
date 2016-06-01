tmp <-c("radiant.data","psych")
tmp <- sapply(tmp, library, character.only = TRUE)
rm(tmp)

options(radiant.path.data = system.file(package = "radiant.data"))

# sourcing from radiant base, note that path is set in base/global.R
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = "UTF-8", local = TRUE)

ifelse (grepl("radiant.basic", getwd()) && file.exists("../inst") , "..", system.file(package = "radiant.basic")) %>%
  options(radiant.path.basic = .)

addResourcePath("figures_basic", "tools/help/figures/")

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
options(radiant.url.patterns = make_url_patterns())
