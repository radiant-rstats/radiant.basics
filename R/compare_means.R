#' Compare sample means
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/compare_means.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param var1 A numeric variable or factor selected for comparison
#' @param var2 One or more numeric variables for comparison. If var1 is a factor only one variable can be selected and the mean of this variable is compared across (factor) levels of var1
#' @param samples Are samples independent ("independent") or not ("paired")
#' @param alternative The alternative hypothesis ("two.sided", "greater" or "less")
#' @param conf_lev Span of the confidence interval
#' @param comb Combinations to evaluate
#' @param adjust Adjustment for multiple comparisons ("none" or "bonf" for Bonferroni)
#' @param test t-test ("t") or Wilcox ("wilcox")
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of all variables defined in the function as an object of class compare_means
#'
#' @examples
#' compare_means(diamonds, "cut", "price") %>% str()
#'
#' @seealso \code{\link{summary.compare_means}} to summarize results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
compare_means <- function(
  dataset, var1, var2, samples = "independent",
  alternative = "two.sided", conf_lev = .95,
  comb = "", adjust = "none", test = "t",
  data_filter = "", envir = parent.frame()
) {

  vars <- c(var1, var2)
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, na.rm = FALSE, envir = envir)

  ## in case : was used for var2
  vars <- colnames(dataset)

  if (is.numeric(dataset[[var1]])) {
    dataset %<>% gather("variable", "values", !! vars)
    dataset[["variable"]] %<>% factor(levels = vars)
    cname <- " "
  } else {
    if (is.character(dataset[[var1]])) dataset[[var1]] <- as.factor(dataset[[var1]])
    if (length(levels(dataset[[var1]])) == nrow(dataset)) {
      return("Test requires multiple observations in each group. Please select another variable." %>%
        add_class("compare_means"))
    }
    colnames(dataset) <- c("variable", "values")
    cname <- var1
  }

  ## needed with new tidyr
  dataset$variable %<>% as.factor()

  not_vary <- vars[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
             add_class("compare_means"))
  }

  ## resetting option to independent if the number of observations is unequal
  ## summary on factor gives counts
  if (samples == "paired") {
    if (summary(dataset[["variable"]]) %>% {max(.) != min(.)}) {
      samples <- "independent (obs. per level unequal)"
    }
  }

  levs <- levels(dataset[["variable"]])

  cmb <- combn(levs, 2) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  rownames(cmb) <- cmb %>%
    apply(1, paste, collapse = ":")
  colnames(cmb) <- c("group1", "group2")

  if (!is_empty(comb)) {
    if (all(comb %in% rownames(cmb))) {
      cmb <- cmb[comb, ]
    } else {
      cmb <- cmb[1, ]
    }
  }

  res <- cmb
  res[, c("t.value", "p.value", "df", "ci_low", "ci_high", "cis_low", "cis_high")] <- 0

  for (i in 1:nrow(cmb)) {
    sel <- sapply(cmb[i, ], as.character)
    x <- filter(dataset, variable == sel[1]) %>% .[["values"]]
    y <- filter(dataset, variable == sel[2]) %>% .[["values"]]

    res[i, c("t.value", "p.value", "df", "ci_low", "ci_high")] <-
      t.test(x, y, paired = samples == "paired", alternative = alternative, conf.level = conf_lev) %>%
      tidy() %>%
      .[1, c("statistic", "p.value", "parameter", "conf.low", "conf.high")]

    if (test != "t") {
      res[i, "p.value"] <-
        wilcox.test(
          x, y, paired = samples == "paired", alternative = alternative,
          conf.int = FALSE, conf.level = conf_lev
        ) %>%
        tidy() %>%
        .[1, "p.value"]
    }

    ## bootstrap confidence intervals
    ## seem almost identical, even with highly skewed data
    # nr_x <- length(x)
    # nr_y <- length(y)

    # sim_ci <-
    #   replicate(1000, mean(sample(x, nr_x, replace = TRUE)) -
    #                   mean(sample(y, nr_y, replace = TRUE))) %>%
    #   quantile(probs = {(1-conf_lev)/2} %>% c(., 1 - .))

    # res[i, c("cis_low", "cis_high")] <- sim_ci
  }

  if (adjust != "none") {
    res$p.value %<>% p.adjust(method = adjust)
  }

  ## adding significance stars
  res$sig_star <- sig_stars(res$p.value)

  ## from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
  me_calc <- function(se, n, conf.lev = .95)
    se * qt(conf.lev / 2 + .5, n - 1)

  dat_summary <- group_by_at(dataset, .vars = "variable") %>%
    summarise_all(
      list(
        mean = ~ mean(., na.rm = TRUE),
        n = length,
        n_missing = n_missing,
        sd = ~ sd(., na.rm = TRUE),
        se = ~ se(., na.rm = TRUE),
        me = ~ me_calc(se, n, conf_lev)
      )
    ) %>%
    rename(!!! setNames("variable", cname))

  vars <- paste0(vars, collapse = ", ")
  rm(x, y, sel, i, me_calc, envir)
  as.list(environment()) %>% add_class("compare_means")
}

#' Summary method for the compare_means function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/compare_means.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{compare_means}}
#' @param show Show additional output (i.e., t.value, df, and confidence interval)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_means(diamonds, "cut", "price")
#' summary(result)
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
summary.compare_means <- function(object, show = FALSE, dec = 3, ...) {
  if (is.character(object)) return(object)

  cat(paste0("Pairwise mean comparisons (", object$test, "-test)\n"))
  cat("Data      :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter    :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables :", object$vars, "\n")
  cat("Samples   :", object$samples, "\n")
  cat("Confidence:", object$conf_lev, "\n")
  cat("Adjustment:", if (object$adjust == "bonf") "Bonferroni" else "None", "\n\n")

  object$dat_summary %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    format_df(dec = dec, mark = ",") %>%
    print(row.names = FALSE)
  cat("\n")

  hyp_symbol <- c(
    "two.sided" = "not equal to",
    "less" = "<",
    "greater" = ">"
  )[object$alternative]

  means <- object$dat_summary$mean
  names(means) <- as.character(object$dat_summary[[1]])

  ## determine lower and upper % for ci
  ci_perc <- ci_label(object$alternative, object$conf_lev)

  mod <- object$res
  mod$`Alt. hyp.` <- paste(mod$group1, hyp_symbol, mod$group2, " ")
  mod$`Null hyp.` <- paste(mod$group1, "=", mod$group2, " ")
  mod$diff <- {means[mod$group1 %>% as.character()] - means[mod$group2 %>% as.character()]} %>%
    round(dec)

  if (show) {
    mod$se <- (mod$diff / mod$t.value) %>% round(dec)
    mod <- mod[, c("Null hyp.", "Alt. hyp.", "diff", "p.value", "se", "t.value", "df", "ci_low", "ci_high", "sig_star")]
    if (!is.integer(mod[["df"]])) mod[["df"]] %<>% round(dec)
    mod[, c("t.value", "ci_low", "ci_high")] %<>% round(dec)
    mod <- rename(mod, !!! setNames(c("ci_low", "ci_high"), ci_perc))
  } else {
    mod <- mod[, c("Null hyp.", "Alt. hyp.", "diff", "p.value", "sig_star")]
  }

  mod <- rename(mod, ` ` = "sig_star")
  mod$p.value <- round(mod$p.value, dec)
  mod$p.value[mod$p.value < .001] <- "< .001"
  print(mod, row.names = FALSE, right = FALSE)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' Plot method for the compare_means function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/compare_means.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{compare_means}}
#' @param plots One or more plots ("bar", "density", "box", or "scatter")
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_means(diamonds, "cut", "price")
#' plot(result, plots = c("bar", "density"))
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{summary.compare_means}} to summarize results
#'
#' @export
plot.compare_means <- function(x, plots = "scatter", shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) return(x)
  cn <- colnames(x$dataset)
  v1 <- cn[1]
  v2 <- cn[-1]

  ## cname is equal to " " when the xvar is numeric
  if (is_empty(x$cname)) {
    var1 <- v1
    var2 <- v2
  } else {
    var1 <- x$var1
    var2 <- x$var2
  }

  ## from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
  plot_list <- list()
  if ("bar" %in% plots) {
    colnames(x$dat_summary)[1] <- "variable"
    ## use of `which` allows the user to change the order of the plots shown
    plot_list[[which("bar" == plots)]] <-
      ggplot(
        x$dat_summary,
        aes_string(x = "variable", y = "mean", fill = "variable")
      ) +
      geom_bar(stat = "identity") +
      geom_errorbar(width = .1, aes(ymin = mean - me, ymax = mean + me)) +
      geom_errorbar(width = .05, aes(ymin = mean - se, ymax = mean + se), color = "blue") +
      theme(legend.position = "none") +
      labs(x = var1, y = paste0(var2, " (mean)"))
  }

  ## graphs on full data
  if ("box" %in% plots) {
    plot_list[[which("box" == plots)]] <-
      visualize(x$dataset, xvar = v1, yvar = v2, type = "box", custom = TRUE) +
      theme(legend.position = "none") +
      labs(x = var1, y = var2)
  }

  if ("density" %in% plots) {
    plot_list[[which("density" == plots)]] <-
      visualize(x$dataset, xvar = v2, type = "density", fill = v1, custom = TRUE) +
      labs(x = var2) +
      guides(fill = guide_legend(title = var1))
  }

  if ("scatter" %in% plots) {
    plot_list[[which("scatter" == plots)]] <-
      visualize(x$dataset, xvar = v1, yvar = v2, type = "scatter", check = "jitter", alpha = 0.3, custom = TRUE) +
      labs(x = var1, y = paste0(var2, " (mean)"))
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = 1) %>%
        {if (shiny) . else print(.)}
    }
  }
}
