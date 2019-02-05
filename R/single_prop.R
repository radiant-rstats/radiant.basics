#' Compare a sample proportion to a population proportion
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/single_prop.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param var The variable selected for the proportion comparison
#' @param lev The factor level selected for the proportion comparison
#' @param comp_value Population value to compare to the sample proportion
#' @param alternative The alternative hypothesis ("two.sided", "greater", or "less")
#' @param conf_lev Span of the confidence interval
#' @param test bionomial exact test ("binom") or Z-test ("z")
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of variables used in single_prop as an object of class single_prop
#'
#' @examples
#' single_prop(titanic, "survived") %>% str()
#' single_prop(titanic, "survived", lev = "Yes", comp_value = 0.5,  alternative = "less") %>% str()
#'
#' @seealso \code{\link{summary.single_prop}} to summarize the results
#' @seealso \code{\link{plot.single_prop}} to plot the results
#'
#' @export
single_prop <- function(
  dataset, var, lev = "", comp_value = 0.5,
  alternative = "two.sided", conf_lev = .95,
  test = "binom", data_filter = ""
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, var, filt = data_filter, na.rm = FALSE) %>%
    mutate_all(as.factor)

  ## removing any missing values
  miss <- n_missing(dataset)
  dataset <- na.omit(dataset)

  levs <- levels(dataset[[var]])
  if (lev != "") {
    if (lev %in% levs && levs[1] != lev) {
      dataset[[var]] %<>% as.character %>% as.factor() %>% relevel(lev)
      levs <- levels(dataset[[var]])
    }
  } else {
    lev <- levs[1]
  }

  n <- nrow(dataset)
  ns <- sum(dataset == lev)
  p <- ns / n

  dat_summary <- data.frame(
    diff = p - comp_value,
    prop = p,
    mean = ns, ## or mean = n * p,
    sd = sqrt(n * p * (1 - p)),
    n = n,
    n_missing = miss,
    stringsAsFactors = FALSE
  )

  if (test == "z") {
    ## use z-test
    res <- sshhr(prop.test(
      ns, n, p = comp_value, alternative = alternative,
      conf.level = conf_lev, correct = FALSE
    )) %>%
      tidy()
    ## convert chi-square stat to a z-score
    res$statistic <- sqrt(res$statistic) * ifelse(res$estimate < comp_value, -1, 1)
  } else {
    ## use binom.test for exact
    res <- binom.test(
      ns, n, p = comp_value, alternative = alternative,
      conf.level = conf_lev
    ) %>%
      tidy()
  }

  as.list(environment()) %>% add_class("single_prop")
}

#' Summary method for the single_prop function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/single_prop.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{single_prop}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_prop(titanic, "survived", lev = "Yes", comp_value = 0.5,  alternative = "less")
#' summary(result)
#'
#' @seealso \code{\link{single_prop}} to generate the results
#' @seealso \code{\link{plot.single_prop}} to plot the results
#'
#' @export
summary.single_prop <- function(object, dec = 3, ...) {

  if (object$test == "z") {
    cat("Single proportion test (z-test)\n")
  } else {
    cat("Single proportion test (binomial exact)\n")
  }
  cat("Data      :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter    :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variable  :", object$var, "\n")
  cat("Level     :", object$lev, "in", object$var, "\n")
  cat("Confidence:", object$conf_lev, "\n")

  hyp_symbol <- c(
    "two.sided" = "not equal to",
    "less" = "<",
    "greater" = ">"
  )[object$alternative]

  cat("Null hyp. : the proportion of", object$lev, "in", object$var, "=", object$comp_value, "\n")
  cat("Alt. hyp. : the proportion of", object$lev, "in", object$var, hyp_symbol, object$comp_value, "\n\n")

  ## determine lower and upper % for ci
  ci_perc <- ci_label(object$alternative, object$conf_lev)

  ## print summary statistics
  object$dat_summary[-1] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    format_df(dec = dec, mark = ",") %>%
    print(row.names = FALSE)
  cat("\n")

  res <- object$res
  res <- bind_cols(data.frame(diff = object$dat_summary[["diff"]]), res[, -1]) %>%
    select(base::setdiff(colnames(.), c("parameter", "method", "alternative")))

  if (object$test == "z") {
    names(res) <- c("diff", "z.value", "p.value", ci_perc[1], ci_perc[2])
    res <- format_df(res, dec = dec, mark = ",") # restrict the number of decimals
  } else {
    names(res) <- c("diff", "ns", "p.value", ci_perc[1], ci_perc[2])
    res <- format_df(mutate(res, ns = as.integer(res$ns)), dec = dec, mark = ",") # restrict the number of decimals
  }
  res$` ` <- sig_stars(res$p.value)
  if (res$p.value < .001) res$p.value <- "< .001"

  ## print statistics
  print(res, row.names = FALSE)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' Plot method for the single_prop function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/single_prop.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{single_prop}}
#' @param plots Plots to generate. "bar" shows a bar chart of the data. The "simulate" chart shows the location of the sample proportion and the comparison value (comp_value). Simulation is used to demonstrate the sampling variability in the data under the null-hypothesis
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_prop(titanic, "survived", lev = "Yes", comp_value = 0.5,  alternative = "less")
#' plot(result, plots = c("bar", "simulate"))
#'
#' @seealso \code{\link{single_prop}} to generate the result
#' @seealso \code{\link{summary.single_prop}} to summarize the results
#'
#' @export
plot.single_prop <- function(
  x, plots = "bar",
  shiny = FALSE, custom = FALSE, ...
) {

  if (any(!plots %in% c("bar", "simulate"))) {
    stop("Available plot types for 'single_prop' are \"bar\" and \"simulate\"")
  }

  lev_name <- x$levs[1]
  plot_list <- list()
  if ("bar" %in% plots) {
    plot_list[[which("bar" == plots)]] <-
      ggplot(x$dataset, aes_string(x = x$var, fill = x$var)) +
      geom_bar(aes(y = (..count..) / sum(..count..)), alpha = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      theme(legend.position = "none") +
      labs(
        title = paste0("Single proportion: ", lev_name, " in ", x$var),
        y = ""
      )
  }
  if ("simulate" %in% plots) {
    simdat <- rbinom(1000, prob = x$comp_value, x$n) %>%
      divide_by(x$n) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(lev_name)

    cip <- ci_perc(simdat[[lev_name]], x$alternative, x$conf_lev) %>% set_names(NULL)

    bw <- simdat %>%
      range() %>%
      diff() %>%
      divide_by(20)

    # to avoid problems with levels that start with numbers or contain spaces
    # http://stackoverflow.com/questions/13445435/ggplot2-aes-string-fails-to-handle-names-starting-with-numbers-or-containing-s
    names(simdat) <- "col1"

    plot_list[[which("simulate" == plots)]] <-
      ggplot(simdat, aes(x = col1)) +
      geom_histogram(fill = "blue", binwidth = bw, alpha = 0.5) +
      geom_vline(
        xintercept = x$comp_value, color = "red",
        linetype = "solid", size = 1
      ) +
      geom_vline(
        xintercept = x$res$estimate, color = "black",
        linetype = "solid", size = 1
      ) +
      geom_vline(xintercept = cip, color = "red", linetype = "longdash", size = .5) +
      labs(
        title = paste0("Simulated proportions if null hyp. is true (", lev_name, " in ", x$var, ")"),
        x = paste0("Level ", lev_name, " in variable ", x$var)
      )
  }

  if (custom) {
    if (length(plot_list) == 1) {
      return(plot_list[[1]])
    } else {
      return(plot_list)
    }
  }

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>% {
    if (shiny) . else print(.)
  }
}
