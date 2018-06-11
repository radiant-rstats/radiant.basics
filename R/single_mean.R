#' Compare a sample mean to a population mean
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/single_mean.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param var The variable selected for the mean comparison
#' @param comp_value Population value to compare to the sample mean
#' @param alternative The alternative hypothesis ("two.sided", "greater", or "less")
#' @param conf_lev Span for the confidence interval
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of variables defined in single_mean as an object of class single_mean
#'
#' @examples
#' single_mean(diamonds, "price") %>% str()
#'
#' @seealso \code{\link{summary.single_mean}} to summarize results
#' @seealso \code{\link{plot.single_mean}} to plot results
#'
#' @export
single_mean <- function(
  dataset, var, comp_value = 0,
  alternative = "two.sided", conf_lev = .95,
  data_filter = ""
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, var, filt = data_filter, na.rm = FALSE)

  ## counting missing values
  miss <- n_missing(dataset)
  ## removing any missing values
  dataset <- na.omit(dataset)

  res <- t.test(dataset[[var]], mu = comp_value, alternative = alternative, conf.level = conf_lev) %>%
    tidy()

  dat_summary <- summarise_all(dataset,
    funs(
      diff = mean(.) - comp_value,
      se = se,
      mean = mean,
      sd = sd,
      n_obs = length
    )
  )
  dat_summary$n_missing <- miss

  as.list(environment()) %>% add_class("single_mean")
}

#' Summary method for the single_mean function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/single_mean.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{single_mean}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_mean(diamonds, "price")
#' summary(result)
#' diamonds %>% single_mean("price") %>% summary()
#'
#' @seealso \code{\link{single_mean}} to generate the results
#' @seealso \code{\link{plot.single_mean}} to plot results
#'
#' @export
summary.single_mean <- function(object, dec = 3, ...) {
  cat("Single mean test\n")
  cat("Data      :", object$df_name, "\n")
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("Filter    :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variable  :", object$var, "\n")
  cat("Confidence:", object$conf_lev, "\n")

  hyp_symbol <- c(
    "two.sided" = "not equal to",
    "less" = "<",
    "greater" = ">"
  )[object$alternative]

  cat("Null hyp. : the mean of", object$var, "=", object$comp_value, "\n")
  cat("Alt. hyp. : the mean of", object$var, "is", hyp_symbol, object$comp_value, "\n\n")

  ## determine lower and upper % for ci
  ci_perc <- ci_label(object$alternative, object$conf_lev)

  ## print summary statistics
  object$dat_summary[ ,-(1:2)] %>%
    # round(dec) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    format_df(dec = dec, mark = ",") %>%
    print(row.names = FALSE)
  cat("\n")

  res <- object$res
  res <- bind_cols(
    data.frame(
      diff = object$dat_summary[["diff"]],
      se = object$dat_summary[["se"]],
      stringsAsFactors = FALSE
    ),
    res[, -1]
  ) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    select(base::setdiff(colnames(.), c("method", "alternative"))) %>%
    mutate(parameter = as.integer(parameter))


  names(res) <- c("diff", "se", "t.value", "p.value", "df", ci_perc[1], ci_perc[2])
  res %<>% round(dec) ## restrict the number of decimals
  res$` ` <- sig_stars(res$p.value)
  if (res$p.value < .001) res$p.value <- "< .001"

  ## print statistics
  print(res, row.names = FALSE)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' Plot method for the single_mean function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/single_mean.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{single_mean}}
#' @param plots Plots to generate. "hist" shows a histogram of the data along with vertical lines that indicate the sample mean and the confidence interval. "simulate" shows the location of the sample mean and the comparison value (comp_value). Simulation is used to demonstrate the sampling variability in the data under the null-hypothesis
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_mean(diamonds, "price", comp_value = 3500)
#' plot(result, plots = c("hist", "simulate"))
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{summary.single_mean}} to summarize results
#'
#' @export
plot.single_mean <- function(
  x, plots = "hist",
  shiny = FALSE, custom = FALSE, ...
) {

  plot_list <- list()
  if ("hist" %in% plots) {
    bw <- x$dataset %>% range(na.rm = TRUE) %>% diff() %>% divide_by(10)

    plot_list[[which("hist" == plots)]] <-
      ggplot(x$dataset, aes_string(x = x$var)) +
      geom_histogram(fill = "blue", binwidth = bw, alpha = 0.5) +
      geom_vline(
        xintercept = x$comp_value,
        color = "red",
        linetype = "solid",
        size = 1
      ) +
      geom_vline(
        xintercept = x$res$estimate,
        color = "black",
        linetype = "solid",
        size = 1
      ) +
      geom_vline(
        xintercept = c(x$res$conf.low, x$res$conf.high),
        color = "black",
        linetype = "longdash",
        size = 0.5
      )
  }
  if ("simulate" %in% plots) {
    var <- x$dataset[[x$var]]
    nr <- length(var)

    simdat <- replicate(1000, mean(sample(var, nr, replace = TRUE))) %>%
      {(. - mean(.)) + x$comp_value} %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(x$var)

    cip <- ci_perc(simdat[[x$var]], x$alternative, x$conf_lev)

    bw <- simdat %>% range() %>% diff() %>% divide_by(20)

    plot_list[[which("simulate" == plots)]] <-
      ggplot(simdat, aes_string(x = x$var)) +
      geom_histogram(
        fill = "blue",
        binwidth = bw,
        alpha = 0.5
      ) +
      geom_vline(
        xintercept = x$comp_value,
        color = "red",
        linetype = "solid",
        size = 1
      ) +
      geom_vline(
        xintercept = x$res$estimate,
        color = "black",
        linetype = "solid",
        size = 1
      ) +
      geom_vline(
        xintercept = cip,
        color = "red",
        linetype = "longdash",
        size = 0.5
      ) +
      labs(title = paste0("Simulated means if null hyp. is true (", x$var, ")"))
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
