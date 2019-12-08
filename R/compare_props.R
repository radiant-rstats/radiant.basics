#' Compare sample proportions across groups
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/compare_props.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param var1 A grouping variable to split the data for comparisons
#' @param var2 The variable to calculate proportions for
#' @param levs The factor level selected for the proportion comparison
#' @param alternative The alternative hypothesis ("two.sided", "greater" or "less")
#' @param conf_lev Span of the confidence interval
#' @param comb Combinations to evaluate
#' @param adjust Adjustment for multiple comparisons ("none" or "bonf" for Bonferroni)
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of all variables defined in the function as an object of class compare_props
#'
#' @examples
#' compare_props(titanic, "pclass", "survived") %>% str()
#'
#' @seealso \code{\link{summary.compare_props}} to summarize results
#' @seealso \code{\link{plot.compare_props}} to plot results
#'
#' @export
compare_props <- function(
  dataset, var1, var2, levs = "",
  alternative = "two.sided", conf_lev = .95,
  comb = "", adjust = "none", data_filter = "",
  envir = parent.frame()
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  vars <- c(var1, var2)
  dataset <- get_data(dataset, vars, filt = data_filter, na.rm = FALSE, envir = envir) %>%
    mutate_all(as.factor)

  dataset <- dataset[!is.na(dataset[[1]]), , drop = FALSE]
  n_miss_df <- group_by_at(dataset, var1) %>%
    summarise_at(n_missing, .vars = var2) %>%
    set_colnames(c(var1, "n_miss"))
  dataset <- na.omit(dataset)

  if (length(levels(dataset[[var1]])) == nrow(dataset)) {
    return("Test requires multiple observations in each group. Please select another variable." %>%
      add_class("compare_props"))
  }

  lv <- levels(dataset[[var2]])
  if (levs != "") {
    if (levs %in% lv && lv[1] != levs) {
      dataset[[var2]] %<>% as.character %>% as.factor() %>% relevel(levs)
      lv <- levels(dataset[[var2]])
    }
  }

  ## check if there is variation in the data
  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
             add_class("compare_props"))
  }

  rn <- ""
  prop_input <- group_by_at(dataset, .vars = c(var1, var2)) %>%
    summarise(n = n()) %>%
    spread(!! var2, "n") %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    {
      rn <<- .[[1]] %>% as.character()
      select(., -1) %>%
        as.matrix() %>%
        set_rownames(rn)
    }

  prop_input[is.na(prop_input)] <- 0

  lv <- rownames(prop_input)
  cmb <- combn(lv, 2) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)

  rownames(cmb) <- cmb %>% apply(1, paste, collapse = ":")
  colnames(cmb) <- c("group1", "group2")

  if (!is_empty(comb)) {
    if (all(comb %in% rownames(cmb))) {
      cmb <- cmb[comb, ]
    } else {
      cmb <- cmb[1, ]
    }
  }

  res <- cmb
  res[, c("chisq.value", "p.value", "df", "ci_low", "ci_high", "sim")] <- 0
  for (i in 1:nrow(cmb)) {
    ind <- c(which(cmb[i, 1] == rownames(prop_input)), which(cmb[i, 2] == rownames(prop_input)))

    pinp <- prop_input[ind, ]

    res[i, c("chisq.value", "p.value", "df", "ci_low", "ci_high")] <-
      sshhr(prop.test(pinp, alternative = alternative, conf.level = conf_lev, correct = FALSE)) %>%
      tidy() %>%
      .[1, c("statistic", "p.value", "parameter", "conf.low", "conf.high")]

    ## calculate expected values
    E <- (rowSums(pinp) %*% t(colSums(pinp))) / sum(pinp)
    if (any(E < 5)) {
      res[i, "p.value"] <- sshhr(chisq.test(pinp, simulate.p.value = TRUE, B = 2000) %>% tidy() %>% .$p.value)
      res[i, "df"] <- NA
    }
  }

  if (adjust != "none") {
    res$p.value %<>% p.adjust(method = adjust)
  }

  ## adding significance stars
  res$sig_star <- sig_stars(res$p.value)

  ## from http://www.cookbook-r.com/Graphs/Plotting_props_and_error_bars_(ggplot2)/
  me_calc <- function(se, conf.lev = .95)
    se * qnorm(conf.lev / 2 + .5, lower.tail = TRUE)

  dat_summary <- data.frame(prop_input, check.names = FALSE, stringsAsFactors = FALSE) %>%
    mutate_if(is.numeric, as.integer) %>%
    mutate(
      p = .[[1]] / as.integer(rowSums(.[, 1:2])),
      n = as.integer(rowSums(.[, 1:2])),
      n_missing = 0,
      sd = sqrt(p * (1 - p)),
      se = sqrt(p * (1 - p) / n),
      me = me_calc(se, conf_lev)
    ) %>%
      set_rownames(rownames(prop_input)) %>%
      rownames_to_column(var = var1)

  dat_summary[[var1]] %<>% factor(., levels = .)
  dat_summary <- suppressWarnings(left_join(dat_summary, n_miss_df, by = var1)) %>%
    mutate(n_missing = n_miss) %>%
    select(-n_miss)
  vars <- paste0(vars, collapse = ", ")
  rm(i, me_calc, envir)
  as.list(environment()) %>% add_class("compare_props")
}

#' Summary method for the compare_props function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/compare_props.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{compare_props}}
#' @param show Show additional output (i.e., chisq.value, df, and confidence interval)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_props(titanic, "pclass", "survived")
#' summary(result)
#'
#' @seealso \code{\link{compare_props}} to calculate results
#' @seealso \code{\link{plot.compare_props}} to plot results
#'
#' @export
summary.compare_props <- function(object, show = FALSE, dec = 3, ...) {
  if (is.character(object)) return(object)

  cat("Pairwise proportion comparisons\n")
  cat("Data      :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter    :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables :", object$vars, "\n")
  cat("Level     :", object$levs, "in", object$var2, "\n")
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

  props <- object$dat_summary$p
  names(props) <- object$rn

  ## determine lower and upper % for ci
  ci_perc <- ci_label(object$alternative, object$conf_lev)

  res <- object$res
  res$`Alt. hyp.` <- paste(res$group1, hyp_symbol, res$group2, " ")
  res$`Null hyp.` <- paste(res$group1, "=", res$group2, " ")
  res$diff <- (props[res$group1 %>% as.character()] - props[res$group2 %>% as.character()]) %>% round(dec)

  res_sim <- is.na(res$df)
  if (show) {
    res <- res[, c("Null hyp.", "Alt. hyp.", "diff", "p.value", "chisq.value", "df", "ci_low", "ci_high", "sig_star")]
    res[, c("chisq.value", "ci_low", "ci_high")] %<>% format_df(dec, mark = ",")

    res$df[res_sim] <- "*1*"
    res <- rename(res, !!! setNames(c("ci_low", "ci_high"), ci_perc))
  } else {
    res <- res[, c("Null hyp.", "Alt. hyp.", "diff", "p.value", "sig_star")]
  }

  res <- rename(res, ` ` = "sig_star")
  res$p.value[res$p.value >= .001] %<>% round(dec)
  res$p.value[res$p.value < .001] <- "< .001"
  res$p.value[res_sim] %<>% paste0(" (2000 replicates)")
  print(res, row.names = FALSE, right = FALSE)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' Plot method for the compare_props function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/compare_props.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{compare_props}}
#' @param plots One or more plots of proportions ("bar" or "dodge")
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_props(titanic, "pclass", "survived")
#' plot(result, plots = c("bar", "dodge"))
#'
#' @seealso \code{\link{compare_props}} to calculate results
#' @seealso \code{\link{summary.compare_props}} to summarize results
#'
#' @export
plot.compare_props <- function(
  x, plots = "bar", shiny = FALSE,
  custom = FALSE, ...
) {

  if (is.character(x)) return(x)
  v1 <- colnames(x$dataset)[1]
  v2 <- colnames(x$dataset)[-1]
  lev_name <- x$levs

  ## from http://www.cookbook-r.com/Graphs/Plotting_props_and_error_bars_(ggplot2)/
  plot_list <- list()
  if ("bar" %in% plots) {
    ## use of `which` allows the user to change the order of the plots shown
    plot_list[[which("bar" == plots)]] <-
      ggplot(x$dat_summary, aes_string(x = v1, y = "p", fill = v1)) +
      geom_bar(stat = "identity", alpha = 0.5) +
      geom_errorbar(width = .1, aes(ymin = p - me, ymax = p + me)) +
      geom_errorbar(width = .05, aes(ymin = p - se, ymax = p + se), color = "blue") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::percent) +
      labs(y = paste0("Proportion of \"", lev_name, "\" in ", v2))
  }

  if ("dodge" %in% plots) {
    plot_list[[which("dodge" == plots)]] <- group_by_at(x$dataset, .vars = c(v1, v2)) %>%
      summarise(count = n()) %>%
      group_by_at(.vars = v1) %>%
      mutate(perc = count / sum(count)) %>%
      ggplot(aes_string(x = v1, y = "perc", fill = v2)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      labs(y = paste0("Proportions per level of ", v1))
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
