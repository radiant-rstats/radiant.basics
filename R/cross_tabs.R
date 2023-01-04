#' Evaluate associations between categorical variables
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/cross_tabs.html} for an example in Radiant
#'
#' @param dataset Dataset (i.e., a data.frame or table)
#' @param var1 A categorical variable
#' @param var2 A categorical variable
#' @param tab Table with frequencies as alternative to dataset
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of all variables used in cross_tabs as an object of class cross_tabs
#'
#' @examples
#' cross_tabs(newspaper, "Income", "Newspaper") %>% str()
#' table(select(newspaper, Income, Newspaper)) %>% cross_tabs(tab = .)
#'
#' @seealso \code{\link{summary.cross_tabs}} to summarize results
#' @seealso \code{\link{plot.cross_tabs}} to plot results
#'
#' @export
cross_tabs <- function(dataset, var1, var2, tab = NULL,
                       data_filter = "", envir = parent.frame()) {
  if (is.table(tab)) {
    df_name <- deparse(substitute(tab))

    if (missing(var1) || missing(var2)) {
      nm <- names(dimnames(tab))
      var1 <- nm[1]
      var2 <- nm[2]
    }

    if (is.empty(var1) || is.empty(var2)) {
      return("The provided table does not have dimension names. See ?cross_tabs for an example" %>%
        add_class("cross_tabs"))
    }
  } else {
    df_name <- if (!is_string(dataset)) deparse(substitute(dataset)) else dataset
    dataset <- get_data(dataset, c(var1, var2), filt = data_filter, envir = envir)

    ## Use simulated p-values when
    # http://stats.stackexchange.com/questions/100976/n-1-pearsons-chi-square-in-r
    # http://stats.stackexchange.com/questions/14226/given-the-power-of-computers-these-days-is-there-ever-a-reason-to-do-a-chi-squa/14230#14230
    # http://stats.stackexchange.com/questions/62445/rules-to-apply-monte-carlo-simulation-of-p-values-for-chi-squared-test

    not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
    if (length(not_vary) > 0) {
      return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
        add_class("cross_tabs"))
    }

    tab <- table(dataset[[var1]], dataset[[var2]])
    tab[is.na(tab)] <- 0
    tab <- tab[, colSums(tab) > 0] %>%
      (function(x) x[rowSums(x) > 0, ]) %>%
      as.table()
    ## dataset not needed in summary or plot
    rm(dataset)
  }

  cst <- sshhr(chisq.test(tab, correct = FALSE))

  ## adding the % deviation table
  cst$chi_sq <- with(cst, (observed - expected)^2 / expected)

  res <- tidy(cst) %>%
    mutate(parameter = as.integer(parameter))
  elow <- sum(cst$expected < 5)

  if (elow > 0) {
    res$p.value <- chisq.test(cst$observed, simulate.p.value = TRUE, B = 2000) %>%
      tidy() %>%
      .$p.value
    res$parameter <- paste0("*", res$parameter, "*")
  }

  rm(envir)

  as.list(environment()) %>% add_class("cross_tabs")
}

#' Summary method for the cross_tabs function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/cross_tabs.html} for an example in Radiant

#' @param object Return value from \code{\link{cross_tabs}}
#' @param check Show table(s) for variables var1 and var2. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "dev_perc" for the percentage difference between the observed and expected frequencies (i.e., (o - e) / e)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' result <- cross_tabs(newspaper, "Income", "Newspaper")
#' summary(result, check = c("observed", "expected", "chi_sq"))
#'
#' @seealso \code{\link{cross_tabs}} to calculate results
#' @seealso \code{\link{plot.cross_tabs}} to plot results
#'
#' @export
summary.cross_tabs <- function(object, check = "", dec = 2, ...) {
  if (is.character(object)) {
    return(object)
  }
  cat("Cross-tabs\n")
  cat("Data     :", object$df_name, "\n")
  if (!is.empty(object$data_filter)) {
    cat("Filter   :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables:", paste0(c(object$var1, object$var2), collapse = ", "), "\n")
  cat("Null hyp.: there is no association between", object$var1, "and", object$var2, "\n")
  cat("Alt. hyp.: there is an association between", object$var1, "and", object$var2, "\n")

  rnames <- object$cst$observed %>%
    rownames() %>%
    c(., "Total")
  cnames <- object$cst$observed %>%
    colnames() %>%
    c(., "Total")

  if ("observed" %in% check) {
    cat("\nObserved:\n")
    observed <- object$cst$observed %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      format(big.mark = ",", scientific = FALSE)

    names(attributes(observed)$dimnames) <- c(object$var1, object$var2)
    print(observed, quote = FALSE)
  }

  if ("expected" %in% check) {
    cat("\nExpected: (row total x column total) / total\n")
    expected <- object$cst$expected %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      round(dec) %>%
      format(big.mark = ",", scientific = FALSE)

    names(attributes(expected)$dimnames) <- c(object$var1, object$var2)
    print(expected, quote = FALSE)
  }

  if ("chi_sq" %in% check) {
    cat("\nContribution to chi-squared: (o - e)^2 / e\n")
    chi_sq <- object$cst$chi_sq %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      round(dec) %>%
      format(big.mark = ",", scientific = FALSE)

    names(attributes(chi_sq)$dimnames) <- c(object$var1, object$var2)
    print(chi_sq, quote = FALSE)
  }

  if ("dev_std" %in% check) {
    cat("\nDeviation standardized: (o - e) / sqrt(e)\n")
    resid <- round(object$cst$residuals, dec) ## standardized residuals
    names(attributes(resid)$dimnames) <- c(object$var1, object$var2)
    print(resid)
  }

  if ("row_perc" %in% check) {
    cat("\nRow percentages:\n")
    row_perc <- object$cst$observed %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      (function(x) x / x[, "Total"]) %>%
      round(dec)

    names(attributes(row_perc)$dimnames) <- c(object$var1, object$var2)
    print(row_perc)
  }

  if ("col_perc" %in% check) {
    cat("\nColumn percentages:\n")
    col_perc <- object$cst$observed %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      (function(x) t(x) / x["Total", ]) %>%
      t() %>%
      round(dec)

    names(attributes(col_perc)$dimnames) <- c(object$var1, object$var2)
    print(col_perc)
  }

  if ("perc" %in% check) {
    cat("\nProbability table:\n")
    perc <- object$cst$observed %>%
      rbind(colSums(.)) %>%
      set_rownames(rnames) %>%
      cbind(rowSums(.)) %>%
      set_colnames(cnames) %>%
      (function(x) x / x["Total", "Total"]) %>%
      round(dec)

    names(attributes(perc)$dimnames) <- c(object$var1, object$var2)
    print(perc)
  }

  object$res <- format_df(object$res, dec = dec + 1, mark = ",")

  if (object$res$p.value < .001) object$res$p.value <- "< .001"
  cat(paste0("\nChi-squared: ", object$res$statistic, " df(", object$res$parameter, "), p.value ", object$res$p.value), "\n\n")
  cat(paste(sprintf("%.1f", 100 * (object$elow / length(object$cst$expected))), "% of cells have expected values below 5\n"), sep = "")
  if (object$elow > 0) cat("p.value for chi-squared statistics obtained using simulation (2,000 replicates)")
}

#' Plot method for the cross_tabs function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/cross_tabs.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{cross_tabs}}
#' @param check Show plots for variables var1 and var2. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "row_perc", "col_perc", and "perc" for row, column, and table percentages respectively
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- cross_tabs(newspaper, "Income", "Newspaper")
#' plot(result, check = c("observed", "expected", "chi_sq"))
#'
#' @seealso \code{\link{cross_tabs}} to calculate results
#' @seealso \code{\link{summary.cross_tabs}} to summarize results
#'
#' @importFrom rlang .data
#'
#' @export
plot.cross_tabs <- function(x, check = "", shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) {
    return(x)
  }
  gather_table <- function(tab) {
    data.frame(tab, check.names = FALSE, stringsAsFactors = FALSE) %>%
      mutate(rnames = rownames(.)) %>%
      (function(x) sshhr(gather(x, "variable", "values", !!base::setdiff(colnames(x), "rnames"))))
  }

  plot_list <- list()
  if (is.empty(check)) check <- "observed"

  if ("observed" %in% check) {
    fact_names <- x$cst$observed %>%
      dimnames() %>%
      as.list()
    tab <- as.data.frame(x$cst$observed, check.names = FALSE, stringsAsFactors = FALSE)
    colnames(tab)[1:2] <- c(x$var1, x$var2)
    tab[[1]] %<>% factor(levels = fact_names[[1]])
    tab[[2]] %<>% factor(levels = fact_names[[2]])

    plot_list[["observed"]] <-
      ggplot(tab, aes(x = .data[[x$var2]], y = .data$Freq, fill = .data[[x$var1]])) +
      geom_bar(stat = "identity", position = "fill", alpha = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = paste("Observed frequencies for ", x$var2, " versus ", x$var1, sep = ""),
        x = x$var2,
        y = "",
        fill = x$var1
      )
  }

  if ("expected" %in% check) {
    fact_names <- x$cst$expected %>%
      dimnames() %>%
      as.list()
    tab <- gather_table(x$cst$expected)
    tab$rnames %<>% factor(levels = fact_names[[1]])
    tab$variable %<>% factor(levels = fact_names[[2]])
    plot_list[["expected"]] <-
      ggplot(tab, aes(x = .data$variable, y = .data$values, fill = .data$rnames)) +
      geom_bar(stat = "identity", position = "fill", alpha = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = paste("Expected frequencies for ", x$var2, " versus ", x$var1, sep = ""),
        x = x$var2,
        y = "",
        fill = x$var1
      )
  }

  if ("chi_sq" %in% check) {
    tab <- as.data.frame(x$cst$chi_sq, check.names = FALSE, stringsAsFactors = FALSE)
    colnames(tab)[1:2] <- c(x$var1, x$var2)
    plot_list[["chi_sq"]] <-
      ggplot(tab, aes(x = .data[[x$var2]], y = .data$Freq, fill = .data[[x$var1]])) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
      labs(
        title = paste("Contribution to chi-squared for ", x$var2, " versus ", x$var1, sep = ""),
        x = x$var2,
        y = ""
      )
  }

  if ("dev_std" %in% check) {
    tab <- as.data.frame(x$cst$residuals, check.names = FALSE, stringsAsFactors = FALSE)
    colnames(tab)[1:2] <- c(x$var1, x$var2)
    plot_list[["dev_std"]] <-
      ggplot(tab, aes(x = .data[[x$var2]], y = .data$Freq, fill = .data[[x$var1]])) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
      geom_hline(yintercept = c(-1.96, 1.96, -1.64, 1.64), color = "black", linetype = "longdash", linewidth = .5) +
      geom_text(x = 1, y = 2.11, label = "95%", vjust = 0) +
      geom_text(x = 1, y = 1.49, label = "90%", vjust = 1) +
      labs(
        title = paste("Deviation standardized for ", x$var2, " versus ", x$var1, sep = ""),
        x = x$var2,
        y = ""
      )
  }

  if ("row_perc" %in% check) {
    plot_list[["row_perc"]] <- as.data.frame(x$cst$observed, check.names = FALSE, stringsAsFactors = FALSE) %>%
      group_by_at(.vars = "Var1") %>%
      mutate(perc = Freq / sum(Freq)) %>%
      ggplot(aes(x = .data$Var2, y = .data$perc, fill = .data$Var1)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Row percentages",
        y = "Percentage",
        x = x$var2,
        fill = x$var1
      )
  }

  if ("col_perc" %in% check) {
    plot_list[["col_perc"]] <- as.data.frame(x$cst$observed, check.names = FALSE, stringsAsFactors = FALSE) %>%
      group_by_at(.vars = "Var2") %>%
      mutate(perc = Freq / sum(Freq)) %>%
      ggplot(aes(x = .data$Var2, y = .data$perc, fill = .data$Var1)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Column percentages",
        y = "Percentage",
        x = x$var2,
        fill = x$var1
      )
  }

  if ("perc" %in% check) {
    plot_list[["perc"]] <- as.data.frame(x$cst$observed, check.names = FALSE, stringsAsFactors = FALSE) %>%
      mutate(perc = Freq / sum(Freq)) %>%
      ggplot(aes(x = .data$Var2, y = .data$perc, fill = .data$Var1)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Table percentages",
        y = "Percentage",
        x = x$var2,
        fill = x$var1
      )
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = 1) %>%
        (function(x) if (isTRUE(shiny)) x else print(x))
    }
  }
}
