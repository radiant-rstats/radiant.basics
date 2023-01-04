#' Evaluate if sample data for a categorical variable is consistent with a hypothesized distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/goodness.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param var A categorical variable
#' @param p Hypothesized distribution as a number, fraction, or numeric vector. If unspecified, defaults to an even distribution
#' @param tab Table with frequencies as alternative to dataset
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of all variables used in goodness as an object of class goodness
#'
#' @examples
#' goodness(newspaper, "Income") %>% str()
#' goodness(newspaper, "Income", p = c(3 / 4, 1 / 4)) %>% str()
#' table(select(newspaper, Income)) %>% goodness(tab = .)
#'
#' @seealso \code{\link{summary.goodness}} to summarize results
#' @seealso \code{\link{plot.goodness}} to plot results
#'
#' @export
goodness <- function(dataset, var, p = NULL, tab = NULL,
                     data_filter = "", envir = parent.frame()) {
  if (is.table(tab)) {
    df_name <- deparse(substitute(tab))
    if (missing(var)) var <- "variable"
  } else {
    df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
    dataset <- get_data(dataset, var, filt = data_filter, envir = envir)

    ## creating and cleaning up the table
    tab <- table(dataset[[var]])
    tab[is.na(tab)] <- 0
    tab <- as.table(tab)
  }
  ## dataset not needed in summary or plot
  rm(dataset)

  if (is.empty(p)) {
    p <- rep(1 / length(tab), length(tab))
  } else if (is.numeric(p)) {
    if (length(p) == 1) p <- rep(p, length(tab))
  } else if (is.character(p)) {
    p <- gsub(",", " ", p) %>%
      strsplit("\\s+") %>%
      unlist() %>%
      strsplit("/")
    asNum <- function(x) ifelse(length(x) > 1, as.numeric(x[1]) / as.numeric(x[2]), as.numeric(x[1]))
    p <- sshhr(sapply(p, asNum))

    if (anyNA(p)) {
      return(paste0("Invalid inputs: ", paste0(p, collapse = ", ")) %>% add_class("goodness"))
    }

    lp <- length(p)
    lt <- length(tab)
    if (lt != lp && lt %% lp == 0) p <- rep(p, lt / lp)
  }

  if (!is.numeric(p) || sum(p) != 1) {
    return(
      paste0("Probabilities do not sum to 1 (", round(sum(p), 3), ")\nUse fractions if appropriate. Variable ", var, " has ", length(tab), " unique values.") %>%
        add_class("goodness")
    )
  }

  cst <- sshhr(chisq.test(tab, p = p, correct = FALSE))

  ## adding the chi-sq table
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

  as.list(environment()) %>% add_class("goodness")
}

#' Summary method for the goodness function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/goodness} for an example in Radiant
#'
#' @param object Return value from \code{\link{goodness}}
#' @param check Show table(s) for the selected variable (var). "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "dev_perc" for the percentage difference between the observed and expected frequencies (i.e., (o - e) / e)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' result <- goodness(newspaper, "Income", c(.3, .7))
#' summary(result, check = c("observed", "expected", "chi_sq"))
#' goodness(newspaper, "Income", c(1 / 3, 2 / 3)) %>% summary("observed")
#'
#' @seealso \code{\link{goodness}} to calculate results
#' @seealso \code{\link{plot.goodness}} to plot results
#'
#' @export
summary.goodness <- function(object, check = "", dec = 2, ...) {
  if (is.character(object)) {
    return(object)
  }

  cat("Goodness of fit test\n")
  cat("Data     :", object$df_name, "\n")
  if (!is.empty(object$data_filter)) {
    cat("Filter   :", gsub("\\n", "", object$data_filter), "\n")
  }
  if (length(object$var) > 0) {
    cat("Variable :", object$var, "\n")
  }
  cat("Specified:", object$p, "\n")
  cat("Null hyp.: the distribution of", object$var, "is consistent with the specified distribution\n")
  cat("Alt. hyp.: the distribution of", object$var, "is not consistent with the specified distribution\n")

  if ("observed" %in% check) {
    cat("\nObserved:\n")
    object$cst$observed %>%
      (function(x) {
        x["Total"] <- sum(x)
        x
      }) %>%
      format(big.mark = ",", scientific = FALSE) %>%
      print(quote = FALSE)
  }

  if ("expected" %in% check) {
    cat("\nExpected: total x p\n")
    object$cst$expected %>%
      (function(x) {
        x["Total"] <- sum(x)
        return(x)
      }) %>%
      round(dec) %>%
      format(big.mark = ",", scientific = FALSE) %>%
      print(quote = FALSE)
  }

  if ("chi_sq" %in% check) {
    cat("\nContribution to chi-squared: (o - e)^2 / e\n")
    object$cst$chi_sq %>%
      (function(x) {
        x["Total"] <- sum(x)
        return(x)
      }) %>%
      round(dec) %>%
      format(big.mark = ",", scientific = FALSE) %>%
      print(quote = FALSE)
  }

  if ("dev_std" %in% check) {
    cat("\nDeviation standardized: (o - e) / sqrt(e)\n")
    print(round(object$cst$residuals, dec))
  }

  object$res <- format_df(object$res, dec = dec + 1, mark = ",")

  if (object$res$p.value < .001) object$res$p.value <- "< .001"
  cat(paste0("\nChi-squared: ", object$res$statistic, " df(", object$res$parameter, "), p.value ", object$res$p.value), "\n\n")
  cat(paste(sprintf("%.1f", 100 * (object$elow / length(object$cst$expected))), "% of cells have expected values below 5\n"), sep = "")
  if (object$elow > 0) cat("p.value for chi-squared statistics obtained using simulation (2,000 replicates)")
}

#' Plot method for the goodness function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/goodness} for an example in Radiant
#'
#' @param x Return value from \code{\link{goodness}}
#' @param check Show plots for variable var. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), and "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e))
#' @param fillcol Color used for bar plots
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- goodness(newspaper, "Income")
#' plot(result, check = c("observed", "expected", "chi_sq"))
#' goodness(newspaper, "Income") %>% plot(c("observed", "expected"))
#'
#' @seealso \code{\link{goodness}} to calculate results
#' @seealso \code{\link{summary.goodness}} to summarize results
#'
#' @importFrom rlang .data
#'
#' @export
plot.goodness <- function(x, check = "", fillcol = "blue",
                          shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) {
    return(x)
  }
  plot_list <- list()
  if (is.empty(check)) check <- "observed"

  if ("observed" %in% check) {
    fact_names <- names(x$cst$observed)
    tab <- as.data.frame(x$cst$observed, check.names = FALSE, stringsAsFactors = FALSE)
    colnames(tab)[1] <- x$var
    tab[[1]] %<>% factor(levels = fact_names)
    tab[["Freq"]] %<>% {
      . / sum(.)
    }
    plot_list[["observed"]] <-
      ggplot(tab, aes(x = .data[[x$var]], y = .data$Freq)) +
      geom_bar(stat = "identity", alpha = 0.5, fill = fillcol) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = paste("Observed frequencies for", x$var),
        x = x$var,
        y = ""
      )
  }

  if ("expected" %in% check) {
    fact_names <- names(x$cst$expected)
    tab <- as.data.frame(x$cst$expected, check.names = FALSE, stringsAsFactors = FALSE)
    colnames(tab)[1] <- "Freq"
    tab[[x$var]] <- factor(rownames(tab), levels = rownames(tab))
    tab[["Freq"]] %<>% (function(x) x / sum(x))
    plot_list[["expected"]] <-
      ggplot(tab, aes(x = .data[[x$var]], y = .data$Freq)) +
      geom_bar(stat = "identity", alpha = 0.5, fill = fillcol) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = paste("Expected frequencies for", x$var),
        x = x$var,
        y = ""
      )
  }

  if ("chi_sq" %in% check) {
    tab <- as.data.frame(x$cst$chi_sq, check.names = FALSE, stringsAsFactors = FALSE)
    colnames(tab)[1] <- x$var
    plot_list[["chi_sq"]] <-
      ggplot(tab, aes(x = .data[[x$var]], y = .data$Freq)) +
      geom_bar(stat = "identity", alpha = 0.5, fill = fillcol) +
      labs(
        title = paste("Contribtion to chi-squared for", x$var),
        x = x$var,
        y = ""
      )
  }

  if ("dev_std" %in% check) {
    tab <- as.data.frame(x$cst$residuals, check.names = FALSE, stringsAsFactors = FALSE)
    mult <- max(abs(tab$Freq)) / 5
    colnames(tab)[1] <- x$var
    plot_list[["dev_std"]] <-
      ggplot(tab, aes(x = .data[[x$var]], y = .data$Freq)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5, fill = fillcol) +
      geom_hline(yintercept = c(-1.96, 1.96, -1.64, 1.64), color = "black", linetype = "longdash", linewidth = .5) +
      geom_text(x = 1, y = 2.11, label = "95%", vjust = 0) +
      geom_text(x = 1, y = 1.49, label = "90%", vjust = 1) +
      labs(
        title = paste("Deviation standardized for", x$var),
        x = x$var,
        y = ""
      )
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = 1) %>%
        (function(x) if (shiny) x else print(x))
    }
  }
}
