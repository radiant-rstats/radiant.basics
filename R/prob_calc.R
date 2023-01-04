#' Probability calculator for the normal distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param mean Mean
#' @param stdev Standard deviation
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @examples
#' prob_norm(mean = 0, stdev = 1, ub = 0)
#'
#' @seealso \code{\link{summary.prob_norm}} to summarize results
#' @seealso \code{\link{plot.prob_norm}} to plot results
#'
#' @export
prob_norm <- function(mean, stdev, lb = NA, ub = NA,
                      plb = NA, pub = NA, dec = 3) {
  p_ub <- pnorm(ub, mean, stdev)
  p_lb <- pnorm(lb, mean, stdev)
  p_int <- max(p_ub - p_lb, 0) %>% round(dec)

  p_ub %<>% round(dec)
  p_lb %<>% round(dec)

  if (!is.na(pub)) {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
  }

  if (!is.na(plb)) {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
  }

  v_ub <- qnorm(pub, mean, stdev) %>% round(dec)
  v_lb <- qnorm(plb, mean, stdev) %>% round(dec)

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(plb) && !is.na(pub)) {
    if (plb > pub) {
      plb <- pub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  as.list(environment()) %>% add_class("prob_norm")
}

#' Plot method for the probability calculator (normal)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_norm}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- prob_norm(mean = 0, stdev = 1, ub = 0)
#' plot(result)
#'
#' @seealso \code{\link{prob_norm}} to calculate results
#' @seealso \code{\link{summary.prob_norm}} to summarize results
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_norm <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }

  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$v_lb
    ub <- x$v_ub
  }

  mean <- x$mean
  stdev <- x$stdev

  limits <- c(mean - 3 * stdev, mean + 3 * stdev)

  dnorm_limit <- function(x) {
    y <- dnorm(x, mean = mean, sd = stdev)
    y[x < lb | x > ub] <- 0
    y
  }

  dnorm_lb <- function(x) {
    if (is.na(lb)) {
      return(0)
    }
    y <- dnorm(x, mean = mean, sd = stdev)
    y[x > lb] <- 0
    y
  }

  dnorm_ub <- function(x) {
    if (is.na(ub)) {
      return(0)
    }
    y <- dnorm(x, mean = mean, sd = stdev)
    y[x < ub] <- 0
    y
  }

  dnorm_lines <- c(ub, lb) %>% na.omit()
  if (length(dnorm_lines) == 0) dnorm_lines <- c(-Inf, Inf)

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(data.frame(x = limits), aes(x = .data$x)) +
    stat_function(fun = stats::dnorm, args = list(mean = mean, sd = stdev)) +
    stat_function(fun = dnorm_limit, geom = "area", fill = "blue", alpha = 0.5, n = 501) +
    stat_function(fun = dnorm_lb, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    stat_function(fun = dnorm_ub, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    geom_vline(xintercept = dnorm_lines, color = "black", linetype = "dashed", linewidth = .5) +
    labs(x = "", y = "")

  sshhr(plt)
}

#' Summary method for the probability calculator (normal)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_norm}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- prob_norm(mean = 0, stdev = 1, ub = 0)
#' summary(result)
#'
#' @seealso \code{\link{prob_norm}} to calculate results
#' @seealso \code{\link{plot.prob_norm}} to plot results
#'
#' @export
summary.prob_norm <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: Normal\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))

  cat("Mean        :", round(mean, dec), "\n")
  cat("St. dev     :", round(stdev, dec), "\n")

  if (type == "values") {
    cat("Lower bound :", if (is.na(lb)) "-Inf" else lb, "\n")
    cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X < ", lb, ") = ", p_lb, "\n"))
        cat(paste0("P(X > ", lb, ") = ", round(1 - p_lb, dec), "\n"))
      }

      if (!is.na(ub)) {
        cat(paste0("P(X < ", ub, ") = ", p_ub, "\n"))
        cat(paste0("P(X > ", ub, ") = ", round(1 - p_ub, dec), "\n"))
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " < X < ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " < X < ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    pub <- if (is.na(pub)) 2 else pub
    plb <- if (is.na(plb)) -1 else plb

    cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
    cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

    if (pub <= 1 || plb >= 0) {
      cat("\n")

      if (plb >= 0) {
        cat(paste0("P(X < ", v_lb, ") = ", plb, "\n"))
        cat(paste0("P(X > ", v_lb, ") = ", round(1 - plb, dec), "\n"))
      }

      if (pub <= 1) {
        cat(paste0("P(X < ", v_ub, ") = ", pub, "\n"))
        cat(paste0("P(X > ", v_ub, ") = ", round(1 - pub, dec), "\n"))
      }

      if (pub <= 1 && plb >= 0) {
        cat(paste0("P(", v_lb, " < X < ", v_ub, ")     = ", pub - plb, "\n"))
        cat(paste0("1 - P(", v_lb, " < X < ", v_ub, ") = ", round(1 - (pub - plb), dec), "\n"))
      }
    }
  }
}

#' Probability calculator for the log normal distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param meanlog Mean of the distribution on the log scale
#' @param sdlog Standard deviation of the distribution on the log scale
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_lnorm}} to summarize results
#' @seealso \code{\link{plot.prob_lnorm}} to plot results
#'
#' @examples
#' prob_lnorm(meanlog = 0, sdlog = 1, lb = 0, ub = 1)
#'
#' @export
prob_lnorm <- function(meanlog, sdlog, lb = NA, ub = NA,
                       plb = NA, pub = NA, dec = 3) {
  p_ub <- plnorm(ub, meanlog, sdlog)
  p_lb <- plnorm(lb, meanlog, sdlog)
  p_int <- max(p_ub - p_lb, 0) %>% round(dec)

  p_ub %<>% round(dec)
  p_lb %<>% round(dec)

  if (!is.na(pub)) {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
  }

  if (!is.na(plb)) {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
  }

  v_ub <- qlnorm(pub, meanlog, sdlog) %>% round(dec)
  v_lb <- qlnorm(plb, meanlog, sdlog) %>% round(dec)

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(plb) && !is.na(pub)) {
    if (plb > pub) {
      plb <- pub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  as.list(environment()) %>% add_class("prob_lnorm")
}

#' Plot method for the probability calculator (log normal)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_norm}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_lnorm}} to calculate results
#' @seealso \code{\link{plot.prob_lnorm}} to plot results
#'
#' @examples
#' result <- prob_lnorm(meanlog = 0, sdlog = 1, lb = 0, ub = 1)
#' plot(result, type = "values")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_lnorm <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }

  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$v_lb
    ub <- x$v_ub
  }

  meanlog <- x$meanlog
  sdlog <- x$sdlog

  # limits <- c(meanlog - 3 * sdlog, meanlog + 3 * sdlog)
  limits <- c(0, meanlog + 3 * sdlog)

  dlnorm_limit <- function(x) {
    y <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)
    y[x < lb | x > ub] <- 0
    y
  }

  dlnorm_lb <- function(x) {
    if (is.na(lb)) {
      return(0)
    }
    y <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)
    y[x > lb] <- 0
    y
  }

  dlnorm_ub <- function(x) {
    if (is.na(ub)) {
      return(0)
    }
    y <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)
    y[x < ub] <- 0
    y
  }

  dlnorm_lines <- c(ub, lb) %>% na.omit()
  if (length(dlnorm_lines) == 0) dlnorm_lines <- c(-Inf, Inf)

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(data.frame(x = limits), aes(x = .data$x)) +
    stat_function(fun = stats::dlnorm, args = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_function(fun = dlnorm_limit, geom = "area", fill = "blue", alpha = 0.5, n = 501) +
    stat_function(fun = dlnorm_lb, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    stat_function(fun = dlnorm_ub, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    geom_vline(xintercept = dlnorm_lines, color = "black", linetype = "dashed", linewidth = .5) +
    labs(x = "", y = "")

  sshhr(plt)
}

#' Summary method for the probability calculator (log normal)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_norm}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_lnorm}} to calculate results
#' @seealso \code{\link{plot.prob_lnorm}} to summarize results
#'
#' @examples
#' result <- prob_lnorm(meanlog = 0, sdlog = 1, lb = 0, ub = 1)
#' summary(result, type = "values")
#'
#' @export
summary.prob_lnorm <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: Log normal\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))

  cat("Mean log    :", round(meanlog, dec), "\n")
  cat("St. dev log :", round(sdlog, dec), "\n")

  if (type == "values") {
    cat("Lower bound :", if (is.na(lb)) "-Inf" else lb, "\n")
    cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X < ", lb, ") = ", p_lb, "\n"))
        cat(paste0("P(X > ", lb, ") = ", round(1 - p_lb, dec), "\n"))
      }

      if (!is.na(ub)) {
        cat(paste0("P(X < ", ub, ") = ", p_ub, "\n"))
        cat(paste0("P(X > ", ub, ") = ", round(1 - p_ub, dec), "\n"))
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " < X < ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " < X < ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    pub <- if (is.na(pub)) 2 else pub
    plb <- if (is.na(plb)) -1 else plb

    cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
    cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

    if (pub <= 1 || plb >= 0) {
      cat("\n")

      if (plb >= 0) {
        cat(paste0("P(X < ", v_lb, ") = ", plb, "\n"))
        cat(paste0("P(X > ", v_lb, ") = ", round(1 - plb, dec), "\n"))
      }

      if (pub <= 1) {
        cat(paste0("P(X < ", v_ub, ") = ", pub, "\n"))
        cat(paste0("P(X > ", v_ub, ") = ", round(1 - pub, dec), "\n"))
      }

      if (pub <= 1 && plb >= 0) {
        cat(paste0("P(", v_lb, " < X < ", v_ub, ")     = ", pub - plb, "\n"))
        cat(paste0("1 - P(", v_lb, " < X < ", v_ub, ") = ", round(1 - (pub - plb), dec), "\n"))
      }
    }
  }
}

#' Probability calculator for the t-distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param df Degrees of freedom
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_tdist}} to summarize results
#' @seealso \code{\link{plot.prob_tdist}} to plot results
#'
#' @examples
#' prob_tdist(df = 10, ub = 2.228)
#'
#' @export
prob_tdist <- function(df, lb = NA, ub = NA,
                       plb = NA, pub = NA, dec = 3) {
  p_ub <- pt(ub, df)
  p_lb <- pt(lb, df)
  p_int <- max(p_ub - p_lb, 0)

  p_ub %<>% round(dec)
  p_lb %<>% round(dec)
  p_int %<>% round(dec)

  if (!is.na(pub)) {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
  }

  if (!is.na(plb)) {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
  }

  v_ub <- qt(pub, df)
  v_lb <- qt(plb, df)

  v_ub %<>% round(dec)
  v_lb %<>% round(dec)

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(plb) && !is.na(pub)) {
    if (plb > pub) {
      plb <- pub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  as.list(environment()) %>% add_class("prob_tdist")
}

#' Plot method for the probability calculator (t-distribution)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_tdist}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_tdist}} to calculate results
#' @seealso \code{\link{summary.prob_tdist}} to summarize results
#'
#' @examples
#' result <- prob_tdist(df = 10, ub = 2.228)
#' plot(result, type = "values")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_tdist <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }
  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$v_lb
    ub <- x$v_ub
  }

  df <- x$df

  limits <- c(-3, 3)
  dt_limit <- function(x) {
    y <- dt(x, df = df)
    y[x < lb | x > ub] <- 0
    y
  }

  dt_lb <- function(x) {
    if (is.na(lb)) {
      return(0)
    }
    y <- dt(x, df = df)
    y[x > lb] <- 0
    y
  }

  dt_ub <- function(x) {
    if (is.na(ub)) {
      return(0)
    }
    y <- dt(x, df = df)
    y[x < ub] <- 0
    y
  }

  dt_lines <- c(ub, lb) %>% na.omit()
  if (length(dt_lines) == 0) dt_lines <- c(-Inf, Inf)

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(data.frame(x = limits), aes(x = .data$x)) +
    stat_function(fun = stats::dt, args = list(df = df)) +
    stat_function(fun = dt_limit, geom = "area", fill = "blue", alpha = 0.5, n = 501) +
    stat_function(fun = dt_lb, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    stat_function(fun = dt_ub, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    geom_vline(xintercept = dt_lines, color = "black", linetype = "dashed", linewidth = .5) +
    labs(x = "", y = "")

  sshhr(plt)
}

#' Summary method for the probability calculator (t-distribution)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_tdist}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_tdist}} to calculate results
#' @seealso \code{\link{plot.prob_tdist}} to plot results
#'
#' @examples
#' result <- prob_tdist(df = 10, ub = 2.228)
#' summary(result, type = "values")
#'
#' @export
summary.prob_tdist <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: t\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }

  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))
  n <- df + 1

  cat("Df          :", df, "\n")
  cat("Mean        :", 0, "\n")
  cat("St. dev     :", ifelse(n > 2, round(n / (n - 2), dec), "NA"), "\n")

  if (type == "values") {
    cat("Lower bound :", if (is.na(lb)) "-Inf" else lb, "\n")
    cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X < ", lb, ") = ", p_lb, "\n"))
        cat(paste0("P(X > ", lb, ") = ", round(1 - p_lb, dec), "\n"))
      }

      if (!is.na(ub)) {
        cat(paste0("P(X < ", ub, ") = ", p_ub, "\n"))
        cat(paste0("P(X > ", ub, ") = ", round(1 - p_ub, dec), "\n"))
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " < X < ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " < X < ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    pub <- if (is.na(pub)) 2 else pub
    plb <- if (is.na(plb)) -1 else plb

    cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
    cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

    if (pub <= 1 || plb >= 0) {
      cat("\n")

      if (plb >= 0) {
        cat(paste0("P(X < ", v_lb, ") = ", plb, "\n"))
        cat(paste0("P(X > ", v_lb, ") = ", round(1 - plb, dec), "\n"))
      }

      if (pub <= 1) {
        cat(paste0("P(X < ", v_ub, ") = ", pub, "\n"))
        cat(paste0("P(X > ", v_ub, ") = ", round(1 - pub, dec), "\n"))
      }

      if (pub <= 1 && plb >= 0) {
        cat(paste0("P(", v_lb, " < X < ", v_ub, ")     = ", pub - plb, "\n"))
        cat(paste0("1 - P(", v_lb, " < X < ", v_ub, ") = ", round(1 - (pub - plb), dec), "\n"))
      }
    }
  }
}

#' Probability calculator for the F-distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param df1 Degrees of freedom
#' @param df2 Degrees of freedom
#' @param lb Lower bound (default is 0)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_fdist}} to summarize results
#' @seealso \code{\link{plot.prob_fdist}} to plot results
#'
#' @examples
#' prob_fdist(df1 = 10, df2 = 10, ub = 2.978)
#'
#' @export
prob_fdist <- function(df1, df2, lb = NA, ub = NA,
                       plb = NA, pub = NA, dec = 3) {
  if (!is_not(lb) && lb < 0) lb <- 0
  if (!is_not(ub) && ub < 0) ub <- 0

  p_ub <- pf(ub, df1, df2)
  p_lb <- pf(lb, df1, df2)
  p_int <- max(p_ub - p_lb, 0)

  p_ub %<>% round(dec)
  p_lb %<>% round(dec)
  p_int %<>% round(dec)

  if (!is.na(pub)) {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
  }

  if (!is.na(plb)) {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
  }

  v_ub <- qf(pub, df1, df2)
  v_lb <- qf(plb, df1, df2)

  v_ub %<>% round(dec)
  v_lb %<>% round(dec)

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(plb) && !is.na(pub)) {
    if (plb > pub) {
      plb <- pub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  as.list(environment()) %>% add_class("prob_fdist")
}

#' Plot method for the probability calculator (F-distribution)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_fdist}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_fdist}} to calculate results
#' @seealso \code{\link{summary.prob_fdist}} to summarize results
#'
#' @examples
#' result <- prob_fdist(df1 = 10, df2 = 10, ub = 2.978)
#' plot(result, type = "values")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_fdist <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }
  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$v_lb
    ub <- x$v_ub
  }

  df1 <- x$df1
  df2 <- x$df2

  limits <- c(
    floor(qf(0.01, df1 = df1, df2 = df2)),
    ceiling(qf(1 - 0.01, df1 = df1, df2 = df2))
  )

  dat <- data.frame(
    x = limits,
    Probability = df(limits, df1 = df1, df2 = df2),
    df1 = df1,
    df2 = df2,
    stringsAsFactors = FALSE
  )

  df_line <- function(x) df(x, df1 = df1, df2 = df2)

  df_limit <- function(x) {
    y <- df(x, df1 = df1, df2 = df2)
    y[x < lb | x > ub] <- 0
    y
  }

  df_lb <- function(x) {
    if (is.na(lb)) {
      return(0)
    }
    y <- df(x, df1 = df1, df2 = df2)
    y[x > lb] <- 0
    y
  }

  df_ub <- function(x) {
    if (is.na(ub)) {
      return(0)
    }
    y <- df(x, df1 = df1, df2 = df2)
    y[x < ub] <- 0
    y
  }

  vlines <- c(ub, lb) %>% na.omit()
  if (length(vlines) == 0) vlines <- c(-Inf, Inf)

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(dat, aes(x = .data$x)) +
    stat_function(fun = df_line, geom = "line") +
    stat_function(fun = df_limit, geom = "area", fill = "blue", alpha = 0.5, n = 501) +
    stat_function(fun = df_lb, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    stat_function(fun = df_ub, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    geom_vline(xintercept = vlines, color = "black", linetype = "dashed", linewidth = 0.5) +
    labs(x = "", y = "")

  sshhr(plt)
}

#' Summary method for the probability calculator (F-distribution)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_fdist}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_fdist}} to calculate results
#' @seealso \code{\link{plot.prob_fdist}} to plot results
#'
#' @examples
#' result <- prob_fdist(df1 = 10, df2 = 10, ub = 2.978)
#' summary(result, type = "values")
#'
#' @export
summary.prob_fdist <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: F\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))


  cat("Df 1        :", df1, "\n")
  cat("Df 2        :", df2, "\n")
  m <- if (df2 > 2) round(df2 / (df2 - 2), dec) else "NA"
  variance <- if (df2 > 4) {
    round((2 * df2^2 * (df1 + df2 - 2)) / (df1 * (df2 - 2)^2 * (df2 - 4)), dec)
  } else {
    "NA"
  }
  cat("Mean        :", m, "\n")
  cat("Variance    :", variance, "\n")

  if (type == "values") {
    cat("Lower bound :", if (is.na(lb)) "0" else lb, "\n")
    cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X < ", lb, ") = ", p_lb, "\n"))
        cat(paste0("P(X > ", lb, ") = ", round(1 - p_lb, dec), "\n"))
      }

      if (!is.na(ub)) {
        cat(paste0("P(X < ", ub, ") = ", p_ub, "\n"))
        cat(paste0("P(X > ", ub, ") = ", round(1 - p_ub, dec), "\n"))
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " < X < ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " < X < ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    pub <- if (is.na(pub)) 2 else pub
    plb <- if (is.na(plb)) -1 else plb

    cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
    cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

    if (pub <= 1 || plb >= 0) {
      cat("\n")

      if (plb >= 0) {
        cat(paste0("P(X < ", v_lb, ") = ", plb, "\n"))
        cat(paste0("P(X > ", v_lb, ") = ", round(1 - plb, dec), "\n"))
      }

      if (pub <= 1) {
        cat(paste0("P(X < ", v_ub, ") = ", pub, "\n"))
        cat(paste0("P(X > ", v_ub, ") = ", round(1 - pub, dec), "\n"))
      }

      if (pub <= 1 && plb >= 0) {
        cat(paste0("P(", v_lb, " < X < ", v_ub, ")     = ", pub - plb, "\n"))
        cat(paste0("1 - P(", v_lb, " < X < ", v_ub, ") = ", round(1 - (pub - plb), dec), "\n"))
      }
    }
  }
}

#' Probability calculator for the chi-squared distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param df Degrees of freedom
#' @param lb Lower bound (default is 0)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_chisq}} to summarize results
#' @seealso \code{\link{plot.prob_chisq}} to plot results
#'
#' @examples
#' prob_chisq(df = 1, ub = 3.841)
#'
#' @export
prob_chisq <- function(df, lb = NA, ub = NA, plb = NA,
                       pub = NA, dec = 3) {
  if (!is_not(lb) && lb < 0) lb <- 0
  if (!is_not(ub) && ub < 0) ub <- 0

  p_ub <- pchisq(ub, df)
  p_lb <- pchisq(lb, df)
  p_int <- max(p_ub - p_lb, 0)

  p_ub %<>% round(dec)
  p_lb %<>% round(dec)
  p_int %<>% round(dec)

  if (!is.na(pub)) {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
  }

  if (!is.na(plb)) {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
  }

  v_ub <- qchisq(pub, df)
  v_lb <- qchisq(plb, df)

  v_ub %<>% round(dec)
  v_lb %<>% round(dec)

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(plb) && !is.na(pub)) {
    if (plb > pub) {
      plb <- pub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  as.list(environment()) %>% add_class("prob_chisq")
}

#' Plot method for the probability calculator (Chi-squared distribution)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_chisq}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_chisq}} to calculate results
#' @seealso \code{\link{summary.prob_chisq}} to summarize results
#'
#' @examples
#' result <- prob_chisq(df = 1, ub = 3.841)
#' plot(result, type = "values")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_chisq <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }

  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$v_lb
    ub <- x$v_ub
  }

  df <- x$df

  limits <- c(
    floor(qchisq(0.001, df = df)),
    ceiling(qchisq(1 - 0.001, df = df))
  )

  dat <- data.frame(
    x = limits,
    Probability = dchisq(limits, df = df),
    df = df,
    stringsAsFactors = FALSE
  )

  dchisq_limit <- function(x) {
    y <- dchisq(x, df = df)
    y[x < lb | x > ub] <- 0
    y
  }

  dchisq_lb <- function(x) {
    if (is.na(lb)) {
      return(0)
    }
    y <- dchisq(x, df = df)
    y[x > lb] <- 0
    y
  }

  dchisq_ub <- function(x) {
    if (is.na(ub)) {
      return(0)
    }
    y <- dchisq(x, df = df)
    y[x < ub] <- 0
    y
  }

  vlines <- c(ub, lb) %>% na.omit()
  if (length(vlines) == 0) vlines <- c(-Inf, Inf)

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(dat, aes(x = .data$x)) +
    stat_function(fun = stats::dchisq, args = list(df = df)) +
    stat_function(fun = dchisq_limit, geom = "area", fill = "blue", alpha = 0.5, n = 501) +
    stat_function(fun = dchisq_lb, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    stat_function(fun = dchisq_ub, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    geom_vline(xintercept = vlines, color = "black", linetype = "dashed", linewidth = 0.5) +
    labs(x = "", y = "")

  sshhr(plt)
}

#' Summary method for the probability calculator (Chi-squared distribution)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_chisq}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_chisq}} to calculate results
#' @seealso \code{\link{plot.prob_chisq}} to plot results
#'
#' @examples
#' result <- prob_chisq(df = 1, ub = 3.841)
#' summary(result, type = "values")
#'
#' @export
summary.prob_chisq <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: Chi-squared\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))

  cat("Df          :", df, "\n")
  cat("Mean        :", df, "\n")
  cat("Variance    :", 2 * df, "\n")

  if (type == "values") {
    cat("Lower bound :", if (is.na(lb)) "0" else lb, "\n")
    cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X < ", lb, ") = ", p_lb, "\n"))
        cat(paste0("P(X > ", lb, ") = ", round(1 - p_lb, dec), "\n"))
      }

      if (!is.na(ub)) {
        cat(paste0("P(X < ", ub, ") = ", p_ub, "\n"))
        cat(paste0("P(X > ", ub, ") = ", round(1 - p_ub, dec), "\n"))
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " < X < ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " < X < ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    pub <- if (is.na(pub)) 2 else pub
    plb <- if (is.na(plb)) -1 else plb

    cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
    cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

    if (pub <= 1 || plb >= 0) {
      cat("\n")

      if (plb >= 0) {
        cat(paste0("P(X < ", v_lb, ") = ", plb, "\n"))
        cat(paste0("P(X > ", v_lb, ") = ", round(1 - plb, dec), "\n"))
      }

      if (pub <= 1) {
        cat(paste0("P(X < ", v_ub, ") = ", pub, "\n"))
        cat(paste0("P(X > ", v_ub, ") = ", round(1 - pub, dec), "\n"))
      }

      if (pub <= 1 && plb >= 0) {
        cat(paste0("P(", v_lb, " < X < ", v_ub, ")     = ", pub - plb, "\n"))
        cat(paste0("1 - P(", v_lb, " < X < ", v_ub, ") = ", round(1 - (pub - plb), dec), "\n"))
      }
    }
  }
}

#' Probability calculator for the uniform distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param min Minimum value
#' @param max Maximum value
#' @param lb Lower bound (default = 0)
#' @param ub Upper bound (default = 1)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_unif}} to summarize results
#' @seealso \code{\link{plot.prob_unif}} to plot results
#'
#' @examples
#' prob_unif(min = 0, max = 1, ub = 0.3)
#'
#' @export
prob_unif <- function(min, max, lb = NA, ub = NA,
                      plb = NA, pub = NA, dec = 3) {
  if (min > max) {
    mess_values <- "\nThe maximum value must be larger than the minimum value"
    mess_probs <- "\nThe maximum value must be larger than the minimum value"
  }

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(plb) && !is.na(pub)) {
    if (plb > pub) {
      plb <- pub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  p_ub <- punif(ub, min, max)
  p_lb <- punif(lb, min, max)
  p_int <- max(p_ub - p_lb, 0) %>% round(dec)

  p_ub %<>% round(dec)
  p_lb %<>% round(dec)

  if (!is.na(pub)) {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
  }

  if (!is.na(plb)) {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
  }

  v_ub <- qunif(pub, min, max) %>% round(dec)
  v_lb <- qunif(plb, min, max) %>% round(dec)

  mean <- (max + min) / 2
  stdev <- sqrt((max - min)^2 / 12)

  as.list(environment()) %>% add_class("prob_unif")
}

#' Plot method for the probability calculator (uniform)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_unif}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_unif}} to calculate results
#' @seealso \code{\link{summary.prob_unif}} to summarize results
#'
#' @examples
#' result <- prob_unif(min = 0, max = 1, ub = 0.3)
#' plot(result, type = "values")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_unif <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }

  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$v_lb
    ub <- x$v_ub
  }

  min <- x$min
  max <- x$max

  if (min > max) {
    return(" ")
  }

  limits <- c(min, max)
  dunif_limit <- function(x) {
    y <- dunif(x, min = min, max = max)
    y[x < lb | x > ub] <- 0
    y
  }

  dunif_lb <- function(x) {
    if (is.na(lb)) {
      return(0)
    }
    y <- dunif(x, min = min, max = max)
    y[x > lb] <- 0
    y
  }

  dunif_ub <- function(x) {
    if (is.na(ub)) {
      return(0)
    }
    y <- dunif(x, min = min, max = max)
    y[x < ub] <- 0
    y
  }

  dunif_lines <- c(ub, lb) %>%
    na.omit() %>%
    base::setdiff(c(min, max))
  if (length(dunif_lines) == 0) dunif_lines <- c(-Inf, Inf)

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- data.frame(x = limits, y = dunif(limits, limits[1], limits[2]), lb = lb, ub = ub) %>%
    ggplot(aes(x = .data$x)) +
    stat_function(fun = dunif_limit, geom = "area", fill = "blue", alpha = 0.5, n = 501) +
    stat_function(fun = dunif_lb, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    stat_function(fun = dunif_ub, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    geom_vline(xintercept = dunif_lines, color = "black", linetype = "dashed", linewidth = 0.5) +
    geom_segment(aes(x = x[1], y = 0, xend = x[1], yend = y[1])) +
    geom_segment(aes(x = x[2], y = 0, xend = x[2], yend = y[2])) +
    geom_segment(aes(x = x[1], y = y[1], xend = x[2], yend = y[2])) +
    labs(x = "", y = "")

  sshhr(plt)
}

#' Summary method for the probability calculator (uniform)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_unif}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_unif}} to calculate results
#' @seealso \code{\link{plot.prob_unif}} to plot results
#'
#' @examples
#' result <- prob_unif(min = 0, max = 1, ub = 0.3)
#' summary(result, type = "values")
#'
#' @export
summary.prob_unif <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: Uniform\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))

  cat("Min         :", min, "\n")
  cat("Max         :", max, "\n")
  if (max > min) {
    cat("Mean        :", round(mean, dec), "\n")
    cat("St. dev     :", round(stdev, dec), "\n")
  }

  if (type == "values") {
    cat("Lower bound :", ifelse(is.na(lb), min, lb), "\n")
    cat("Upper bound :", ifelse(is.na(ub), max, ub), "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X < ", lb, ") = ", p_lb, "\n"))
        cat(paste0("P(X > ", lb, ") = ", round(1 - p_lb, dec), "\n"))
      }

      if (!is.na(ub)) {
        cat(paste0("P(X < ", ub, ") = ", p_ub, "\n"))
        cat(paste0("P(X > ", ub, ") = ", round(1 - p_ub, dec), "\n"))
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " < X < ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " < X < ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    pub <- if (is.na(pub)) 2 else pub
    plb <- if (is.na(plb)) -1 else plb

    cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
    cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

    if (pub <= 1 || plb >= 0) {
      cat("\n")

      if (plb >= 0) {
        cat(paste0("P(X < ", v_lb, ") = ", plb, "\n"))
        cat(paste0("P(X > ", v_lb, ") = ", round(1 - plb, dec), "\n"))
      }

      if (pub <= 1) {
        cat(paste0("P(X < ", v_ub, ") = ", pub, "\n"))
        cat(paste0("P(X > ", v_ub, ") = ", round(1 - pub, dec), "\n"))
      }

      if (pub <= 1 && plb >= 0) {
        cat(paste0("P(", v_lb, " < X < ", v_ub, ")     = ", pub - plb, "\n"))
        cat(paste0("1 - P(", v_lb, " < X < ", v_ub, ") = ", round(1 - (pub - plb), dec), "\n"))
      }
    }
  }
}

#' Probability calculator for the binomial distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param n Number of trials
#' @param p Probability
#' @param lb Lower bound on the number of successes
#' @param ub Upper bound on the number of successes
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_binom}} to summarize results
#' @seealso \code{\link{plot.prob_binom}} to plot results
#'
#' @examples
#' prob_binom(n = 10, p = 0.3, ub = 3)
#'
#' @export
prob_binom <- function(n, p, lb = NA, ub = NA,
                       plb = NA, pub = NA, dec = 3) {

  ## making sure n is integer
  n <- as_integer(n)

  if (!is_not(lb) && lb < 0) lb <- 0
  if (!is_not(ub) && ub < 0) ub <- 0

  if (is.na(lb) || lb < 0) {
    p_elb <- p_lb <- lb <- NA
  } else {
    lb <- as_integer(lb)
    if (lb > n) lb <- n
    p_elb <- dbinom(lb, n, p) %>% round(dec)
    p_lelb <- pbinom(lb, n, p) %>% round(dec)
    if (lb > 0) {
      p_lb <- sum(dbinom(0:max((lb - 1), 0), n, p)) %>% round(dec)
    } else {
      p_lb <- 0
    }
  }

  if (is.na(ub) || ub < 0) {
    p_eub <- p_ub <- ub <- NA
  } else {
    ub <- as_integer(ub)
    if (ub > n) ub <- n
    p_eub <- dbinom(ub, n, p) %>% round(dec)
    p_leub <- pbinom(ub, n, p) %>% round(dec)
    if (ub > 0) {
      p_ub <- sum(dbinom(0:max((ub - 1), 0), n, p)) %>% round(dec)
    } else {
      p_ub <- 0
    }
  }

  if (!is.na(ub) && !is.na(lb)) {
    p_int <- sum(dbinom(lb:ub, n, p)) %>%
      max(0) %>%
      round(dec)
  } else {
    p_int <- NA
  }

  if (is.na(plb)) {
    vlb <- NA
  } else {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
    vlb <- qbinom(plb, n, p)

    vp_elb <- dbinom(vlb, n, p) %>% round(dec)
    vp_lelb <- pbinom(vlb, n, p) %>% round(dec)
    if (vlb > 0) {
      vp_lb <- sum(dbinom(0:max((vlb - 1), 0), n, p)) %>% round(dec)
    } else {
      vp_lb <- 0
    }
  }

  if (is.na(pub)) {
    vub <- NA
  } else {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
    vub <- qbinom(pub, n, p)

    vp_eub <- dbinom(vub, n, p) %>% round(dec)
    vp_leub <- pbinom(vub, n, p) %>% round(dec)
    if (vub > 0) {
      vp_ub <- sum(dbinom(0:max((vub - 1), 0), n, p)) %>% round(dec)
    } else {
      vp_ub <- 0
    }
  }

  if (!is.na(pub) && !is.na(plb)) {
    vp_int <- sum(dbinom(vlb:vub, n, p)) %>%
      max(0) %>%
      round(dec)
  } else {
    vp_int <- NA
  }

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(vlb) && !is.na(vub)) {
    if (vlb > vub) {
      plb <- pub <- vlb <- vub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  as.list(environment()) %>% add_class("prob_binom")
}

#' Plot method for the probability calculator (binomial)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_binom}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#
#' @seealso \code{\link{prob_binom}} to calculate results
#' @seealso \code{\link{summary.prob_binom}} to summarize results
#'
#' @examples
#' result <- prob_binom(n = 10, p = 0.3, ub = 3)
#' plot(result, type = "values")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_binom <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }

  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$vlb
    ub <- x$vub
  }

  n <- x$n
  p <- x$p

  limits <- 0:n

  k <- factor(rep("below", n + 1), levels = c("below", "equal", "above"))
  if (!is_not(ub)) {
    k[ub + 1] <- "equal"
    if (!is.na(lb)) k[(lb:ub) + 1] <- "equal"
    k[0:n > ub] <- "above"
  } else if (!is_not(lb)) {
    k[lb + 1] <- "equal"
    k[0:n > lb] <- "above"
  } else {
    return(" ")
  }

  dat <- data.frame(
    x = as_factor(limits),
    Probability = dbinom(limits, size = n, prob = p),
    k = k,
    stringsAsFactors = FALSE
  ) %>%
    filter(., .$Probability > 0.00001)

  if (nrow(dat) < 40) {
    breaks <- dat$x
  } else {
    dx <- as_integer(dat$x)
    breaks <- seq(min(dx), max(dx), length.out = 10) %>% round(0)
  }

  cols <- c(below = "red", equal = "blue", above = "black")

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(dat, aes(x = .data$x, y = .data$Probability, fill = .data$k)) +
    geom_bar(stat = "identity", alpha = 0.5) +
    labs(x = "") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "none") +
    scale_x_discrete(breaks = breaks)

  sshhr(plt)
}

#' Summary method for the probability calculator (binomial)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_binom}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_binom}} to calculate results
#' @seealso \code{\link{plot.prob_binom}} to plot results
#'
#' @examples
#' result <- prob_binom(n = 10, p = 0.3, ub = 3)
#' summary(result, type = "values")
#'
#' @export
summary.prob_binom <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: Binomial\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))

  cat("n           :", n, "\n")
  cat("p           :", p, "\n")
  cat("Mean        :", round(n * p, dec), "\n")
  cat("St. dev     :", sqrt(n * p * (1 - p)) %>% round(dec), "\n")

  if (type == "values") {
    cat("Lower bound :", ifelse(is.na(lb), "", lb), "\n")
    cat("Upper bound :", ifelse(is.na(ub), "", ub), "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X  = ", lb, ") = ", p_elb, "\n"))
        if (lb > 0) {
          cat(paste0("P(X  < ", lb, ") = ", p_lb, "\n"))
          cat(paste0("P(X <= ", lb, ") = ", p_lelb, "\n"))
        }
        if (lb < n) {
          cat(paste0("P(X  > ", lb, ") = ", round(1 - (p_lb + p_elb), dec), "\n"))
          cat(paste0("P(X >= ", lb, ") = ", round(1 - p_lb, dec), "\n"))
        }
      }

      if (!is.na(ub)) {
        cat(paste0("P(X  = ", ub, ") = ", p_eub, "\n"))
        if (ub > 0) {
          cat(paste0("P(X  < ", ub, ") = ", p_ub, "\n"))
          cat(paste0("P(X <= ", ub, ") = ", p_leub, "\n"))
        }
        if (ub < n) {
          cat(paste0("P(X  > ", ub, ") = ", round(1 - (p_ub + p_eub), dec), "\n"))
          cat(paste0("P(X >= ", ub, ") = ", round(1 - p_ub, dec), "\n"))
        }
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " <= X <= ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " <= X <= ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    cat("Lower bound :", if (is.na(plb)) "\n" else paste0(plb, " (", vlb, ")\n"))
    cat("Upper bound :", if (is.na(pub)) "\n" else paste0(pub, " (", vub, ")\n"))

    if (!is.na(pub) || !is.na(plb)) {
      cat("\n")

      if (!is.na(plb)) {
        cat(paste0("P(X  = ", vlb, ") = ", vp_elb, "\n"))
        if (vlb > 0) {
          cat(paste0("P(X  < ", vlb, ") = ", vp_lb, "\n"))
          cat(paste0("P(X <= ", vlb, ") = ", vp_lelb, "\n"))
        }
        if (vlb < n) {
          cat(paste0("P(X  > ", vlb, ") = ", round(1 - (vp_lb + vp_elb), dec), "\n"))
          cat(paste0("P(X >= ", vlb, ") = ", round(1 - vp_lb, dec), "\n"))
        }
      }

      if (!is.na(pub)) {
        cat(paste0("P(X  = ", vub, ") = ", vp_eub, "\n"))
        if (vub > 0) {
          cat(paste0("P(X  < ", vub, ") = ", vp_ub, "\n"))
          cat(paste0("P(X <= ", vub, ") = ", vp_leub, "\n"))
        }
        if (vub < n) {
          cat(paste0("P(X  > ", vub, ") = ", round(1 - (vp_ub + vp_eub), dec), "\n"))
          cat(paste0("P(X >= ", vub, ") = ", round(1 - vp_ub, dec), "\n"))
        }
      }

      if (!is.na(plb) && !is.na(pub)) {
        cat(paste0("P(", vlb, " <= X <= ", vub, ")     = ", vp_int, "\n"))
        cat(paste0("1 - P(", vlb, " <= X <= ", vub, ") = ", round(1 - vp_int, dec), "\n"))
      }
    }
  }
}

#' Probability calculator for a discrete distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param v Values
#' @param p Probabilities
#' @param lb Lower bound on the number of successes
#' @param ub Upper bound on the number of successes
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_disc}} to summarize results
#' @seealso \code{\link{plot.prob_disc}} to plot results
#'
#' @examples
#' prob_disc(v = 1:6, p = 1 / 6, pub = 0.95)
#' prob_disc(v = 1:6, p = c(2 / 6, 2 / 6, 1 / 12, 1 / 12, 1 / 12, 1 / 12), pub = 0.95)
#'
#' @export
prob_disc <- function(v, p, lb = NA, ub = NA,
                      plb = NA, pub = NA, dec = 3) {

  # Think about adding an "expand.grid" setup so you can run this n times. e.g., rolling multiple dice
  # expand.grid(height = 1:6, weight = 1:6)
  rex <- "(\\s*,\\s*|\\s*;\\s*|\\s+)"
  if (is.character(v)) v <- strsplit(v, rex) %>% unlist()
  if (is.character(p)) p <- strsplit(p, rex) %>% unlist()
  rm(rex)

  lp <- length(p)
  lv <- length(v)
  if (lv != lp && lv %% lp == 0) p <- rep(p, lv / lp)

  if (length(v) != length(p)) {
    mess <- "The number of values must be the same or a multiple of the number of probabilities"
    return(list(mess_probs = mess, mess_values = mess) %>% add_class("prob_disc"))
  }

  asNum <- function(x) ifelse(length(x) > 1, as.numeric(x[1]) / as.numeric(x[2]), as.numeric(x[1]))
  if (is.character(v)) v <- sshhr(strsplit(v, "/") %>% sapply(asNum))
  if (is.character(p)) p <- sshhr(strsplit(p, "/") %>% sapply(asNum))

  if (anyNA(p) | anyNA(v)) {
    mess <- "The number of probabilities entered must be a multiple of the number of values"
    mess <- paste0("Invalid inputs:\n\nv: ", paste0(v, collapse = " "), "\np: ", paste0(p, collapse = " "))
    return(list(mess_probs = mess, mess_values = mess) %>% add_class("prob_disc"))
  }

  ## make sure values and probabilities are ordered correctly
  df <- data.frame(v = v, p = p, stringsAsFactors = FALSE) %>%
    arrange(v)
  p <- df$p
  v <- df$v

  if (sum(p) < .99 || sum(p) > 1.01) {
    mess_probs <- mess_values <- paste0("Probabilities for a discrete variable do not sum to 1 (", round(sum(p), 3), ")")
    return(as.list(environment()) %>% add_class("prob_disc"))
  }

  ddisc <- function(b, df) filter(df, v == b)$p
  pdisc <- function(b, df) filter(df, v < b)$p %>% sum()
  ## consistent with http://www.stat.umn.edu/geyer/old/5101/rlook.html#qbinom
  qdisc <- function(prob, df) {
    mutate(df, p = cumsum(df$p)) %>%
      filter(p >= prob) %>%
      .$v %>%
      min()
  }

  if (is.na(lb)) {
    p_elb <- p_lb <- lb <- NA
  } else if (!lb %in% v) {
    p_elb <- 0
    p_lb <- ifelse(lb < min(v), 0, pdisc(lb, df) %>% round(dec))
    p_lelb <- p_elb + p_lb
  } else {
    p_elb <- ddisc(lb, df) %>% round(dec)
    p_lb <- pdisc(lb, df) %>% round(dec)
    p_lelb <- p_elb + p_lb
  }

  if (is.na(ub)) {
    p_eub <- p_ub <- ub <- NA
  } else if (!ub %in% v) {
    p_eub <- 0
    p_ub <- ifelse(ub < min(v), 0, pdisc(ub, df) %>% round(dec))
    p_leub <- p_eub + p_ub
  } else {
    p_eub <- ddisc(ub, df) %>% round(dec)
    p_ub <- pdisc(ub, df) %>% round(dec)
    p_leub <- p_eub + p_ub
  }

  if (!is.na(ub) && !is.na(lb)) {
    p_int <- p_leub - p_lb
  } else {
    p_int <- NA
  }

  if (is.na(plb)) {
    plb <- vlb <- NA
  } else if (length(qdisc(plb, df)) == 0) {
    mess_probs <- "Lower bound is too low"
    return(as.list(environment()) %>% add_class("prob_disc"))
  } else {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
    vlb <- qdisc(plb, df)
    vp_elb <- ddisc(vlb, df) %>% round(dec)
    vp_lb <- pdisc(vlb, df) %>% round(dec)
    vp_lelb <- vp_elb + vp_lb
  }

  if (is.na(pub)) {
    pub <- vub <- NA
  } else if (length(qdisc(pub, df)) == 0) {
    mess_probs <- "Upper bound is too low"
    return(as.list(environment()) %>% add_class("prob_disc"))
  } else {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
    vub <- qdisc(pub, df)
    vp_eub <- ddisc(vub, df) %>% round(dec)
    vp_ub <- pdisc(vub, df) %>% round(dec)
    vp_leub <- vp_eub + vp_ub
  }

  if (!is.na(pub) && !is.na(plb)) {
    vp_int <- vp_leub - vp_lb
  } else {
    vp_int <- NA
  }

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(vlb) && !is.na(vub)) {
    if (vlb > vub || plb > pub) {
      plb <- pub <- vlb <- vub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  rm(qdisc, pdisc, ddisc, asNum)

  as.list(environment()) %>% add_class("prob_disc")
}

#' Plot method for the probability calculator (discrete)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_disc}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_disc}} to calculate results
#' @seealso \code{\link{summary.prob_disc}} to summarize results
#'
#' @examples
#' result <- prob_disc(v = 1:6, p = c(2 / 6, 2 / 6, 1 / 12, 1 / 12, 1 / 12, 1 / 12), pub = 0.95)
#' plot(result, type = "probs")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_disc <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }

  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$vlb
    ub <- x$vub
  }

  v <- x$v
  p <- x$p

  limits <- v

  k <- factor(rep("below", length(v)), levels = c("below", "equal", "above"))
  if (!is.empty(ub)) {
    if (!is.na(lb)) {
      k[v >= lb & v <= ub] <- "equal"
    } else if (ub %in% v) {
      k[v == ub] <- "equal"
    }
    k[v > ub] <- "above"
  } else if (!is.empty(lb)) {
    if (lb %in% v) k[v == lb] <- "equal"
    k[v > lb] <- "above"
  } else {
    return(" ")
  }

  dat <- data.frame(
    x = limits %>% as_factor(),
    Probability = p,
    k = k,
    stringsAsFactors = FALSE
  )

  if (nrow(dat) < 30) {
    breaks <- dat$x
  } else {
    dx <- as_integer(dat$x)
    breaks <- seq(min(dx), max(dx), length.out = 10) %>% round(0)
  }

  cols <- c(below = "red", equal = "blue", above = "black")

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(dat, aes(x = .data$x, y = .data$Probability, fill = .data$k)) +
    geom_bar(stat = "identity", alpha = 0.5) +
    labs(x = "") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "none") +
    scale_x_discrete(breaks = breaks)

  sshhr(plt)
}

#' Summary method for the probability calculator (discrete)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_disc}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_disc}} to calculate results
#' @seealso \code{\link{plot.prob_disc}} to plot results
#'
#' @examples
#' result <- prob_disc(v = 1:6, p = c(2 / 6, 2 / 6, 1 / 12, 1 / 12, 1 / 12, 1 / 12), pub = 0.95)
#' summary(result, type = "probs")
#'
#' @export
summary.prob_disc <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution : Discrete\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))

  cat("Values       :", paste0(v, collapse = " "), "\n")
  cat("Probabilities:", paste0(round(p, dec), collapse = " "), "\n")
  m <- sum(v * p)
  std <- sqrt(sum(p * (v - m)^2))
  cat("Mean         :", round(m, dec), "\n")
  cat("St. dev      :", round(std, dec), "\n")

  if (type == "values") {
    cat("Lower bound  :", ifelse(is.na(lb), "", lb), "\n")
    cat("Upper bound  :", ifelse(is.na(ub), "", ub), "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X  = ", lb, ") = ", p_elb, "\n"))
        if (lb > min(v)) {
          cat(paste0("P(X  < ", lb, ") = ", p_lb, "\n"))
          cat(paste0("P(X <= ", lb, ") = ", p_lelb, "\n"))
        }
        if (lb < max(v)) {
          cat(paste0("P(X  > ", lb, ") = ", round(1 - (p_lb + p_elb), dec), "\n"))
          cat(paste0("P(X >= ", lb, ") = ", round(1 - p_lb, dec), "\n"))
        }
      }

      if (!is.na(ub)) {
        cat(paste0("P(X  = ", ub, ") = ", p_eub, "\n"))
        if (ub > min(v)) {
          cat(paste0("P(X  < ", ub, ") = ", p_ub, "\n"))
          cat(paste0("P(X <= ", ub, ") = ", p_leub, "\n"))
        }
        if (ub < max(v)) {
          cat(paste0("P(X  > ", ub, ") = ", round(1 - (p_ub + p_eub), dec), "\n"))
          cat(paste0("P(X >= ", ub, ") = ", round(1 - p_ub, dec), "\n"))
        }
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " <= X <= ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " <= X <= ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    cat("Lower bound  :", if (is.na(plb)) "\n" else paste0(plb, " (", vlb, ")\n"))
    cat("Upper bound  :", if (is.na(pub)) "\n" else paste0(pub, " (", vub, ")\n"))

    if (!is.na(pub) || !is.na(plb)) {
      cat("\n")

      if (!is.na(plb)) {
        cat(paste0("P(X  = ", vlb, ") = ", vp_elb, "\n"))
        if (vlb > min(v)) {
          cat(paste0("P(X  < ", vlb, ") = ", vp_lb, "\n"))
          cat(paste0("P(X <= ", vlb, ") = ", vp_lelb, "\n"))
        }
        if (vlb < max(v)) {
          cat(paste0("P(X  > ", vlb, ") = ", round(1 - (vp_lb + vp_elb), dec), "\n"))
          cat(paste0("P(X >= ", vlb, ") = ", round(1 - vp_lb, dec), "\n"))
        }
      }

      if (!is.na(pub)) {
        cat(paste0("P(X  = ", vub, ") = ", vp_eub, "\n"))
        if (vub > min(v)) {
          cat(paste0("P(X  < ", vub, ") = ", vp_ub, "\n"))
          cat(paste0("P(X <= ", vub, ") = ", vp_leub, "\n"))
        }
        if (vub < max(v)) {
          cat(paste0("P(X  > ", vub, ") = ", round(1 - (vp_ub + vp_eub), dec), "\n"))
          cat(paste0("P(X >= ", vub, ") = ", round(1 - vp_ub, dec), "\n"))
        }
      }

      if (!is.na(plb) && !is.na(pub)) {
        cat(paste0("P(", vlb, " <= X <= ", vub, ")     = ", vp_int, "\n"))
        cat(paste0("1 - P(", vlb, " <= X <= ", vub, ") = ", round(1 - vp_int, dec), "\n"))
      }
    }
  }
}

#' Probability calculator for the exponential distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param rate Rate
#' @param lb Lower bound (default is 0)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_expo}} to summarize results
#' @seealso \code{\link{plot.prob_expo}} to plot results
#'
#' @examples
#' prob_expo(rate = 1, ub = 2.996)
#'
#' @export
prob_expo <- function(rate, lb = NA, ub = NA,
                      plb = NA, pub = NA, dec = 3) {
  if (!is_not(lb) && lb < 0) lb <- 0
  if (!is_not(ub) && ub < 0) ub <- 0

  p_ub <- pexp(ub, rate)
  p_lb <- pexp(lb, rate)
  p_int <- max(p_ub - p_lb, 0)

  p_ub %<>% round(dec)
  p_lb %<>% round(dec)
  p_int %<>% round(dec)

  if (!is.na(pub)) {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
  }

  if (!is.na(plb)) {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
  }

  v_ub <- qexp(pub, rate) %>% round(dec)
  v_lb <- qexp(plb, rate) %>% round(dec)

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(plb) && !is.na(pub)) {
    if (plb > pub) {
      plb <- pub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  as.list(environment()) %>% add_class("prob_expo")
}

#' Plot method for the probability calculator (Exponential distribution)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_expo}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_expo}} to calculate results
#' @seealso \code{\link{summary.prob_expo}} to summarize results
#'
#' @examples
#' result <- prob_expo(rate = 1, ub = 2.996)
#' plot(result, type = "values")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_expo <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }

  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$v_lb
    ub <- x$v_ub
  }

  rate <- x$rate

  limits <- c(
    floor(qexp(0.001, rate = rate)),
    ceiling(qexp(1 - 0.001, rate = rate))
  )

  dat <- data.frame(
    x = limits,
    Probability = dexp(limits, rate = rate),
    rate = rate,
    stringsAsFactors = FALSE
  )

  dexp_limit <- function(x) {
    y <- dexp(x, rate = rate)
    y[x < lb | x > ub] <- 0
    y
  }

  dexp_lb <- function(x) {
    if (is.na(lb)) {
      return(0)
    }
    y <- dexp(x, rate = rate)
    y[x > lb] <- 0
    y
  }

  dexp_ub <- function(x) {
    if (is.na(ub)) {
      return(0)
    }
    y <- dexp(x, rate = rate)
    y[x < ub] <- 0
    y
  }

  vlines <- c(ub, lb) %>% na.omit()
  if (length(vlines) == 0) vlines <- c(-Inf, Inf)

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(dat, aes(x = .data$x)) +
    stat_function(fun = stats::dexp, args = list(rate = rate)) +
    stat_function(fun = dexp_limit, geom = "area", fill = "blue", alpha = 0.5, n = 501) +
    stat_function(fun = dexp_lb, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    stat_function(fun = dexp_ub, geom = "area", fill = "red", alpha = 0.5, n = 501) +
    geom_vline(xintercept = vlines, color = "black", linetype = "dashed", linewidth = 0.5) +
    labs(x = "", y = "")

  sshhr(plt)
}


#' Summary method for the probability calculator (exponential)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_expo}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_expo}} to calculate results
#' @seealso \code{\link{plot.prob_expo}} to plot results
#'
#' @examples
#' result <- prob_expo(rate = 1, ub = 2.996)
#' summary(result, type = "values")
#'
#' @export
summary.prob_expo <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: Exponential\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))

  cat("Rate        :", rate, "\n")
  cat("Mean        :", round(1 / rate, dec), "\n")
  cat("Variance    :", round(rate^-2, dec), "\n")

  if (type == "values") {
    cat("Lower bound :", if (is.na(lb)) "0" else lb, "\n")
    cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X < ", lb, ") = ", p_lb, "\n"))
        cat(paste0("P(X > ", lb, ") = ", round(1 - p_lb, dec), "\n"))
      }

      if (!is.na(ub)) {
        cat(paste0("P(X < ", ub, ") = ", p_ub, "\n"))
        cat(paste0("P(X > ", ub, ") = ", round(1 - p_ub, dec), "\n"))
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " < X < ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " < X < ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    pub <- if (is.na(pub)) 2 else pub
    plb <- if (is.na(plb)) -1 else plb

    cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
    cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

    if (pub <= 1 || plb >= 0) {
      cat("\n")

      if (plb >= 0) {
        cat(paste0("P(X < ", v_lb, ") = ", plb, "\n"))
        cat(paste0("P(X > ", v_lb, ") = ", round(1 - plb, dec), "\n"))
      }

      if (pub <= 1) {
        cat(paste0("P(X < ", v_ub, ") = ", pub, "\n"))
        cat(paste0("P(X > ", v_ub, ") = ", round(1 - pub, dec), "\n"))
      }

      if (pub <= 1 && plb >= 0) {
        cat(paste0("P(", v_lb, " < X < ", v_ub, ")     = ", pub - plb, "\n"))
        cat(paste0("1 - P(", v_lb, " < X < ", v_ub, ") = ", round(1 - (pub - plb), dec), "\n"))
      }
    }
  }
}

#' Probability calculator for the poisson distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param lambda Rate
#' @param lb Lower bound (default is 0)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{summary.prob_pois}} to summarize results
#' @seealso \code{\link{plot.prob_pois}} to plot results
#'
#' @examples
#' prob_pois(lambda = 1, ub = 3)
#'
#' @export
prob_pois <- function(lambda, lb = NA, ub = NA,
                      plb = NA, pub = NA, dec = 3) {
  if (lambda <= 0) mess_values <- "\nLambda must be positive"

  if (!is_not(lb) && lb < 0) lb <- 0
  if (!is_not(ub) && ub < 0) ub <- 0

  if (is.na(lb) || lb < 0) {
    p_elb <- p_lb <- lb <- NA
  } else {
    p_elb <- dpois(lb, lambda) %>% round(dec)
    p_lelb <- ppois(lb, lambda) %>% round(dec)
    if (lb > 0) {
      p_lb <- (ppois(lb, lambda) - dpois(lb, lambda)) %>% round(dec)
    } else {
      p_lb <- 0
    }
  }

  if (is.na(ub) || ub < 0) {
    p_eub <- p_ub <- ub <- NA
  } else {
    p_eub <- dpois(ub, lambda) %>% round(dec)
    p_leub <- ppois(ub, lambda) %>% round(dec)
    if (ub > 0) {
      p_ub <- (ppois(ub, lambda) - dpois(ub, lambda)) %>% round(dec)
    } else {
      p_ub <- 0
    }
  }

  if (!is.na(ub) && !is.na(lb)) {
    p_int <- sum(dpois(lb:ub, lambda)) %>%
      max(0) %>%
      round(dec)
  } else {
    p_int <- NA
  }

  if (is.na(plb)) {
    vlb <- NA
  } else {
    if (plb > 1) plb <- 1
    if (plb < 0) plb <- 0
    vlb <- qpois(plb, lambda)

    vp_elb <- dpois(vlb, lambda) %>% round(dec)
    vp_lelb <- ppois(vlb, lambda) %>% round(dec)
    if (vlb > 0) {
      vp_lb <- (ppois(vlb, lambda) - dpois(vlb, lambda)) %>% round(dec)
    } else {
      vp_lb <- 0
    }
  }

  if (is.na(pub)) {
    vub <- NA
  } else {
    if (pub > 1) pub <- 1
    if (pub < 0) pub <- 0
    vub <- qpois(pub, lambda)

    vp_eub <- dpois(vub, lambda) %>% round(dec)
    vp_leub <- ppois(vub, lambda) %>% round(dec)
    if (vub > 0) {
      vp_ub <- (ppois(vub, lambda) - dpois(vub, lambda)) %>% round(dec)
    } else {
      vp_ub <- 0
    }
  }

  if (!is.na(lb) && !is.na(ub)) {
    if (lb > ub) {
      lb <- ub <- NA
      mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(vlb) && !is.na(vub)) {
    if (vlb > vub || plb > pub) {
      plb <- pub <- vlb <- vub <- NA
      mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
    }
  }

  if (!is.na(pub) && !is.na(plb)) {
    vp_int <- sum(dpois(vlb:vub, lambda)) %>%
      max(0) %>%
      round(dec)
  } else {
    vp_int <- NA
  }

  as.list(environment()) %>% add_class("prob_pois")
}

#' Plot method for the probability calculator (poisson)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_pois}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_pois}} to calculate results
#' @seealso \code{\link{summary.prob_pois}} to summarize results
#'
#' @examples
#' result <- prob_pois(lambda = 1, ub = 3)
#' plot(result, type = "values")
#'
#' @importFrom rlang .data
#'
#' @export
plot.prob_pois <- function(x, type = "values", ...) {
  mess <- paste0("mess_", type)
  if (!is.null(x[[mess]])) {
    return(" ")
  }

  if (type == "values") {
    lb <- x$lb
    ub <- x$ub
  } else {
    lb <- x$vlb
    ub <- x$vub
  }

  lambda <- x$lambda
  limits <- 0:(ceiling(qpois(1 - 0.00001, lambda)))
  n <- max(limits)

  if (!is.na(lb) && lb > n) {
    limits <- 0:lb
    n <- lb
  }

  if (!is.na(ub) && ub > n) {
    limits <- 0:ub
    n <- ub
  }

  k <- factor(rep("below", n + 1), levels = c("below", "equal", "above"))
  if (!is_not(ub)) {
    k[ub + 1] <- "equal"
    if (!is.na(lb)) k[(lb:ub) + 1] <- "equal"
    k[0:n > ub] <- "above"
  } else if (!is_not(lb)) {
    k[lb + 1] <- "equal"
    k[0:n > lb] <- "above"
  } else {
    return(" ")
  }

  dat <- data.frame(
    x = limits %>% as_factor(),
    Probability = dpois(limits, lambda),
    k = k,
    stringsAsFactors = FALSE
  ) %>% filter(., .$Probability > 0.00001)

  if (nrow(dat) < 40) {
    breaks <- dat$x
  } else {
    dx <- as_integer(dat$x)
    breaks <- seq(min(dx), max(dx), length.out = 10) %>% round(0)
  }

  cols <- c(below = "red", equal = "blue", above = "black")

  ## based on https://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
  ## and R Graphics Cookbook
  plt <- ggplot(dat, aes(x = .data$x, y = .data$Probability, fill = .data$k)) +
    geom_bar(stat = "identity", alpha = 0.5) +
    labs(x = "") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "none") +
    scale_x_discrete(breaks = breaks)

  sshhr(plt)
}

#' Summary method for the probability calculator (poisson)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_pois}}
#' @param type Probabilities ("probs") or values ("values")
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{prob_pois}} to calculate results
#' @seealso \code{\link{plot.prob_pois}} to plot results
#'
#' @examples
#' result <- prob_pois(lambda = 1, ub = 3)
#' summary(result, type = "values")
#'
#' @export
summary.prob_pois <- function(object, type = "values", ...) {
  cat("Probability calculator\n")
  cat("Distribution: Poisson\n")

  mess <- object[[paste0("mess_", type)]]
  if (!is.null(mess)) {
    return(mess)
  }
  env <- environment()
  ret <- sapply(names(object), function(x) assign(x, object[[x]], envir = env))

  cat("Lambda      :", lambda, "\n")
  cat("Mean        :", lambda, "\n")
  cat("Variance    :", lambda, "\n")

  if (type == "values") {
    cat("Lower bound :", ifelse(is.na(lb), "", lb), "\n")
    cat("Upper bound :", ifelse(is.na(ub), "", ub), "\n")

    if (!is.na(ub) || !is.na(lb)) {
      cat("\n")

      if (!is.na(lb)) {
        cat(paste0("P(X  = ", lb, ") = ", p_elb, "\n"))
        if (lb > 0) {
          cat(paste0("P(X  < ", lb, ") = ", p_lb, "\n"))
          cat(paste0("P(X <= ", lb, ") = ", p_lelb, "\n"))
        }
        cat(paste0("P(X  > ", lb, ") = ", round(1 - (p_lb + p_elb), dec), "\n"))
        cat(paste0("P(X >= ", lb, ") = ", round(1 - p_lb, dec), "\n"))
      }

      if (!is.na(ub)) {
        cat(paste0("P(X  = ", ub, ") = ", p_eub, "\n"))
        if (ub > 0) {
          cat(paste0("P(X  < ", ub, ") = ", p_ub, "\n"))
          cat(paste0("P(X <= ", ub, ") = ", p_leub, "\n"))
        }
        cat(paste0("P(X  > ", ub, ") = ", round(1 - (p_ub + p_eub), dec), "\n"))
        cat(paste0("P(X >= ", ub, ") = ", round(1 - p_ub, dec), "\n"))
      }

      if (!is.na(lb) && !is.na(ub)) {
        cat(paste0("P(", lb, " <= X <= ", ub, ")     = ", p_int, "\n"))
        cat(paste0("1 - P(", lb, " <= X <= ", ub, ") = ", round(1 - p_int, dec), "\n"))
      }
    }
  } else {
    cat("Lower bound :", if (is.na(plb)) "\n" else paste0(plb, " (", vlb, ")\n"))
    cat("Upper bound :", if (is.na(pub)) "\n" else paste0(pub, " (", vub, ")\n"))

    if (!is.na(pub) || !is.na(plb)) {
      cat("\n")

      if (!is.na(plb)) {
        cat(paste0("P(X  = ", vlb, ") = ", vp_elb, "\n"))
        if (vlb > 0) {
          cat(paste0("P(X  < ", vlb, ") = ", vp_lb, "\n"))
          cat(paste0("P(X <= ", vlb, ") = ", vp_lelb, "\n"))
        }
        cat(paste0("P(X  > ", vlb, ") = ", round(1 - (vp_lb + vp_elb), dec), "\n"))
        cat(paste0("P(X >= ", vlb, ") = ", round(1 - vp_lb, dec), "\n"))
      }

      if (!is.na(pub)) {
        cat(paste0("P(X  = ", vub, ") = ", vp_eub, "\n"))
        if (vub > 0) {
          cat(paste0("P(X  < ", vub, ") = ", vp_ub, "\n"))
          cat(paste0("P(X <= ", vub, ") = ", vp_leub, "\n"))
        }
        cat(paste0("P(X  > ", vub, ") = ", round(1 - (vp_ub + vp_eub), dec), "\n"))
        cat(paste0("P(X >= ", vub, ") = ", round(1 - vp_ub, dec), "\n"))
      }

      if (!is.na(plb) && !is.na(pub)) {
        cat(paste0("P(", vlb, " <= X <= ", vub, ")     = ", vp_int, "\n"))
        cat(paste0("1 - P(", vlb, " <= X <= ", vub, ") = ", round(1 - vp_int, dec), "\n"))
      }
    }
  }
}
