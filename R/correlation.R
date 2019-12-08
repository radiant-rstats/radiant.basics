#' Calculate correlations for two or more variables
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/correlation.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param vars Variables to include in the analysis. Default is all but character and factor variables with more than two unique values are removed
#' @param method Type of correlations to calculate. Options are "pearson", "spearman", and "kendall". "pearson" is the default
#' @param hcor Use polycor::hetcor to calculate the correlation matrix
#' @param hcor_se Calculate standard errors when using polycor::hetcor
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list with all variables defined in the function as an object of class compare_means
#'
#' @examples
#' correlation(diamonds, c("price", "carat")) %>% str()
#' correlation(diamonds, "x:z") %>% str()
#'
#' @seealso \code{\link{summary.correlation}} to summarize results
#' @seealso \code{\link{plot.correlation}} to plot results
#'
#' @importFrom psych corr.test
#' @importFrom lubridate is.Date
#' @importFrom polycor hetcor
#'
#'
#' @export
correlation <- function(
  dataset, vars = "", method = "pearson", hcor = FALSE, hcor_se = FALSE,
  data_filter = "", envir = parent.frame()
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))

  ## data.matrix as the last step in the chain is about 25% slower using
  ## system.time but results (using diamonds and mtcars) are identical
  dataset <- get_data(dataset, vars, filt = data_filter, envir = envir) %>%
    mutate_if(is.Date, as_numeric)
  anyCategorical <- sapply(dataset, is.numeric) == FALSE

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("correlation"))
  }

  num_dat <- mutate_all(dataset, radiant.data::as_numeric)

  ## calculate the correlation matrix with p.values using the psych package
  if (hcor) {
    cmath <- try(sshhr(polycor::hetcor(dataset, ML = FALSE, std.err = hcor_se)), silent = TRUE)
    if (inherits(cmath, "try-error")) {
      message("Calculating the heterogeneous correlation matrix produced an error.\nUsing standard correlation matrix instead")
      hcor <- "Calculation failed"
      cmat <- sshhr(psych::corr.test(num_dat, method = method))
    } else {
      cmat <- list()
      cmat$r <- cmath$correlations
      cmat$p <- matrix(NA, ncol(cmat$r), nrow(cmat$r))
      rownames(cmat$p) <- colnames(cmat$p) <- colnames(cmat$r)
      if (hcor_se) {
        cmat_z <- cmat$r / cmath$std.errors
        cmat$p <- 2*pnorm(abs(cmat_z), lower.tail = FALSE)
      }
    }
    rm(cmath)
  } else {
    cmat <- sshhr(psych::corr.test(num_dat, method = method))
  }

  ## calculate covariance matrix
  cvmat <- sshhr(cov(num_dat, method = method))
  rm(num_dat, envir)

  if (sum(anyCategorical) > 0) {
    if (isTRUE(hcor)) {
      adj_text <- "\n\nNote: Categorical variables are assumed to be ordinal and were calculated using polycor::hetcor\n\n"
    } else {
      adj_text <- "\n\nNote: Categorical variables were included without adjustment\n\n"
    }
  } else {
    adj_text <- "\n\n"
  }
  descr <- paste0("## Correlation matrix\n\nCorrelations were calculated using the \"", df_name, "\" dataset", adj_text, "Variables used:\n\n* ", paste0(vars, collapse = "\n* "))

  as.list(environment()) %>% add_class("correlation") %>% add_class("rcorr")
}

#' Summary method for the correlation function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/correlation.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{correlation}}
#' @param cutoff Show only correlations larger than the cutoff in absolute value. Default is a cutoff of 0
#' @param covar Show the covariance matrix (default is FALSE)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' result <- correlation(diamonds, c("price", "carat", "table"))
#' summary(result, cutoff = .3)
#'
#' @seealso \code{\link{correlation}} to calculate results
#' @seealso \code{\link{plot.correlation}} to plot results
#'
#' @export
summary.correlation <- function(object, cutoff = 0, covar = FALSE, dec = 2, ...) {

  if (is.character(object)) return(object)

  ## calculate the correlation matrix with p.values using the psych package
  cr <- object$cmat$r
  crf <- try(format_nr(cr, dec = dec, na.rm = FALSE), silent = TRUE)
  if (inherits(crf, "try-error")) {
    cr <- round(cr, dec)
  } else {
    cr[1:nrow(cr), 1:ncol(cr)] <- crf
  }
  cr[is.na(object$cmat$r)] <- "-"
  cr[abs(object$cmat$r) < cutoff] <- ""
  ltmat <- lower.tri(cr)
  cr[!ltmat] <- ""

  cp <- object$cmat$p
  cpf <- try(format_nr(cp, dec = dec, na.rm = FALSE), silent = TRUE)
  if (inherits(cpf, "try-error")) {
    cp <- round(cp, dec)
  } else {
    cp[1:nrow(cp), 1:ncol(cp)] <- cpf
  }
  cp[is.na(object$cmat$p)] <- "-"
  cp[abs(object$cmat$r) < cutoff] <- ""
  cp[!ltmat] <- ""

  cat("Correlation\n")
  cat("Data        :", object$df_name, "\n")
  method <- paste0(toupper(substring(object$method, 1, 1)), substring(object$method, 2))
  if (is.character(object$hcor)) {
    cat(paste0("Method      : ", method, " (adjustment using polycor::hetcor failed)\n"))
  } else if (isTRUE(object$hcor)) {
    if (sum(object$anyCategorical) > 0) {
      cat(paste0("Method      : Heterogeneous correlations using polycor::hetcor\n"))
    } else {
      cat(paste0("Method      : ", method, " (no adjustment applied)\n"))
    }
  } else {
    cat("Method      :", method, "\n")
  }
  if (cutoff > 0) {
    cat("Cutoff      :", cutoff, "\n")
  }
  if (!is_empty(object$data_filter)) {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables   :", paste0(object$vars, collapse = ", "), "\n")
  cat("Null hyp.   : variables x and y are not correlated\n")
  cat("Alt. hyp.   : variables x and y are correlated\n")
  if (sum(object$anyCategorical) > 0) {
    if (isTRUE(object$hcor)) {
      cat("** Variables of type {factor} are assumed to be ordinal **\n\n")
    } else {
      cat("** Variables of type {factor} included without adjustment **\n\n")
    }
  } else if (isTRUE(object$hcor)) {
    cat("** No variables of type {factor} selected. No adjustment applied **\n\n")
  } else {
    cat("\n")
  }

  cat("Correlation matrix:\n")
  cr[-1, -ncol(cr), drop = FALSE] %>%
   format(justify = "right") %>%
   print(quote = FALSE)

  if (!isTRUE(object$hcor) || isTRUE(object$hcor_se)) {
    cat("\np.values:\n")
    cp[-1, -ncol(cp), drop = FALSE] %>%
     format(justify = "right") %>%
     print(quote = FALSE)
  }

  if (covar) {
    cvr <- apply(object$cvmat, 2, format_nr, dec = dec) %>%
      set_rownames(rownames(object$cvmat))
    cvr[abs(object$cmat$r) < cutoff] <- ""
    ltmat <- lower.tri(cvr)
    cvr[!ltmat] <- ""

    cat("\nCovariance matrix:\n")
    cvr[-1, -ncol(cvr), drop = FALSE] %>%
     format(justify = "right") %>%
     print(quote = FALSE)
  }

  return(invisible())
}

#' Print method for the correlation function
#'
#' @param x Return value from \code{\link{correlation}}
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.rcorr <- function(x, ...) summary.correlation(x, ...)

#' Plot method for the correlation function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/correlation.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{correlation}}
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param jit A numeric vector that determines the amount of jittering to apply to the x and y variables in a scatter plot. Default is 0. Use, e.g., 0.3 to add some jittering
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' result <- correlation(diamonds, c("price", "carat", "table"))
#' plot(result)
#'
#' @seealso \code{\link{correlation}} to calculate results
#' @seealso \code{\link{summary.correlation}} to summarize results
#'
#' @importFrom graphics plot
#'
#' @export
plot.correlation <- function(x, nrobs = -1, jit = c(0, 0), dec = 2, ...) {

  if (is.character(x)) return(NULL)
  if (is.null(x$dataset)) {
    if (any(sapply(x, is.factor))) {
      x <- correlation(x, hcor = TRUE, hcor_se = FALSE)
    } else {
      x <- correlation(x, hcor = FALSE)
    }
  }

  cor_text <- function(r, p, dec = 2) {
    if (is.na(p)) p <- 1
    sig <- symnum(
      p, corr = FALSE, na = TRUE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " ")
    )

    rt <- format(r, digits = dec)
    cex <- 0.5 / strwidth(rt)
    plot(c(0, 1), c(0, 1), ann = FALSE, type = "n", xaxt = "n", yaxt = "n")
    text(.5, .5, rt, cex = cex * abs(r))
    text(.8, .8, sig, cex = cex, col = "blue")
  }

  cor_label <- function(label, longest) {
    plot(c(0, 1), c(0, 1), ann = FALSE, type = "n", xaxt = "n", yaxt = "n")
    cex <- 0.5 / strwidth(longest)
    text(.5, .5, label, cex = cex)
  }

  cor_plot <- function(x, y, nobs = 1000) {
    if (nobs != Inf && nobs != -1) {
      ind <- sample(seq_len(length(y)), min(nobs, length(y)))
      x <- x[ind]
      y <- y[ind]
    }
    if (is.factor(y) && is.factor(x)) {
      # plot(x, y, col = y, axes = FALSE, xlab = "", ylab = "")
      plot(x, y, axes = FALSE, xlab = "", ylab = "")
    } else if (is.factor(y) & is.numeric(x)) {
      plot(y, x, ann = FALSE, xaxt = "n", yaxt = "n", horizontal=TRUE)
    } else if (is.numeric(y) & is.factor(x)) {
      plot(x, y, ann = FALSE, xaxt = "n", yaxt = "n")
    } else {
      y = as.numeric(y)
      x = as.numeric(x)
      plot(jitter(x, jit[1]), jitter(y, jit[2]), ann = FALSE, xaxt = "n", yaxt = "n")
    }
  }

  cor_mat <- function(dataset, cmat, pmat = NULL, dec = 2, nobs = 1000) {
    nr <- ncol(dataset)
    ops <- par(mfrow = c(nr, nr), mar = rep(0.2, 4))
    on.exit(par(ops))
    cn <- colnames(dataset)
    longest <- names(sort(sapply(cn, nchar), decreasing = TRUE))[1]
    for (i in seq_along(cn)) {
      for (j in seq_along(cn)) {
        if (i == j) {
          cor_label(cn[i], longest)
        } else if (i > j) {
          cor_plot(dataset[[i]], dataset[[j]], nobs = nobs)
        } else {
          cor_text(cmat[i, j], pmat[i, j], dec = 2)
        }
      }
    }
  }

  cor_mat(x$dataset, cmat = x$cmat$r, pmat = x$cmat$p, dec = dec, nobs = nrobs)
}

#' Store a correlation matrix as a (long) data.frame
#'
#' @details Return the correlation matrix as a (long) data.frame. See \url{https://radiant-rstats.github.io/docs/basics/correlation.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{correlation}}
#' @param labels Column names for the correlation pairs
#' @param ... further arguments passed to or from other methods
#'
#' @export
cor2df <- function(object, labels = c("label1", "label2"), ...) {
  cmat <- object$cmat$r
  correlation <- cmat[lower.tri(cmat)]
  distance <- 0.5 * (1 - correlation)
  labs <- as.data.frame(t(combn(colnames(cmat), 2)))
  colnames(labs) <- labels
  cbind(labs, correlation, distance)
}
