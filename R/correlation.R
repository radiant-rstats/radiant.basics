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
#' @param jit A numeric vector that determines the amount of jittering to apply to scatter plot. Default is 0. Use, e.g., 0.3 to add some jittering
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
#' @importFrom ggplot2 alpha
#'
#' @export
plot.correlation <- function(x, nrobs = -1, jit = c(0, 0), dec = 2, ...) {

  if (is.character(x)) return(NULL)
  ind_scale <- 1000
  if (is.null(x$dataset)) {
    dataset <- x
    method <- "pearson"
    hcor_se <- FALSE
    if (any(sapply(dataset, is.factor))) {
      dataset <- mutate_if(dataset, is.Date, as_numeric)
      hcor <- TRUE
      cmath <- try(sshhr(polycor::hetcor(dataset, ML = FALSE, std.err = hcor_se)), silent = TRUE)
      if (inherits(cmath, "try-error")) {
        message("Calculating the heterogeneous correlation matrix produced an error.\nUsing standard correlation matrix instead")
        hcor <- FALSE
      } else {
        cmat <- list()
        cmat$r <- cmath$correlations
        cmat$p <- matrix(1, ncol(cmat$r), nrow(cmat$r))
      }
    } else {
      hcor <- FALSE
    }
  } else {
    dataset <- x$dataset
    ## defined method to be use in panel.plot
    method <- x$method
    ## use heterogeneous correlations
    hcor <- x$hcor
    hcor_se <- x$hcor_se
    cmat <- x$cmat
  }

  ## based mostly on http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=137
  panel.plot <- function(x, y) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))

    if (isTRUE(hcor)) {
      ind <- as.integer(round(c(ind_scale*(x[1] - x[2]), ind_scale*(y[1] - y[2])), 0))
      ct <- c()
      ct$estimate <- cmat$r[ind[1], ind[2]]
      if (isTRUE(hcor_se)) {
        ct$p.value <- cmat$p[ind[1], ind[2]]
      } else {
        ct$p.value <- 1
      }
    } else {
      ct <- sshhr(cor.test(x[-1:-2], y[-1:-2], method = method))
    }
    sig <- symnum(
      ct$p.value, corr = FALSE, na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " ")
    )
    r <- ct$estimate
    rt <- format(r, digits = dec)[1]
    cex <- 0.5 / strwidth(rt)

    text(.5, .5, rt, cex = cex * abs(r))
    text(.8, .8, sig, cex = cex, col = "blue")
  }
  panel.smooth <- function(x, y) {
    x <- x[-1:-2]
    y <- y[-1:-2]
    if (nrobs > 0 & length(x) > nrobs) {
      ind <- sample(1:length(x), nrobs)
      x <- x[ind]
      y <- y[ind]
    }
    points(
      jitter(x, jit[1]), jitter(y, jit[length(jit)]), pch = 16,
      col = ggplot2::alpha("black", 0.5)
    )
    ## uncomment the lines below if you want linear and loess lines
    ## in the scatter plot matrix
    # abline(lm(y~x), col="red")
    # lines(stats::lowess(y~x), col="blue")
  }

  num_dat <- mutate_all(dataset, as_numeric)
  ## hack so I can pass index information for the original
  ## data (with factors) to panel.plot
  ind_dat <- round(num_dat[c(1, 1),], 3)
  ind_dat[1,] <- ind_dat[2, ] + (seq_len(ncol(num_dat)) / ind_scale)
  num_dat <- bind_rows(ind_dat, num_dat)

  pairs(num_dat, lower.panel = panel.smooth, upper.panel = panel.plot)
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
