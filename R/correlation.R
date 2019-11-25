#' Calculate correlations for two or more variables
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/correlation.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param vars Variables to include in the analysis. Default is all but character and factor variables with more than two unique values are removed
#' @param method Type of correlations to calculate. Options are "pearson", "spearman", and "kendall". "pearson" is the default
#' @param mcor Use psych::mixedCor to calculate the correlation matrix
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
#' @importFrom psych corr.test mixedCor
#' @importFrom lubridate is.Date
#'
#' @export
correlation <- function(
  dataset, vars = "", method = "pearson", mcor = FALSE,
  data_filter = "", envir = parent.frame()
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))

  ## data.matrix as the last step in the chain is about 25% slower using
  ## system.time but results (using diamonds and mtcars) are identical
  dataset <- get_data(dataset, vars, filt = data_filter, envir = envir)
  anyCategorical <- sapply(dataset, function(x) is.numeric(x) || is.Date(x)) == FALSE

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("correlation"))
  }

  ## calculate the correlation matrix with p.values using the psych package
  if (mcor) {
    mc <- radiant.basics::.mixedCor_cpd(dataset)
    dataset <- mutate_all(dataset, radiant.data::as_numeric)
    cmatr <- try(sshhr(psych::mixedCor(dataset, c = mc$c, p = mc$p, d = mc$d, ncat = Inf)$rho), silent = TRUE)
    if (inherits(cmatr, "try-error")) {
      message("Calculating the mixed correlation matrix produced an error.\nUsing standard correlation matrix instead")
      mcor <- "Calculation failed"
      cmat <- sshhr(psych::corr.test(dataset, method = method))
    } else {
      cmat <- sshhr(psych::corr.test(dataset, method = method))
      cmat$r <- cmatr
    }
  } else {
    dataset <- mutate_all(dataset, radiant.data::as_numeric)
    cmat <- sshhr(psych::corr.test(dataset, method = method))
  }

  ## calculate covariance matrix
  cvmat <- sshhr(cov(dataset, method = method))

  rm(envir)

  if (sum(anyCategorical) > 0) {
    if (isTRUE(mcor)) {
      adj_text <- "\n\nNote: Categorical variables are assumed to be ordinal and were calculated using psych::mixedCor\n\n"
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
  cr <- apply(object$cmat$r, 2, format_nr, dec = dec) %>%
    set_rownames(rownames(object$cmat$r))
  cr[is.na(object$cmat$r)] <- "-"
  cr[abs(object$cmat$r) < cutoff] <- ""
  ltmat <- lower.tri(cr)
  cr[!ltmat] <- ""

  cp <- apply(object$cmat$p, 2, format_nr, dec = dec) %>%
    set_rownames(rownames(object$cmat$p))
  cp[is.na(object$cmat$p)] <- "-"
  cp[abs(object$cmat$r) < cutoff] <- ""
  cp[!ltmat] <- ""

  cat("Correlation\n")
  cat("Data        :", object$df_name, "\n")
  method <- paste0(toupper(substring(object$method, 1, 1)), substring(object$method, 2))
  if (is.character(object$mcor)) {
    cat(paste0("Method      : ", method, " (adjustment using psych::mixedCor failed)\n"))
  } else if (isTRUE(object$mcor)) {
    cat(paste0("Method      : Mixed correlations using psych::mixedCor\n"))
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
    if (isTRUE(object$mcor)) {
      cat("** Categorical variables are assumed to be ordinal **\n")
      cat("** Reported p.values apply to un-adjusted correlations **\n\n")
    } else {
      cat("** Categorical variables included without adjustment **\n\n")
    }
  } else {
    cat("\n")
  }

  cat("Correlation matrix:\n")
  cr[-1, -ncol(cr), drop = FALSE] %>%
   format(justify = "right") %>%
   print(quote = FALSE)

  cat("\np.values:\n")
  cp[-1, -ncol(cp), drop = FALSE] %>%
   format(justify = "right") %>%
   print(quote = FALSE)

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

  ## defined method to be use in panel.plot
  method <- x$method
  ## based mostly on http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=137
  panel.plot <- function(x, y) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    ct <- sshhr(cor.test(x, y, method = method))
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

  {if (is.null(x$dataset)) x else x$dataset} %>%
    pairs(lower.panel = panel.smooth, upper.panel = panel.plot)
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

#' @noRd
#' @importFrom lubridate is.Date
#' @export
.mixedCor_cpd <- function(dataset) {
  cn <- colnames(dataset)
  cnt <- dfct <- pfct <- NULL
  isC <- sapply(dataset, function(x) is.numeric(x) || is.Date(x))
  if (sum(isC) > 0) {
    cnt <- which(isC)
  }

  isFct <- sapply(dataset, is.factor)
  if (sum(isFct) > 0) {
    dorp <- sapply(dataset[,isFct, drop = FALSE], function(x) length(levels(x)))
    isD <- dorp == 2
    if (sum(isD) > 0) {
      dfct <- which(cn %in% names(isD[isD]))
    }
    isP <- dorp > 2
    if (sum(isP) > 0) {
      pfct <- which(cn %in% names(isP[isP]))
    }
  }

  if ((length(cnt) + length(dfct) + length(pfct)) < length(dataset)) {
    cnt <- dfct <- pfct <- NULL
  }
  list(c = cnt, p = pfct, d = dfct)
}
