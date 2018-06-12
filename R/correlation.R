#' Calculate correlations for two or more variables
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/correlation.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param vars Variables to include in the analysis. Default is all but character and factor variables with more than two unique values are removed
#' @param method Type of correlations to calculate. Options are "pearson", "spearman", and "kendall". "pearson" is the default
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
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
#'
#' @export
correlation <- function(dataset, vars = "", method = "pearson", data_filter = "") {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))

  ## data.matrix as the last step in the chain is about 25% slower using
  ## system.time but results (using diamonds and mtcars) are identical
  dataset <- get_data(dataset, vars, filt = data_filter) %>%
    mutate_all(funs(as_numeric))

  ## calculate the correlation matrix with p.values using the psych package
  cmat <- sshhr(psych::corr.test(dataset, method = method))

  ## calculate covariance matrix
  cvmat <- sshhr(cov(dataset, method = method))

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
  cat("Data     :", object$df_name, "\n")
  cat("Method   :", object$method, "\n")
  if (cutoff > 0) {
    cat("Cutoff   :", cutoff, "\n")
  }
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("Filter   :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables:", paste0(object$vars, collapse = ", "), "\n")
  cat("Null hyp.: variables x and y are not correlated\n")
  cat("Alt. hyp.: variables x and y are correlated\n\n")

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
#' @param jit Level of jittering to apply to scatter plot. Default is .3. Use 0 for no jittering
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
plot.correlation <- function(x, nrobs = -1, jit = .3, dec = 2, ...) {

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
      jitter(x, jit), jitter(y, jit), pch = 16,
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
