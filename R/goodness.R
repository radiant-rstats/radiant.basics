#' Evaluate if sample data for a categorical variable is consistent wtih a hypothesized distribution
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/goodness.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var A categorical variable
#' @param p Hypothesized distribution as a number, fraction, or numeric vector. If unspecified, defaults to an even distribution
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in goodness as an object of class goodness
#'
#' @examples
#' result <- goodness("newspaper", "Income")
#'
#' @seealso \code{\link{summary.goodness}} to summarize results
#' @seealso \code{\link{plot.goodness}} to plot results
#'
#' @export
goodness <- function(dataset, var, p = NULL,
                     data_filter = "") {

	dat <- getdata(dataset, var, filt = data_filter)
  if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

  ## creating and cleaning up the table
	tab <- table(dat[[var]])
	tab[is.na(tab)] <- 0
	tab <- as.table(tab)

	if (is_not(p) || p == "") {
		p <- rep(1/length(tab), length(tab))
	} else if (is.character(p)) {
	  p <- gsub(","," ", p) %>% strsplit("\\s+") %>% unlist %>% strsplit("/")
		asNum <- function(x) ifelse(length(x) > 1, as.numeric(x[1])/as.numeric(x[2]), as.numeric(x[1]))
    p <- sshhr(sapply(p, asNum))

	  if (anyNA(p))
	    return(paste0("Invalid inputs: ", paste0(p, collapse = ", ")) %>% add_class("goodness"))

	  lp <- length(p); lt <- length(tab)
	  if (lt != lp && lt %% lp == 0) p <- rep(p, lt / lp)

	  if (!is.numeric(p) || sum(p) != 1)
	    return(
  		  paste0("Probabilities do not sum to 1 (",round(sum(p),3),")\nUse fractions if appropriate. Variable ", var, " has ", length(tab), " unique values.") %>%
  		  add_class("goodness")
		  )
	}

	cst <- sshhr( chisq.test(tab, p = p, correct = FALSE) )

	## adding the chi-sq table
	cst$chi_sq	<- with(cst, (observed - expected)^2 / expected)

	## dat not needed in summary or plot
	rm(dat)

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
#' result <- goodness("newspaper", "Income", c(.3, .7))
#' summary(result, check = c("observed","expected","chi_sq"))
#' newspaper %>% goodness("Income", "1/3 2/3") %>% summary("observed")
#'
#' @seealso \code{\link{goodness}} to calculate results
#' @seealso \code{\link{plot.goodness}} to plot results
#'
#' @export
summary.goodness <- function(object, check = "", dec = 2, ...) {

  if (is.character(object)) return(object)

  cat("Goodness of fit test\n")
	cat("Data     :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
	cat("Variable :", object$var, "\n")
	cat("Specified:", object$p, "\n")
	cat("Null hyp.: the distribution of", object$var, "is consistent with the specified distribution\n")
	cat("Alt. hyp.: the distribution of", object$var, "is not consistent with the specified distribution\n")

	if ("observed" %in% check) {
		cat("\nObserved:\n")
		object$cst$observed %>% {.["Total"] <- sum(.); .} %>% print
	}

	if ("expected" %in% check) {
		cat("\nExpected: total x p\n")
		object$cst$expected %>% {.["Total"] <- sum(.); .} %>% round(dec) %>% print
	}

	if ("chi_sq" %in% check) {
		cat("\nContribution to chi-squared: (o - e)^2 / e\n")
		object$cst$chi_sq %>% {.["Total"] <- sum(.); .} %>% round(dec) %>% print
	}

	if ("dev_std" %in% check) {
		cat("\nDeviation standardized: (o - e) / sqrt(e)\n")
		print(round(object$cst$residuals, dec))
	}

	res <- object$cst %>% tidy
	elow <- sum(object$cst$expected < 5)

  if (elow > 0) {
  	res$p.value <- chisq.test(object$cst$observed, simulate.p.value = TRUE, B = 2000) %>% tidy %>% .$p.value
  	res$parameter <- paste0("*",res$parameter,"*")
  }

  round_fun <- function(x) is.na(x) || is.character(x)
	res[!sapply(res, round_fun)] %<>% round(dec + 1)

	if (res$p.value < .001) res$p.value  <- "< .001"
	cat(paste0("\nChi-squared: ", res$statistic, " df(", res$parameter, "), p.value ", res$p.value), "\n\n")
	cat(paste(sprintf("%.1f",100 * (elow / length(object$cst$expected))),"% of cells have expected values below 5\n"), sep = "")
	if (elow > 0) cat("p.value for chi-squared statistics obtained using simulation (2000 replicates)")
}

#' Plot method for the goodness function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/goodness} for an example in Radiant
#'
#' @param x Return value from \code{\link{goodness}}
#' @param check Show plots for variable var. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), and "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e))
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- goodness("newspaper", "Income")
#' plot(result, check = c("observed","expected","chi_sq"))
#' newspaper %>% goodness("Income") %>% plot(c("observed","expected"))
#'
#' @seealso \code{\link{goodness}} to calculate results
#' @seealso \code{\link{summary.goodness}} to summarize results
#'
#' @export
plot.goodness <- function(x, check = "", shiny = FALSE, custom = FALSE, ...) {

	object <- x; rm(x)
  if (is.character(object)) return(object)
	plot_list <- list()

	if ("observed" %in% check) {
	  fact_names <- names(object$cst$observed)
    tab <- as.data.frame(object$cst$observed, check.names = FALSE)
		colnames(tab)[1] <- object$var
 	  tab[[1]] %<>% as.factor %>% factor(levels = fact_names)
 	  tab[["Freq"]] %<>% {. / sum(.)}
		plot_list[['observed']] <-
		  ggplot(tab, aes_string(x = object$var, y = "Freq")) +
		    geom_bar(stat="identity", alpha = .5) +
		    ggtitle(paste("Observed frequencies for",object$var)) +
		    xlab(object$var) +
		    ylab("") +
		    scale_y_continuous(labels = scales::percent)
	}

	if ("expected" %in% check) {
	  fact_names <- names(object$cst$expected)
	  tab <- as.data.frame(object$cst$expected, check.names = FALSE)
		colnames(tab)[1] <- "Freq"
		tab[[object$var]] <- rownames(tab)
 	  tab[["Freq"]] %<>% {. / sum(.)}
		plot_list[['expected']] <-
  		ggplot(tab, aes_string(x = object$var, y = "Freq")) +
		    geom_bar(stat="identity", alpha = .5) +
		    ggtitle(paste("Expected frequencies for",object$var)) +
		    xlab(object$var) +
		    ylab("") +
		    scale_y_continuous(labels = scales::percent)
	}

	if ("chi_sq" %in% check) {
  	tab <- as.data.frame(object$cst$chi_sq, check.names = FALSE)
		colnames(tab)[1] <- object$var
		plot_list[['chi_sq']] <-
  		ggplot(tab, aes_string(x = object$var, y = "Freq")) +
		    geom_bar(stat="identity", alpha = .5) +
		    ggtitle(paste("Contribtion to chi-squared for ",object$var)) +
		    xlab(object$var) +
		    ylab("")
  }

	if ("dev_std" %in% check) {
  	tab <- as.data.frame(object$cst$residuals, check.names = FALSE)
		colnames(tab)[1] <- object$var
		plot_list[['dev_std']] <-
  		ggplot(tab, aes_string(x = object$var, y = "Freq")) +
		    geom_bar(stat="identity", position = "dodge", alpha = .5) +
		    geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
		    geom_text(x = 1, y = 2.11, label = "95%") +
		    geom_text(x = 1, y = 1.49, label = "90%") +
		    ggtitle(paste("Deviation standardized for",object$var)) +
		    xlab(object$var) +
		    ylab("")
	}

  if (custom)
    if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

	sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>%
	  {if (shiny) . else print(.)}
}
