% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlation.R
\name{plot.correlation}
\alias{plot.correlation}
\title{Plot method for the correlation function}
\usage{
\method{plot}{correlation}(x, nrobs = -1, jit = c(0, 0), dec = 2, ...)
}
\arguments{
\item{x}{Return value from \code{\link{correlation}}}

\item{nrobs}{Number of data points to show in scatter plots (-1 for all)}

\item{jit}{A numeric vector that determines the amount of jittering to apply to the x and y variables in a scatter plot. Default is 0. Use, e.g., 0.3 to add some jittering}

\item{dec}{Number of decimals to show}

\item{...}{further arguments passed to or from other methods.}
}
\description{
Plot method for the correlation function
}
\details{
See \url{https://radiant-rstats.github.io/docs/basics/correlation.html} for an example in Radiant
}
\examples{
result <- correlation(diamonds, c("price", "carat", "table"))
plot(result)

}
\seealso{
\code{\link{correlation}} to calculate results

\code{\link{summary.correlation}} to summarize results
}
