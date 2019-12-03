#' Central Limit Theorem simulation
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/clt.html} for an example in Radiant
#'
#' @param dist Distribution to simulate
#' @param n Sample size
#' @param m Number of samples
#' @param norm_mean Mean for the normal distribution
#' @param norm_sd Standard deviation for the normal distribution
#' @param binom_size Size for the binomial distribution
#' @param binom_prob Probability for the binomial distribution
#' @param unif_min Minimum for the uniform distribution
#' @param unif_max Maximum for the uniform distribution
#' @param expo_rate Rate for the exponential distribution
#'
#' @importFrom stats rexp rnorm runif rbinom
#'
#' @return A list with the name of the Distribution and a matrix of simulated data
#'
#' @examples
#' clt("Uniform", 10, 10, unif_min = 10, unif_max = 20)
#'
#' @export
clt <- function(
  dist, n = 100, m = 100,
  norm_mean = 0, norm_sd = 1,
  binom_size = 10, binom_prob = 0.2,
  unif_min = 0, unif_max = 1,
  expo_rate = 1
) {
  if (dist == "Uniform") {
    sim <- matrix(runif(n * m, min = unif_min, max = unif_max), n, m)
  } else if (dist == "Normal") {
    sim <- matrix(rnorm(n * m, mean = norm_mean, sd = norm_sd), n, m)
  } else if (dist == "Exponential") {
    sim <- matrix(rexp(n * m, rate = expo_rate), n, m)
  } else if (dist == "Binomial") {
    sim <- matrix(rbinom(n * m, size = binom_size, prob = binom_prob), n, m)
  }

  add_class(list(dist = dist, sim = sim), "clt")
}

#' Plot method for the Central Limit Theorem simulation
#'
#' @details See \url{https://radiant-rstats.github.io/docs/basics/clt.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{clt}}
#' @param stat Statistic to use (sum or mean)
#' @param bins Number of bins to use
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' clt("Uniform", 100, 100, unif_min = 10, unif_max = 20) %>% plot()
#'
#' @export
plot.clt <- function(x, stat = "sum", bins = 15, ...) {

  if (stat == "sum") {
    sstat <- data.frame(stat = colSums(x$sim), stringsAsFactors = FALSE)
  } else {
    sstat <- data.frame(stat = colMeans(x$sim), stringsAsFactors = FALSE)
  }

  m <- dim(x$sim)[2]
  data1 <- data.frame(sample_1 = x$sim[, 1], stringsAsFactors = FALSE)
  datam <- data.frame(sample_m = x$sim[, m], stringsAsFactors = FALSE)

  plot_list <- list()
  plot_list[[1]] <- visualize(data1, xvar = "sample_1", bins = bins, custom = TRUE) +
    labs(x = "Histogram of sample #1")

  plot_list[[2]] <- visualize(datam, xvar = "sample_m", bins = bins, custom = TRUE) +
    labs(x = paste0("Histogram of sample #", m))

  plot_list[[3]] <- visualize(sstat, xvar = "stat", bins = bins, custom = TRUE) +
    labs(x = ifelse(stat == "Sum", "Histogram of sample sums", "Histogram of sample means"))


  plot_list[[4]] <- visualize(sstat, xvar = "stat", type = "density", custom = TRUE) +
    stat_function(fun = dnorm, args = list(
      mean = mean(sstat[[1]]),
      sd = sd(sstat[[1]])
    ), color = "black", size = 1) +
    labs(x = ifelse(stat == "Sum", "Density of sample sums", "Density of sample means"))

  patchwork::wrap_plots(plot_list, ncol = 2) + patchwork::plot_annotation(title = glue("CLT: {x$dist} distribution"))
}
