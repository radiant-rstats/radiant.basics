######### tests ########
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
# library(radiant.basics)
# library(testthat)

context("Compare means")

test_that("compare_means 1", {
  result <- compare_means(diamonds, "cut", "price")
  res1 <- capture.output(summary(result))[9] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "Fair 4,505.238   101 3,749.540 373.093 740.206"
  expect_equal(res1, res2)
})

test_that("compare_means 2", {
  result <- compare_means(diamonds, "cut", "price")
  res1 <- capture_output(summary(result, show = TRUE))
  res2 <- "Pairwise mean comparisons (t-test)\nData      : diamonds \nVariables : cut, price \nSamples   : independent \nConfidence: 0.95 \nAdjustment: None \n\n       cut      mean     n        sd      se      ci\n      Fair 4,505.238   101 3,749.540 373.093 740.206\n      Good 4,130.433   275 3,730.354 224.949 442.848\n Very Good 3,959.916   677 3,895.899 149.732 293.995\n   Premium 4,369.409   771 4,236.977 152.591 299.544\n     Ideal 3,470.224 1,176 3,827.423 111.610 218.977\n\n Null hyp.             Alt. hyp.                        diff     p.value\n Fair = Good           Fair not equal to Good            374.805 0.391  \n Fair = Very Good      Fair not equal to Very Good       545.322 0.177  \n Fair = Premium        Fair not equal to Premium         135.829 0.737  \n Fair = Ideal          Fair not equal to Ideal          1035.014 0.009  \n Good = Very Good      Good not equal to Very Good       170.517 0.528  \n Good = Premium        Good not equal to Premium        -238.976 0.38   \n Good = Ideal          Good not equal to Ideal           660.209 0.009  \n Very Good = Premium   Very Good not equal to Premium   -409.493 0.056  \n Very Good = Ideal     Very Good not equal to Ideal      489.692 0.009  \n Premium = Ideal       Premium not equal to Ideal        899.185 < .001 \n se      t.value df       2.5%     97.5%       \n 435.661  0.860   177.365 -484.941 1234.551    \n 402.018  1.356   134.291 -249.783 1340.427    \n 403.091  0.337   135.759 -661.321  932.979    \n 389.429  2.658   118.618  263.879 1806.149 ** \n 270.225  0.631   528.529 -360.330  701.364    \n 271.820 -0.879   543.242 -772.922  294.971    \n 251.115  2.629   419.577  166.609 1153.809 ** \n 213.784 -1.915  1442.922 -828.853    9.868 .  \n 186.752  2.622  1389.163  123.346  856.039 ** \n 189.052  4.756  1527.729  528.355 1270.015 ***\n\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
  expect_equal(res1, res2)
})

context("Compare proportions")

test_that("compare_props 1", {
  result <- compare_props(titanic, "pclass", "survived")
  res1 <- capture.output(summary(result))[9] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "1st 179 103 282 0.635 0.029 0.056"
  expect_equal(res1, res2)
})

test_that("compare_props 2", {
  result <- compare_props(titanic, "pclass", "survived")
  res1 <- capture_output(summary(result, show = TRUE))
  # cat(paste0(res1, "\n"))
  res2 <- "Pairwise proportion comparisons\nData      : titanic \nVariables : pclass, survived \nLevel     :  in survived \nConfidence: 0.95 \nAdjustment: None \n\n pclass Yes  No   n     p    se    ci\n    1st 179 103 282 0.635 0.029 0.056\n    2nd 115 146 261 0.441 0.031 0.060\n    3rd 131 369 500 0.262 0.020 0.039\n\n Null hyp.   Alt. hyp.              diff  p.value chisq.value df 2.5%  97.5%\n 1st = 2nd   1st not equal to 2nd   0.194 < .001  20.576      1  0.112 0.277\n 1st = 3rd   1st not equal to 3rd   0.373 < .001  104.704     1  0.305 0.441\n 2nd = 3rd   2nd not equal to 3rd   0.179 < .001  25.008      1  0.107 0.250\n    \n ***\n ***\n ***\n\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
  expect_equal(res1, res2)
})

context("Single proportion")

test_that("single_prop 1", {
  result <- single_prop(diamonds, "color")
  expect_equal(result$lev, "D")
  res1 <- capture_output(summary(result))
  res2 <- "Single proportion test (binomial exact)\nData      : diamonds \nVariable  : color \nLevel     : D in color \nConfidence: 0.95 \nNull hyp. : the proportion of D in color = 0.5 \nAlt. hyp. : the proportion of D in color not equal to 0.5 \n\n  prop mean     sd     n n_missing\n 0.127  382 18.258 3,000         0\n\n   diff  ns p.value  2.5% 97.5%    \n -0.373 382  < .001 0.116 0.140 ***\n\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
  expect_equal(res1, res2)
})

test_that("single_prop 2", {
  result <- single_prop(diamonds, "clarity", lev = "IF", comp_value = 0.05)
  expect_equal(result$lev, "IF")
  res1 <- capture_output(summary(result))
  res2 <- "Single proportion test (binomial exact)\nData      : diamonds \nVariable  : clarity \nLevel     : IF in clarity \nConfidence: 0.95 \nNull hyp. : the proportion of IF in clarity = 0.05 \nAlt. hyp. : the proportion of IF in clarity not equal to 0.05 \n\n  prop mean    sd     n n_missing\n 0.033   99 9.784 3,000         0\n\n   diff ns p.value  2.5% 97.5%    \n -0.017 99  < .001 0.027 0.040 ***\n\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
  expect_equal(res1, res2)
})

context("Single mean")

test_that("single_mean 1", {
  result <- single_mean(diamonds, "carat")
  res1 <- capture.output(summary(result))[12] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "0.794 0.009  91.816  < .001 2999 0.777 0.811 ***"
  expect_equal(res1, res2)
})

test_that("single_mean 2", {
  result <- single_mean(titanic, "age", comp_value = 40)
  res1 <- capture.output(summary(result))[12] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "-10.187 0.445   -22.9  < .001 1042 28.94 30.686 ***"
  expect_equal(res1, res2)
})
