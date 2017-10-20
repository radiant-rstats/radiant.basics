# CHANGES IN radiant.basics 0.8.7.5

* Upgraded tidyr dependency to 0.7
* Fixed CI printing error for `compare_prop`

# CHANGES IN radiant.basics 0.8.7.1

* Upgraded dplyr dependency to 0.7.1

# CHANGES IN radiant.basics 0.8.3

* `correlation` defaults to all variables if no value for `var` is provided
* Renamed methods `summary.correlation_` and `plot.correlation_` to `summary.correlation` and `plot.correlation`
* Added `tab` argument to `goodness` and `cross_tabs` so a table object can be passed directly
* Documentation updates
* Scatter plots in _Correlation > Plot_ are now based on 1,000 data points by default. Use _R > Report_ to adjust (e.g., `plot(result, n = -1)`)
* Fix for level ordering in goodness-of-fit expected-values plot
* Code clean-up and various minor fixes and improvements

# CHANGES IN radiant.basics 0.8.0

## NEW FEATURES

- Show dataset name in output if dataframe passed directly to analysis function
- Scatter plots in Basics > Correlation > Plot now based on 1,000 data points by default. Add n = -1 to use all data points 
- As an alternative to using the Estimate button to run a model you can now also use CTRL-enter or CMD-enter
- Use ALT-enter to put code into R > Report
- Documentation added on how to customize plots

## BUG FIXES

- Fixed correlation dropdown. Correlations did not change when method was changed (thanks @Fiordmaster)
- Improved formatting for small negative values in Basics > Correlation
- Convert numeric bounds to integer in Basics > Probability calculator > Binomial to avoid warnings

## Deprecated

- Use of *_each is deprecated
