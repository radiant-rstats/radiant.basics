# radiant.design 0.9.3.2

## Major changes

* Various changes to the code to accomodate the use of `shiny::makeReactiveBinding`. The advantage is that the code generated for _Report > Rmd_ and _Report > R_ will no longer have to use `r_data` to store and access data. This means that code generated and used in the Radiant browser interface will be directly usable without the browser interface as well.

# radiant.basics 0.9.2.0

## Major changes

* Upload and download data using the Rstudio file browser. Allows using relative paths to files (e.g., data or images inside an Rstudio project)
* Variable selection in Summary tabs only to simplify Plot interface

## Bug fixes

* Fix for [#43](https://github.com/radiant-rstats/radiant/issues/43) where scatter plot was not shown for a dataset with less than 1,000 rows

# radiant.basics 0.9.0.4

## Minor changes

* Format tables with thousand separator
* Added print method for return from `correlation`

# radiant.basics 0.9.0.3

## Minor changes

* Enhanced keyboard shortcuts
* `summary.single_prop` will not print row numbers 
* Added log.normal as an option in the probability calculator
* The correlation plot now has an option to select a sample of data for scatter plots (e.g., 1K, 5K, 10K, or All)

# radiant.basics 0.8.9.0

## Minor changes

* Upgraded broom dependency to 0.4.3
* Upgraded dplyr dependency to 0.7.4
* Upgraded tidyr dependency to 0.7.2
* Fixed CI printing error for `compare_prop`
* Applied `styler` to code
* Long lines of code generated for _Report > Rmd_ and _Report > R_ will be wrapped to enhance readability 

# radiant.basics 0.8.3.0

## Minor changes

* `correlation` defaults to all variables if no value for `var` is provided
* Renamed methods `summary.correlation_` and `plot.correlation_` to `summary.correlation` and `plot.correlation`
* Added `tab` argument to `goodness` and `cross_tabs` so a table object can be passed directly
* Documentation updates
* Scatter plots in _Correlation > Plot_ are now based on 1,000 data points by default. Use _Report > Rmd_ or _Report > R_ to adjust (e.g., `plot(result, n = -1)`)

## Bug fixes

* Fix for level ordering in goodness-of-fit expected-values plot
* Code clean-up and various minor fixes and improvements

# radiant.basics 0.8.0.0

## Major changes

- Show dataset name in output if dataframe passed directly to analysis function
- Scatter plots in Basics > Correlation > Plot now based on 1,000 data points by default. Add n = -1 to use all data points 
- As an alternative to using the Estimate button to run a model you can now also use CTRL-enter or CMD-enter
- Use ALT-enter to put code into _Report > Rmd_ or _Report > R_
- Documentation added on how to customize plots

## Bug fixes

- Fixed correlation dropdown. Correlations did not change when method was changed (thanks @Fiordmaster)
- Improved formatting for small negative values in Basics > Correlation
- Convert numeric bounds to integer in Basics > Probability calculator > Binomial to avoid warnings

## Deprecated

- Use of *_each is deprecated
