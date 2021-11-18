# radiant.basics 1.4.1.0

* Fixed `is_empty` function clash with `rlang`
* Adjustments to work with the latest version of `shiny` and `bootstrap4`

# radiant.basics 1.4.0.0

Adjusted DESCRIPTION file by adding 'markdown' to the Suggests section. This addresses an issue in radiant.basics, similar to the issue linked below  
https://github.com/radiant-rstats/radiant/issues/157. This is issue originated with https://github.com/yihui/knitr/issues/1864

# radiant.basics 1.3.4.0

* Minor adjustments in anticipation of dplyr 1.0.0

# radiant.basics 1.3.0.0

* Documentation updates to link to new video tutorials
* Use `patchwork` for grouping multiple plots together
* Use `polycor::hetcor` to calculate correlations for a mix of numeric and categorical variables
* Updated correlation plot that accommodates a mix of numeric and categorical variables
* Fix for sd estimate in `single_prop` and `compare_prop` functions
* Add dimension labels to all tables in _Basics > Cross-tabs_

# radiant.basics 1.2.0.0

* Update action buttons that initiate calculations when one or more relevant inputs are changed. When, for example, a CLT simulation should be updated, a spinning "refresh" icon will be shown
* Allow fractions as input for the `Goodness of fit` and `Probability calculator > Discrete` tools

# radiant.basics 1.1.4.0

* Summary statistics provided for _single_mean_, _single_prop_, _compare_means_, and _compare_props_ are now consistent
* `n_missing` were not show correctly for _compare_means_ and _compare_props_

# radiant.basics 1.1.3.0

* Fix for code generation from the probability calculator when the `distribution` type is set to binomial
* Fix for input restoration from a state file for the probability calculator. For the _value_ or _probability_ inputs two sided values might be restored when only a one-sided input was previously specified

# radiant.basics 1.1.1.0
  
* Documentation updates (i.e., key functions for each tool)
* Improvements in `goodness` and `prob_dics` to allow fractions in generated code sent to _Report > rmd_ or _Report > R_
* Improved checks for variables that show no variation
* Numerous small code changes to support enhanced auto-completion, tooltips, and annotations in shinyAce 0.4.1

# radiant.basics 1.0.0.0

* Flexible adjustment of level of jitter in `plot.correlation`
* Support for variables of type `ts`

# radiant.basics 0.9.9.0

* Various fixes to address (soft) deprecations in dplyr 0.8.0

# radiant.basics 0.9.8.0

* Option to pass additional arguments to `shiny::runApp` when starting radiant such as the port to use. For example, radiant.basics::radiant.basics("https://github.com/radiant-rstats/docs/raw/gh-pages/examples/demo-dvd-rnd.state.rda", port = 8080) 
* Catch settings where the number of levels in a comparison of means or proportions is the same as the number of rows in the data (e.g., grouping by a unique identifier)
* Show significant stars for `Compare means` and `Compare proportions` even when `Show additional output` is not selected
* `ci` in summary table `compare_means` and `compare_props` should be margin of err (`me`)
* Option to use `Z-test` in `single_prop`

# radiant.basics 0.9.7.2

* Load a state file on startup by providing a (relative) file path or a url

# radiant.basics 0.9.7.0

* Using [`shinyFiles`](https://github.com/thomasp85/shinyFiles) to provide convenient access to data located on a server

# radiant.basics 0.9.5.0

## Major changes

* Various changes to the code to accommodate the use of `shiny::makeReactiveBinding`. The advantage is that the code generated for _Report > Rmd_ and _Report > R_ will no longer have to use `r_data` to store and access data. This means that code generated and used in the Radiant browser interface will be directly usable without the browser interface as well.

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
