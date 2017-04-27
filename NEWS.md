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
