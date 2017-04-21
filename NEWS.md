# CHANGES IN radiant.basics

## NEW FEATURES

- Allow sampling data for correlation plot. Default is 1,000 obs
- Show dataset name in output if dataframe passed directly to analysis function
- Scatter plots in Correlation > Plot now based on 1,000 data points by default. Add n = -1 to use all data points 
- As an alternative to using the Estimate button to run a model you can now also use CTRL-enter or CMD-enter
- Use ALT-enter to put code into R > Report
- Documentation added on how to customize plots

## BUG FIXES

- Fixed correlation dropdown. Correlations did not change when method was changed (thanks @Fiordmaster)
- Improved formatting for small negative values
- Convert numeric bounds to integer in Probability calculator > Binomial to avoid warnings
