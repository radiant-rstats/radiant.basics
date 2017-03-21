# CHANGES IN radiant.basics VERSION 0.7.3

## NEW FEATURES

- Allow sampling data for correlation plot. Default is 1,000 obs
- Show df name in output

## BUG FIXES

- Fixed correlation dropdown. Correlations did not change when method was changed (thanks @Fiordmaster)
- Improved formating for small negative values
- Convert numeric bounds to integer in Probability calculator > Binomial to avoid warnings
