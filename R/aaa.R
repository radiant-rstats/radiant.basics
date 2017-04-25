# to avoid 'no visible binding for global variable' NOTE
globalVariables(c(".","..count..","matches","n","se","Freq","ci","col1","n","y","parameter"))

#' radiant.basics
#'
#' @name radiant.basics
#' @docType package
#' @import radiant.data shiny ggplot2
#' @importFrom dplyr mutate_all summarise_all funs rename_ bind_cols select filter filter_ group_by_ summarise arrange mutate count
#' @importFrom tidyr gather_
#' @importFrom gridExtra grid.arrange
#' @importFrom scales percent
#' @importFrom magrittr %>% %<>% set_rownames set_colnames set_names divide_by
#' @importFrom methods is
#' @importFrom graphics pairs par points strwidth text
#' @importFrom stats na.omit binom.test chisq.test cor.test cov dbinom dchisq dexp df dnorm dpois dt dunif p.adjust pbinom pchisq pexp pf pnorm ppois prop.test pt punif qbinom qchisq qexp qf qpois qt qunif qnorm rbinom relevel sd setNames symnum t.test wilcox.test
#' @importFrom utils combn
#' @importFrom import from
NULL

#' Newspaper readership
#' @details Newspaper readership data for 580 consumers. Description provided in attr(newspaper,"description")
#' @docType data
#' @keywords datasets
#' @name newspaper
#' @usage data(newspaper)
#' @format A data frame with 580 rows and 2 variables
NULL

#' Car brand consideration
#' @details Survey data of consumer purchase intentions. Description provided in attr(consider,"description")
#' @docType data
#' @keywords datasets
#' @name consider
#' @usage data(consider)
#' @format A data frame with 1000 rows and 2 variables
NULL

#' Demand in the UK
#' @details Survey data of consumer purchase intentions. Description provided in attr(demand_uk,"description")
#' @docType data
#' @keywords datasets
#' @name demand_uk
#' @usage data(demand_uk)
#' @format A data frame with 1000 rows and 2 variables
NULL

#' Salaries for Professors
#' @details 2008-2009 nine-month salary for professors in a college in the US. Description provided in attr(salary,description")
#' @docType data
#' @keywords datasets
#' @name salary
#' @usage data(salary)
#' @format A data frame with 397 rows and 6 variables
NULL




