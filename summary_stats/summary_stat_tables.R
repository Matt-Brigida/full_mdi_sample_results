### tables of summary statistics

library(plm)
library(stargazer)

not_mdi_subset <- readRDS("../call_non_mdi_pdata.rds")

## pull MDI and non-MDI subsets-----------------

aa_subset <- readRDS("../call_mdi_black_pdata.rds")

h_subset <- readRDS("../call_mdi_hispanic_pdata.rds")

###

## variables

var <- c("CSBLTOT", "CNUMSBL", "SBLTOT_TA", "EQTA0", "ROA0", "NPA0", "LIQTA0", "CORETA0", "BCOMMITTAC0", "LNTA0", "BIGBANK", "BADBANK", "DENOVO")

not_mdi_vars <- data.frame(not_mdi_subset[, var])

aa_vars <- data.frame(aa_subset[, var])

h_vars <- data.frame(h_subset[, var])


## create tables with stargazer

stargazer(aa_vars, type = "html", title="Descriptive Statistics: African American MDIs", digits=3, out="aa_summary_statistics.htm", covariate.labels = c("% Change Amt. S. Bus. Loans", "% Change Num. S. Bus. Loans", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "ln(Total Assets)", "Large Bank", "Troubled Bank", "De Novo"))

apply(aa_vars, 2, median)

 ##     CSBLTOT      CNUMSBL    SBLTOT_TA        EQTA0         ROA0         NPA0 
 ## 0.004838109 -0.038674033  0.219922663  0.086529718  0.003255115  0.040756535 
 ##      LIQTA0      CORETA0  BCOMMITTAC0        LNTA0      BIGBANK      BADBANK 
 ## 0.305870163  0.418691519  0.058160880 11.422289345  0.000000000  0.000000000 
 ##      DENOVO 
 ## 0.000000000 

stargazer(h_vars, type = "html", title="Descriptive Statistics: Hispanic MDIs", digits=3, out="h_summary_statistics.htm", covariate.labels = c("% Change Amt. S. Bus. Loans", "% Change Num. S. Bus. Loans", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "ln(Total Assets)", "Large Bank", "Troubled Bank", "De Novo"))

apply(h_vars, 2, median)

 ##     CSBLTOT      CNUMSBL    SBLTOT_TA        EQTA0         ROA0         NPA0 
 ## 0.000000000 -0.038277512  0.120252062  0.100228673  0.006820809  0.021812998 
 ##      LIQTA0      CORETA0  BCOMMITTAC0        LNTA0      BIGBANK      BADBANK 
 ## 0.314726066  0.333666722  0.055000992 12.292089778  0.000000000  0.000000000 
 ##      DENOVO 
 ## 0.000000000 

stargazer(not_mdi_vars, type = "html", title="Descriptive Statistics: Non-MDIs", digits=3, out="not_mdi_vars_summary_statistics.htm", covariate.labels = c("% Change Amt. S. Bus. Loans", "% Change Num. S. Bus. Loans", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "ln(Total Assets)", "Large Bank", "Troubled Bank", "De Novo"))

apply(not_mdi_vars, 2, median)

 ##     CSBLTOT      CNUMSBL    SBLTOT_TA        EQTA0         ROA0         NPA0 
 ## 0.024126133 -0.007462687  0.149401827  0.100019688  0.009408143  0.014192974 
 ##      LIQTA0      CORETA0  BCOMMITTAC0        LNTA0      BIGBANK      BADBANK 
 ## 0.296709684  0.464564176  0.062569106 11.736780763  0.000000000  0.000000000 
 ##      DENOVO 
 ## 0.000000000 
