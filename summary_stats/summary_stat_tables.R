### tables of summary statistics

library(plm)
library(stargazer)

not_mdi_subset <- readRDS("../call_non_mdi_pdata.rds")

## pull MDI and non-MDI subsets-----------------

aa_subset <- readRDS("../call_mdi_black_pdata.rds")

h_subset <- readRDS("../call_mdi_hispanic_pdata.rds")

###

## variables

var <- c("CSBLTOT", "CNUMSBL", "SBLTOT", "SBLTOT_TA", "EQTA0", "ROA0", "NPA0", "LIQTA0", "CORETA0", "BCOMMITTAC0", "RCFD2170", "LNTA0", "BIGBANK", "BADBANK", "DENOVO")

not_mdi_vars <- data.frame(not_mdi_subset[, var])

aa_vars <- data.frame(aa_subset[, var])

h_vars <- data.frame(h_subset[, var])


## create tables with stargazer

stargazer(aa_vars, type = "html", title="Descriptive Statistics: African American MDIs", digits=3, out="aa_summary_statistics.htm", covariate.labels = c("% Change Amt. S. Bus. Loans", "% Change Num. S. Bus. Loans", "Amt. Loans", "Loans/TA", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "Total Assets", "ln(Total Assets)", "Large Bank", "Troubled Bank", "De Novo"))

apply(aa_vars, 2, median)

 ##      CSBLTOT       CNUMSBL        SBLTOT     SBLTOT_TA         EQTA0 
 ## 4.838109e-03 -3.867403e-02  2.298000e+04  2.199227e-01  8.652972e-02 
 ##         ROA0          NPA0        LIQTA0       CORETA0   BCOMMITTAC0 
 ## 3.255115e-03  4.075653e-02  3.058702e-01  4.186915e-01  5.816088e-02 
 ##     RCFD2170         LNTA0       BIGBANK       BADBANK        DENOVO 
 ## 9.099700e+04  1.142229e+01  0.000000e+00  0.000000e+00  0.000000e+00 

stargazer(h_vars, type = "html", title="Descriptive Statistics: Hispanic MDIs", digits=3, out="h_summary_statistics.htm", covariate.labels = c("% Change Amt. S. Bus. Loans", "% Change Num. S. Bus. Loans", "Amt. Loans", "Loans/TA", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments",  "Total Assets", "ln(Total Assets)", "Large Bank", "Troubled Bank", "De Novo"))

apply(h_vars, 2, median)

 ##     CSBLTOT       CNUMSBL        SBLTOT     SBLTOT_TA         EQTA0 
 ## 0.000000e+00 -3.827751e-02  3.435700e+04  1.202521e-01  1.002287e-01 
 ##         ROA0          NPA0        LIQTA0       CORETA0   BCOMMITTAC0 
 ## 6.820809e-03  2.181300e-02  3.147261e-01  3.336667e-01  5.500099e-02 
 ##     RCFD2170         LNTA0       BIGBANK       BADBANK        DENOVO 
 ## 2.269520e+05  1.229209e+01  0.000000e+00  0.000000e+00  0.000000e+00 

stargazer(not_mdi_vars, type = "html", title="Descriptive Statistics: Non-MDIs", digits=3, out="not_mdi_vars_summary_statistics.htm", covariate.labels = c("% Change Amt. S. Bus. Loans", "% Change Num. S. Bus. Loans", "Amt. Loans", "Loans/TA", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments",  "Total Assets", "ln(Total Assets)", "Large Bank", "Troubled Bank", "De Novo"))

apply(not_mdi_vars, 2, median)

 ##      CSBLTOT       CNUMSBL        SBLTOT     SBLTOT_TA         EQTA0 
 ## 2.412613e-02 -7.462687e-03  2.212400e+04  1.494018e-01  1.000197e-01 
 ##         ROA0          NPA0        LIQTA0       CORETA0   BCOMMITTAC0 
 ## 9.408143e-03  1.419297e-02  2.967097e-01  4.645642e-01  6.256911e-02 
 ##     RCFD2170         LNTA0       BIGBANK       BADBANK        DENOVO 
 ## 1.337200e+05  1.173678e+01  0.000000e+00  0.000000e+00  0.000000e+00 


### get NUMSBL summary stats

## non-MDI
median(not_mdi_subset$NUMSBL, na.rm = T)
mean(not_mdi_subset$NUMSBL, na.rm = T)
sd(not_mdi_subset$NUMSBL, na.rm = T)
min(not_mdi_subset$NUMSBL, na.rm = T)
max(not_mdi_subset$NUMSBL, na.rm = T)

## AA
median(aa_subset$NUMSBL, na.rm = T)
mean(aa_subset$NUMSBL, na.rm = T)
sd(aa_subset$NUMSBL, na.rm = T)
min(aa_subset$NUMSBL, na.rm = T)
max(aa_subset$NUMSBL, na.rm = T)

## H
median(h_subset$NUMSBL, na.rm = T)
mean(h_subset$NUMSBL, na.rm = T)
sd(h_subset$NUMSBL, na.rm = T)
min(h_subset$NUMSBL, na.rm = T)
max(h_subset$NUMSBL, na.rm = T)
