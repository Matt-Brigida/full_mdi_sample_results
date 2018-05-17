### tables of summary statistics

library(plm)
library(stargazer)

## pull MDI and non-MDI subsets-----------------

all_mdi_subset <- readRDS("../../call_mdi_pdata.rds")

## total loans RCFD1400


###

## fe3 <- plm(CSBLTOT ~  SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = all_mdi_subset, model = "within", effect = "twoways")
fe3 <- plm(SBLTOT_TA ~  EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = all_mdi_subset, model = "within", effect = "twoways")

## Table 4

## fe4 <- plm(CSBLTOT ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = all_mdi_subset, model = "within", effect = "twoways")
fe4 <- plm(SBLTOT_TA ~  BIGBANK + BIGBKFC + BIGBKPC + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = all_mdi_subset, model = "within", effect = "twoways")


## Table 5

## fe5 <- plm(CSBLTOT ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = all_mdi_subset, model = "within", effect = "twoways")
fe5 <- plm(SBLTOT_TA ~  BADBANK + BADBKFC + BADBKPC + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = all_mdi_subset, model = "within", effect = "twoways")


## Output three models (tables 3, 4, 5) to word via html-----

## stargazer(fe3, fe4, fe5, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo"), dep.var.labels = "Annual % Change in the Amount of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "All MDI: Annual % Change in the Amount of Small-Business Loans Outstanding", out = "all_mdi_tables345.htm", intercept.bottom = TRUE)


## analyze effect of firm size----

## amount of loans

fe.size1 <- plm(SBLTOT_TA ~  EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = all_mdi_subset, model = "within", effect = "twoways")


fe.size2 <- plm(SBLTOT_TA ~  BIGBANK + BIGBKFC + BIGBKPC + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = all_mdi_subset, model = "within", effect = "twoways")

## Table size 3

fe.size3 <- plm(SBLTOT_TA ~  BADBANK + BADBKFC + BADBKPC + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = all_mdi_subset, model = "within", effect = "twoways")


## Output three models to word via html-----

## stargazer(fe.size1, fe.size2, fe.size3, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "ln(Assets)", "Total Equity x ln(Assets)"), dep.var.labels = "Annual % Change in the Amount of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "All MDI: Annual % Change in the Amount of Small-Business Loans Outstanding", out = "all_mdi_tables_size123.htm", intercept.bottom = TRUE)


