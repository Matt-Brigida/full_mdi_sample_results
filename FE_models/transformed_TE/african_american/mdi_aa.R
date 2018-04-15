### tables of summary statistics

library(plm)
library(stargazer)

## pull MDI and non-MDI subsets-----------------

mdi_subset <- readRDS("../../../call_mdi_black_pdata.rds")
te_squared <- mdi_subset$EQTA0 * mdi_subset$EQTA0
mdi_subset <- cbind(mdi_subset, te_squared)

###

fe3 <- plm(CSBLTOT ~  SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = mdi_subset, model = "within", effect = "twoways")

## Table 4

fe4 <- plm(CSBLTOT ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = mdi_subset, model = "within", effect = "twoways")


## Table 5

fe5 <- plm(CSBLTOT ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = mdi_subset, model = "within", effect = "twoways")


## Output three models (tables 3, 4, 5) to word via html-----

stargazer(fe3, fe4, fe5, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "Total Equity Squared", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo"), dep.var.labels = "Annual % Change in the Amount of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "African American MDI: Annual % Change in the Amount of Small-Business Loans Outstanding", out = "mdi_tables345.htm", intercept.bottom = TRUE)


#### Number of Small-Business Loans------

fe6 <- plm(CNUMSBL ~  SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = mdi_subset, model = "within", effect = "twoways")

## Table 7

fe7 <- plm(CNUMSBL ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = mdi_subset, model = "within", effect = "twoways")

## Table 8

fe8 <- plm(CNUMSBL ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = mdi_subset, model = "within", effect = "twoways")


## Output three models (tables 6, 7, 8) to word via html-----

stargazer(fe6, fe7, fe8, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo"), dep.var.labels = "Annual % Change in the Number of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "African American MDI: Annual % Change in the Number of Small-Business Loans Outstanding", out = "mdi_tables678.htm", intercept.bottom = TRUE)


## analyze effect of firm size----

## amount of loans

fe.size1 <- plm(CSBLTOT ~  SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = mdi_subset, model = "within", effect = "twoways")


fe.size2 <- plm(CSBLTOT ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = mdi_subset, model = "within", effect = "twoways")

## Table size 3

fe.size3 <- plm(CSBLTOT ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = mdi_subset, model = "within", effect = "twoways")


## Output three models to word via html-----

stargazer(fe.size1, fe.size2, fe.size3, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "ln(Assets)", "Total Equity x ln(Assets)"), dep.var.labels = "Annual % Change in the Amount of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "African American MDI: Annual % Change in the Amount of Small-Business Loans Outstanding", out = "mdi_tables_size123.htm", intercept.bottom = TRUE)


## number of loans

fe.size4 <- plm(CNUMSBL ~  SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = mdi_subset, model = "within", effect = "twoways")


fe.size5 <- plm(CNUMSBL ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = mdi_subset, model = "within", effect = "twoways")

## Table size 3

fe.size6 <- plm(CNUMSBL ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + I(EQTA0^2) + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + EQTA0*LNTA0, data = mdi_subset, model = "within", effect = "twoways")


## Output three models to word via html-----

stargazer(fe.size4, fe.size5, fe.size6, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "ln(Assets)", "Total Equity x ln(Assets)"), dep.var.labels = "Annual % Change in the Number of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "African American MDI: Annual % Change in the Number of Small-Business Loans Outstanding", out = "mdi_tables_size456.htm", intercept.bottom = TRUE)
