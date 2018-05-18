### analysis------

library(plm)
library(stargazer)

call_all <- readRDS("../../../call_all_mdi_ind_pdata.rds")

## "Total loans gross": RCFD1400


#### Amount of Small-Business Loans

## using variables from Table 1, this model should be one from *Table 6*-----

## The only questionable variable is the Loans (SBLTOT_TA), but doesnt affect other coefficients and signs

## fe3 <- plm(CSBLTOT ~  mdi_ind*EQTA0 + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")

fe3 <- plm(SBLTOT_TA ~  mdi_ind*EQTA0 + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")

## Table 4

fe4 <- plm(SBLTOT_TA ~  BIGBANK + BIGBKFC + BIGBKPC + mdi_ind*EQTA0 + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")

## Table 5

fe5 <- plm(SBLTOT_TA ~  BADBANK + BADBKFC + BADBKPC + mdi_ind*EQTA0 + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")


## Output three models (tables 3, 4, 5) to word via html-----

## stargazer(fe3, fe4, fe5, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "Total Equity x MDI"), dep.var.labels = "Annual % Change in the Amount of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "All MDIs: Annual % Change in the Amount of Small-Business Loans Outstanding", out = "tables345.htm", intercept.bottom = TRUE, notes = "Results are from fixed-effects models with both time and bank fixed effects, for the years 1995 through 2015.  The dependent variable is the annual percent change in the amount of small-business loans outstanding. Business loans are defined as the sum of commercial, industrial, and commercial real-estate loans.  ")

## analyze effect of firm size----

## amount of loans

## fe.size1 <- plm(SBLTOT_TA ~  EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")
fe.size1 <- plm(SBLTOT_TA ~  EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + I(mdi_ind*EQTA0) + LNTA0, data = call_all, model = "within", effect = "twoways")


fe.size2 <- plm(CSBLTOT ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")

## Table size 3

fe.size3 <- plm(CSBLTOT ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")


## Output three models to word via html-----

stargazer(fe.size1, fe.size2, fe.size3, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "ln(Assets)", "Total Equity x MDI", "Total Equity x ln(Assets)"), dep.var.labels = "Annual % Change in the Amount of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "All MDIs: Annual % Change in the Amount of Small-Business Loans Outstanding", out = "tables_size123.htm", intercept.bottom = TRUE, notes = "Results are from fixed-effects models with both time and bank fixed effects, for the years 1995 through 2015.  The dependent variable is the annual percent change in the amount of small-business loans outstanding. Business loans are defined as the sum of commercial, industrial, and commercial real-estate loans.  ")


## number of loans

fe.size4 <- plm(CNUMSBL ~  SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")


fe.size5 <- plm(CNUMSBL ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")

## Table size 3

fe.size6 <- plm(CNUMSBL ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")


## Output three models to word via html-----

stargazer(fe.size4, fe.size5, fe.size6, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "ln(Assets)", "Total Equity x MDI", "Total Equity x ln(Assets)"), dep.var.labels = "Annual % Change in the Number of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "All MDIs: Annual % Change in the Number of Small-Business Loans Outstanding", out = "tables_size456.htm", intercept.bottom = TRUE, notes = "Results are from fixed-effects models with both time and bank fixed effects, for the years 1995 through 2015.  The dependent variable is the annual percent change in the number of small-business loans outstanding. Business loans are defined as the sum of commercial, industrial, and commercial real-estate loans.  ")
