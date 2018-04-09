### vizualizations of summary statistics

library(plm)
library(ggplot2)

not_mdi_subset <- readRDS("../call_non_mdi_pdata.rds")

## pull MDI and non-MDI subsets-----------------

aa_subset <- readRDS("../call_mdi_black_pdata.rds")

h_subset <- readRDS("../call_mdi_hispanic_pdata.rds")

## tagged set

tagged_set <- readRDS("../call_all_mdi_type.rds")

## variables

var <- c("CSBLTOT", "CNUMSBL", "SBLTOT_TA", "EQTA0", "ROA0", "NPA0", "LIQTA0", "CORETA0", "BCOMMITTAC0", "LNTA0", "BIGBANK", "BADBANK", "DENOVO")

not_mdi_vars <- data.frame(not_mdi_subset[, var])

aa_vars <- data.frame(aa_subset[, var])

h_vars <- data.frame(h_subset[, var])

all_tagged_vars <- data.frame(tagged_set[, c(var, "mdi_type")])

###

not_mdi_vars$BIGBANK <- ifelse(not_mdi_vars$BIGBANK == 1, "Bigbank", "Smallbank")

## create viz with ggplot2

ggplot(data = not_mdi_vars, mapping = aes(y = NPA0, x = BIGBANK)) +
  geom_boxplot()

## looking at correlation-----------
ggplot(not_mdi_vars, aes(EQTA0, CSBLTOT)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()



ggplot(all_tagged_vars, aes(EQTA0, CSBLTOT)) +
    geom_point(aes(color = mdi_type)) +
    geom_smooth(se = TRUE) 

ggplot(all_tagged_vars[all_tagged_vars$mdi_type %in% c("MDI_African_American", "MDI_Hispanic"), ], aes(EQTA0, CSBLTOT)) +
    geom_point(aes(color = mdi_type)) +
    geom_smooth(se = TRUE) 

ggplot(all_tagged_vars[all_tagged_vars$mdi_type %in% c("MDI_African_American", "MDI_Hispanic", "MDI_Asian"), ], aes(EQTA0, CSBLTOT)) +
    geom_point(aes(color = mdi_type)) +
    geom_smooth(se = TRUE) 


## histogram on TE-----
ggplot(all_tagged_vars[all_tagged_vars$mdi_type %in% c("MDI_African_American", "MDI_Hispanic"), ], aes(EQTA0, fill = mdi_type)) +
    geom_histogram()

pdf("emp_dens_equity_aa_h.pdf")
ggplot(all_tagged_vars[all_tagged_vars$mdi_type %in% c("MDI_African_American", "MDI_Hispanic"), ], aes(EQTA0, fill = mdi_type)) +
    geom_density() +
    labs(
    title = "Empirical Densith Plot of Total Equity Divided by Total Assets",
    subtitle = "By MDI Type",
    caption = "Data from Fed/FDIC",
    x = "Total Equity Divided by Total Assets",
    y = "Density"
    )
dev.off()

pdf("emp_dens_equity_aa_h_a.pdf")
ggplot(all_tagged_vars[all_tagged_vars$mdi_type %in% c("MDI_African_American", "MDI_Hispanic", "MDI_Asian"), ], aes(EQTA0, fill = mdi_type)) +
    geom_density() +
        labs(
    title = "Empirical Densith Plot of Total Equity Divided by Total Assets",
    subtitle = "By MDI Type",
    caption = "Data from Fed/FDIC",
    x = "Total Equity Divided by Total Assets",
    y = "Density"
  )
dev.off()




