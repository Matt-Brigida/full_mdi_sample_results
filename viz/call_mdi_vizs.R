## load libraries-----
library(tidyverse)

## read data------
data <- readRDS("../call_all_mdi_type.rds")

dataMdi <- subset(data, mdi_type != "Non-MDI")

ggplot(data, aes(x = EQTA0, y = CSBLTOT)) +
    geom_point(aes(color = mdi_type)) +
    geom_smooth(se = TRUE) +
    labs(
    title = "",
    subtitle = "",
    caption = "Data from FDIC",
    x = "Total Equity/Total Assets",
    y = "% Change Small Business Loans"
  )

ggplot(dataMdi, aes(x = EQTA0, y = CSBLTOT)) +
    geom_point(aes(color = mdi_type)) +
    geom_smooth(se = TRUE) +
    labs(
    title = "",
    subtitle = "",
    caption = "Data from FDIC",
    x = "Total Equity/Total Assets",
    y = "% Change Small Business Loans"
  )



ggplot(data, aes(LNTA0, fill = mdi_type)) +
    geom_histogram() +
        labs(
    title = "Firm Size (in log)",
    subtitle = "The colors represent MDI type.",
    caption = "Data from FDIC",
    x = "LNTA0"
    )

ggplot(dataMdi, aes(LNTA0, fill = mdi_type)) +
    geom_histogram() +
        labs(
    title = "Firm Size (in log)",
    subtitle = "",
    caption = "Data from FDIC",
    x = "LNTA0"
    )


