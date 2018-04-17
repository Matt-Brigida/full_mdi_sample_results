##
library(ggplot2)
library(ggthemes)

## Amount of Loans

nmA <- data.frame(cbind(c("Non-MDI", "Non-MDI"), c("Low", "High"), c(.206, .348)), stringsAsFactors = FALSE)
aaA <- data.frame(cbind(c("African American", "African American"), c("Low", "High"), c(.319, .538)), stringsAsFactors = FALSE)
hA <- data.frame(cbind(c("Hispanic", "Hispanic"), c("Low", "High"), c(.223, .354)), stringsAsFactors = FALSE)

dataA <- rbind(nmA, hA, aaA)
names(dataA) <- c("MDI Type", "Range", "Change in S.B. Loans ($)")
dataA$`Change in S.B. Loans ($)` <- as.numeric(dataA$`Change in S.B. Loans ($)`)

pdf("amount_default.pdf")
ggplot(dataA, aes(x= `MDI Type`, y=`Change in S.B. Loans ($)`)) + geom_boxplot()
dev.off()

pdf("amount_economist.pdf")
ggplot(dataA, aes(x= `MDI Type`, y=`Change in S.B. Loans ($)`)) + geom_boxplot() + theme_economist() + scale_color_economist()
dev.off()

pdf("amount_solarized_light.pdf")
ggplot(dataA, aes(x= `MDI Type`, y=`Change in S.B. Loans ($)`)) + geom_boxplot(fill = "white") + theme_solarized() + scale_color_solarized()
dev.off()

pdf("amount_tufte1.pdf")
ggplot(dataA, aes(`MDI Type`, `Change in S.B. Loans ($)`)) + theme_tufte(ticks=FALSE) +
  geom_tufteboxplot(median.type = "line", whisker.type = 'line', hoffset = 0, width = 3)
dev.off()

pdf("amount_tufte2.pdf")
ggplot(dataA, aes(`MDI Type`, `Change in S.B. Loans ($)`)) + theme_tufte(ticks=FALSE) + geom_tufteboxplot()
dev.off()


## Number of Loans

nmN <- data.frame(cbind(c("Non-MDI", "Non-MDI"), c("Low", "High"), c(1.83, 3.41)), stringsAsFactors = FALSE)
aaN <- data.frame(cbind(c("African American", "African American"), c("Low", "High"), c(-0.927, 0)), stringsAsFactors = FALSE)
hN <- data.frame(cbind(c("Hispanic", "Hispanic"), c("Low", "High"), c(2.375, 3.895)), stringsAsFactors = FALSE)

dataN <- rbind(nmN, hN, aaN)
names(dataN) <- c("MDI Type", "Range", "Change in Num. of S.B. Loans")
dataN$`Change in Num. of S.B. Loans` <- as.numeric(dataN$`Change in Num. of S.B. Loans`)

pdf("number_default.pdf")
ggplot(dataN, aes(x= `MDI Type`, y=`Change in Num. of S.B. Loans`)) + geom_boxplot()
dev.off()

pdf("number_economist.pdf")
ggplot(dataN, aes(x= `MDI Type`, y=`Change in Num. of S.B. Loans`)) + geom_boxplot() + theme_economist() + scale_color_economist()
dev.off()

pdf("number_solarized_light.pdf")
ggplot(dataN, aes(x= `MDI Type`, y=`Change in Num. of S.B. Loans`)) + geom_boxplot(fill = "white") + theme_solarized() + scale_color_solarized()
dev.off()

pdf("number_tufte1.pdf")
ggplot(dataN, aes(`MDI Type`, `Change in Num. of S.B. Loans`)) + theme_tufte(ticks=FALSE) +
  geom_tufteboxplot(median.type = "line", whisker.type = 'line', hoffset = 0, width = 3)
dev.off()

pdf("number_tufte2.pdf")
ggplot(dataN, aes(`MDI Type`, `Change in Num. of S.B. Loans`)) + theme_tufte(ticks=FALSE) + geom_tufteboxplot()
dev.off()

