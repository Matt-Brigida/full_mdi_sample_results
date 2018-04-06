## to read sas7bdat file --------

library(haven)
library(plm)
library(zoo)

call_all <- read_sas("../../Data/call_all.sas7bdat")

### removing duplicate rows-----
## create one index
theindex <- paste0(call_all$DATE, "_", call_all$RSSD9001)

call_all <- cbind(theindex, call_all)

call_all <- subset(call_all, !duplicated(call_all$theindex))

## remove all data before 20030630 which is the first observation in MDI dataset -----

call_all <- call_all[call_all$DATE >= 20030630, ]

### preparing MDI data -----

mdi <- read.csv("./new_jim_mdi_dataset.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
mdi$CITY <- na.locf(mdi$CITY)
mdi$STATE <- na.locf(mdi$STATE)
mdi$MinorityStatusCode <- na.locf(mdi$MinorityStatusCode)
mdi$DATE <- as.Date(mdi$DATE, format = "%m/%d/%Y")
mdi$DATE <- as.numeric(gsub("-", "", mdi$DATE))

## remove all mdi data after 20140630  which is the last observation in Rebel dataset -----

mdi <- mdi[mdi$DATE <= 20140630, ]

## now create the matching key-----date_idrssd

theindex <- paste0(mdi$DATE, "_", mdi$IDRSSD)

mdi <- cbind(theindex, mdi)

## split into race subsets-----

mdi_black <- subset(mdi, MinorityStatusCode == "B")
mdi_hispanic <- subset(mdi, MinorityStatusCode == "H")
mdi_asian <- subset(mdi, MinorityStatusCode == "A")
mdi_native <- subset(mdi, MinorityStatusCode == "N")
mdi_multi <- subset(mdi, MinorityStatusCode == "M")

### Create subsets--------

call_non_mdi <- subset(call_all, !(theindex %in% mdi$theindex))
call_mdi <- subset(call_all, (theindex %in% mdi$theindex))
call_mdi_black <- subset(call_all, (theindex %in% mdi_black$theindex))
call_mdi_hispanic <- subset(call_all, (theindex %in% mdi_hispanic$theindex))
call_mdi_asian <- subset(call_all, (theindex %in% mdi_asian$theindex))
call_mdi_native <- subset(call_all, (theindex %in% mdi_native$theindex))
call_mdi_multi <- subset(call_all, (theindex %in% mdi_multi$theindex))
call_mdi_bhn <- rbind(call_mdi_black, call_mdi_hispanic, call_mdi_native)


### create MDI indicator on full sample-----

mdi_ind <- 0
bhn_ind <- 0
hispanic_ind <- 0
asian_ind <- 0
black_ind <- 0
native_ind <- 0
multi_ind <- 0

## all mdi
for (i in 1:dim(call_all)[1]){
    mdi_ind[i] <- ifelse(call_all$RSSD9001[i] %in% call_mdi$RSSD9001, 1, 0)
}

call_all_mdi_ind <- cbind(call_all, mdi_ind)

## asian mdi
for (i in 1:dim(call_all)[1]){
    asian_ind[i] <- ifelse(call_all$RSSD9001[i] %in% call_mdi_asian$RSSD9001, 1, 0)
}

call_all_mdi_ind <- cbind(call_all_mdi_ind, asian_ind)

## non asian mdi
for (i in 1:dim(call_all)[1]){
    bhn_ind[i] <- ifelse(call_all$RSSD9001[i] %in% call_mdi_bhn$RSSD9001, 1, 0)
}

call_all_mdi_ind <- cbind(call_all_mdi_ind, bhn_ind)

## black mdi
for (i in 1:dim(call_all)[1]){
    black_ind[i] <- ifelse(call_all$RSSD9001[i] %in% call_mdi_black$RSSD9001, 1, 0)
}

call_all_mdi_ind <- cbind(call_all_mdi_ind, black_ind)

## hispanic mdi
for (i in 1:dim(call_all)[1]){
    hispanic_ind[i] <- ifelse(call_all$RSSD9001[i] %in% call_mdi_hispanic$RSSD9001, 1, 0)
}

call_all_mdi_ind <- cbind(call_all_mdi_ind, hispanic_ind)



### add mdi type by name to all data------------------------------------------------

mdi_type <- ""

for (i in 1:dim(call_all)[1]){
    mdi_type[i] <- if(call_all$RSSD9001[i] %in% call_mdi_asian$RSSD9001){
                       "MDI_Asian"
                   } else {
                       if(call_all$RSSD9001[i] %in% call_mdi_bhn$RSSD9001){
                           "MDI_Non_Asian"
                       } else {
                           "Non-MDI"
                       }
                   }
}

call_all_mdi_type <- cbind(call_all, mdi_type)
saveRDS(call_all_mdi_type, "call_all_mdi_type.rds")




## convert to pdata.frame for panel data calculations------

## call_all <- pdata.frame(call_all, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_non_mdi_p <- pdata.frame(call_non_mdi, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_mdi_p <- pdata.frame(call_mdi, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_mdi_black_p <- pdata.frame(call_mdi_black, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_mdi_hispanic_p <- pdata.frame(call_mdi_hispanic, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_mdi_asian_p <- pdata.frame(call_mdi_asian, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_mdi_native_p <- pdata.frame(call_mdi_native, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_mdi_multi_p <- pdata.frame(call_mdi_multi, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_mdi_bhn_p <- pdata.frame(call_mdi_bhn, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)

## with mdi ind
call_all_mdi_ind_p <- pdata.frame(call_all_mdi_ind, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)
call_all_mdi_type_p <- pdata.frame(call_all_mdi_type, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)


## saveRDS(call_all, "callall_pdata.rds")
saveRDS(call_non_mdi_p, "call_non_mdi_pdata.rds")
saveRDS(call_mdi_p, "call_mdi_pdata.rds")
saveRDS(call_mdi_black_p, "call_mdi_black_pdata.rds")
saveRDS(call_mdi_hispanic_p, "call_mdi_hispanic_pdata.rds")
saveRDS(call_mdi_asian_p, "call_mdi_asian_pdata.rds")
saveRDS(call_mdi_native_p, "call_mdi_native_pdata.rds")
saveRDS(call_mdi_multi_p, "call_mdi_multi_pdata.rds")
saveRDS(call_mdi_bhn_p, "call_mdi_bhn_pdata.rds")
saveRDS(call_all_mdi_ind_p, "call_all_mdi_ind_pdata.rds")
saveRDS(call_all_mdi_type_p, "call_all_mdi_type_pdata.rds")

