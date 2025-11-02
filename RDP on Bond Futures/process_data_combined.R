# Process Combined Data
# Richard Finlay / NY Fed
# February 2025

# 0. Preliminaries ####

library(tidyr)
library(dplyr)
library(chron)

# 1. Load data ####

load("Intermediates/Full/data_full_5m_3.Rdata")
data3y5m <- data5m
ind_roll3 <- ind_roll

load("Intermediates/Full/data_full_5m_10.Rdata")
data10y5m <- data5m
ind_roll10 <- ind_roll

rm(data5m)
rm(ind_roll)

# 2. Process data ####

## 2.1. Remove impossible bid-asks ####

ind_1bp3 <- {
  data3y5m$date >= "2022-10-18"
} # When 3y min tick went to 1 bps
data3y5m$BidAsk1[!ind_roll3][data3y5m$BidAsk1[!ind_roll3] < 0.005] <- NA
data3y5m$BidAsk1[{
  ind_1bp3 & !ind_roll3
}][data3y5m$BidAsk1[{
  ind_1bp3 & !ind_roll3
}] < 0.01] <- NA
data3y5m$BidAsk1[ind_roll3][data3y5m$BidAsk1[ind_roll3] < 0.002] <- NA
data3y5m$BidAsk2[ind_roll3][data3y5m$BidAsk2[ind_roll3] < 0.002] <- NA
data10y5m$BidAsk1[!ind_roll10][data10y5m$BidAsk1[!ind_roll10] < 0.005] <- NA
data10y5m$BidAsk1[ind_roll10][data10y5m$BidAsk1[ind_roll10] < 0.001] <- NA
data10y5m$BidAsk2[ind_roll10][data10y5m$BidAsk2[ind_roll10] < 0.001] <- NA

## 2.2. Create new dataset combining first and 2nd contract metrics - 3y ####

comb3 <- data3y5m[, c(1, 2, 7, 9, 14, 16, 18, 20, 22, 24, 26)]
comb3 <- rename(comb3, dPrice = "dPrice1", BidAsk = "BidAsk1", bDepth = "bDepth1", aDepth = "aDepth1", tDepth = "tDepth1", bTurnover = "bTurnover1", aTurnover = "aTurnover1", tTurnover = "tTurnover1")
comb3$beta[!ind_roll3] <- data3y5m$beta1[!ind_roll3]
comb3$aTurnover <- rowSums(cbind(data3y5m$aTurnover1, data3y5m$aTurnover2), na.rm = T)
comb3$bTurnover <- rowSums(cbind(data3y5m$bTurnover1, data3y5m$bTurnover2), na.rm = T)
comb3$tTurnover <- rowSums(cbind(data3y5m$tTurnover1, data3y5m$tTurnover2), na.rm = T)
for (i in 1:sum(ind_roll3)) {
  ind_day <- data3y5m$date == data3y5m$date[ind_roll3][i]
  w <- sum(data3y5m$tTurnover1[ind_day], na.rm = T) / sum(comb3$tTurnover[ind_day], na.rm = T)
  comb3$dPrice[ind_roll3][i] <- sum(w * data3y5m$dPrice1[ind_roll3][i], (1 - w) * data3y5m$dPrice2[ind_roll3][i], na.rm = T)
  comb3$BidAsk[ind_roll3][i] <- sum(w * data3y5m$BidAsk1[ind_roll3][i], (1 - w) * data3y5m$BidAsk2[ind_roll3][i], na.rm = T)
  comb3$bDepth[ind_roll3][i] <- sum(w * data3y5m$bDepth1[ind_roll3][i], (1 - w) * data3y5m$bDepth2[ind_roll3][i], na.rm = T)
  comb3$aDepth[ind_roll3][i] <- sum(w * data3y5m$aDepth1[ind_roll3][i], (1 - w) * data3y5m$aDepth2[ind_roll3][i], na.rm = T)
  comb3$tDepth[ind_roll3][i] <- sum(w * data3y5m$tDepth1[ind_roll3][i], (1 - w) * data3y5m$tDepth2[ind_roll3][i], na.rm = T)
}

## 2.3. Create new dataset combining first and 2nd contract metrics - 10y ####

comb10 <- data10y5m[, c(1, 2, 7, 9, 14, 16, 18, 20, 22, 24, 26)]
comb10 <- rename(comb10, dPrice = "dPrice1", BidAsk = "BidAsk1", bDepth = "bDepth1", aDepth = "aDepth1", tDepth = "tDepth1", bTurnover = "bTurnover1", aTurnover = "aTurnover1", tTurnover = "tTurnover1")
comb10$beta[!ind_roll10] <- data10y5m$beta1[!ind_roll10]
comb10$aTurnover <- rowSums(cbind(data10y5m$aTurnover1, data10y5m$aTurnover2), na.rm = T)
comb10$bTurnover <- rowSums(cbind(data10y5m$bTurnover1, data10y5m$bTurnover2), na.rm = T)
comb10$tTurnover <- rowSums(cbind(data10y5m$tTurnover1, data10y5m$tTurnover2), na.rm = T)
for (i in 1:sum(ind_roll10)) {
  ind_day <- data10y5m$date == data10y5m$date[ind_roll10][i]
  w <- sum(data10y5m$tTurnover1[ind_day], na.rm = T) / sum(comb10$tTurnover[ind_day], na.rm = T)
  comb10$dPrice[ind_roll10][i] <- sum(w * data10y5m$dPrice1[ind_roll10][i], (1 - w) * data10y5m$dPrice2[ind_roll10][i], na.rm = T)
  comb10$BidAsk[ind_roll10][i] <- sum(w * data10y5m$BidAsk1[ind_roll10][i], (1 - w) * data10y5m$BidAsk2[ind_roll10][i], na.rm = T)
  comb10$bDepth[ind_roll10][i] <- sum(w * data10y5m$bDepth1[ind_roll10][i], (1 - w) * data10y5m$bDepth2[ind_roll10][i], na.rm = T)
  comb10$aDepth[ind_roll10][i] <- sum(w * data10y5m$aDepth1[ind_roll10][i], (1 - w) * data10y5m$aDepth2[ind_roll10][i], na.rm = T)
  comb10$tDepth[ind_roll10][i] <- sum(w * data10y5m$tDepth1[ind_roll10][i], (1 - w) * data10y5m$tDepth2[ind_roll10][i], na.rm = T)
}

# 3. Produce outputs ####

save(comb3, ind_roll3, ind_1bp3, file = "Intermediates/Combined/data_combined_3.Rdata")
save(comb10, ind_roll10, file = "Intermediates/Combined/data_combined_10.Rdata")

write.csv(comb3[, 1:5], file = "Intermediates/Combined/comb3_1.csv")
write.csv(comb3[, 6:11], file = "Intermediates/Combined/comb3_2.csv")
write.csv(comb10[, 1:5], file = "Intermediates/Combined/comb10_1.csv")
write.csv(comb10[, 6:11], file = "Intermediates/Combined/comb10_2.csv")
write.csv(ind_1bp3, file = "Intermediates/Combined/ind_1bp3.csv")
write.csv(ind_roll3, file = "Intermediates/Combined/ind_roll3.csv")
write.csv(ind_roll10, file = "Intermediates/Combined/ind_roll10.csv")
