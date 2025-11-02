# Process Full Data
# Richard Finlay / NY Fed
# February 2025

# 0. Preliminaries ####

library(tidyr)
library(dplyr)
library(chron)

# 1. Add 1 second to opening time, so that sum over times using > and <= will get all data ####

ticks$time[ticks$time == 30600] <- ticks$time[ticks$time == 30600] + 1

# 2. Create new tibble for 30 sec aggregated data ####

nDates <- length(unique(ticks$date))
start <- 8.5
end <- 16.5
inc <- 0.5
nTimes <- (end - start) * 60 / inc + 1
nObs <- nDates * nTimes
data30s <- tibble(data.frame(matrix(NA, nDates * nTimes, 13, dimnames = list(1:(nDates * nTimes), c("date", "time", "netBuy1", "netBuy2", "lastPrice1", "lastPrice2", "dPrice1", "dPrice2", "beta", "stdError", "tValue", "pValue", "df")))))
reOrder <- order(unique(ticks$date))
data30s$date <- rep(unique(ticks$date)[reOrder], each = nTimes)
data30s$time <- rep(seq(start * 60 * 60, end * 60 * 60, inc * 60), nDates)

# 3. New re-purpose last column of ticks data to be total trades ####

ticks$trade_other <- 0
ind <- ticks$type == "trade"
ticks$trade_other[ind] <- ticks$size[ind]

# 4. Isolate just dates, and data that we need ####

n1 <- dim(ticks)
dat <- ticks[, c(1, 2)]
ind1 <- ticks$contract_type == "first"
ind2 <- ticks$contract_type == "second"
netBuy <- rowSums(cbind(ticks$trade_ask, -ticks$trade_bid, rep(0, n1[1])), na.rm = TRUE)
lastPrice <- rowSums(cbind(ticks$price_mid, rep(0, n1[1])))

# 5. Remove "R" type trades that occur during roll periods ####

ind_roll <- {
  ticks$date == "2019-12-10" | ticks$date == "2019-12-11" | ticks$date == "2019-12-12" | ticks$date == "2019-12-13" | ticks$date == "2019-12-16" | ticks$date == "2020-03-10" | ticks$date == "2020-03-11" | ticks$date == "2020-03-12" | ticks$date == "2020-03-13" | ticks$date == "2020-03-16" | ticks$date == "2020-06-09" | ticks$date == "2020-06-10" | ticks$date == "2020-06-11" | ticks$date == "2020-06-12" | ticks$date == "2020-06-15" | ticks$date == "2020-09-09" | ticks$date == "2020-09-10" | ticks$date == "2020-09-11" | ticks$date == "2020-09-14" | ticks$date == "2020-09-15" | ticks$date == "2020-12-09" | ticks$date == "2020-12-10" | ticks$date == "2020-12-11" | ticks$date == "2020-12-14" | ticks$date == "2020-12-15" | ticks$date == "2021-03-09" | ticks$date == "2021-03-10" | ticks$date == "2021-03-11" | ticks$date == "2021-03-12" | ticks$date == "2021-03-15" | ticks$date == "2021-06-09" | ticks$date == "2021-06-10" | ticks$date == "2021-06-11" | ticks$date == "2021-06-14" | ticks$date == "2021-06-15" | ticks$date == "2021-09-09" | ticks$date == "2021-09-10" | ticks$date == "2021-09-13" | ticks$date == "2021-09-14" | ticks$date == "2021-09-15" | ticks$date == "2021-12-09" | ticks$date == "2021-12-10" | ticks$date == "2021-12-13" | ticks$date == "2021-12-14" | ticks$date == "2021-12-15" | ticks$date == "2022-03-09" | ticks$date == "2022-03-10" | ticks$date == "2022-03-11" | ticks$date == "2022-03-14" | ticks$date == "2022-03-15" | ticks$date == "2022-06-09" | ticks$date == "2022-06-10" | ticks$date == "2022-06-13" | ticks$date == "2022-06-14" | ticks$date == "2022-06-15" | ticks$date == "2022-09-09" | ticks$date == "2022-09-12" | ticks$date == "2022-09-13" | ticks$date == "2022-09-14" | ticks$date == "2022-09-15" | ticks$date == "2022-12-09" | ticks$date == "2022-12-12" | ticks$date == "2022-12-13" | ticks$date == "2022-12-14" | ticks$date == "2022-12-15" | ticks$date == "2023-03-09" | ticks$date == "2023-03-10" | ticks$date == "2023-03-13" | ticks$date == "2023-03-14" | ticks$date == "2023-03-15" | ticks$date == "2023-06-09" | ticks$date == "2023-06-12" | ticks$date == "2023-06-13" | ticks$date == "2023-06-14" | ticks$date == "2023-06-15" | ticks$date == "2023-09-11" | ticks$date == "2023-09-12" | ticks$date == "2023-09-13" | ticks$date == "2023-09-14" | ticks$date == "2023-09-15" | ticks$date == "2023-12-11" | ticks$date == "2023-12-12" | ticks$date == "2023-12-13" | ticks$date == "2023-12-14" | ticks$date == "2023-12-15" | ticks$date == "2024-03-11" | ticks$date == "2024-03-12" | ticks$date == "2024-03-13" | ticks$date == "2024-03-14" | ticks$date == "2024-03-15" | ticks$date == "2024-06-11" | ticks$date == "2024-06-12" | ticks$date == "2024-06-13" | ticks$date == "2024-06-14" | ticks$date == "2024-06-17" | ticks$date == "2024-09-10" | ticks$date == "2024-09-11" | ticks$date == "2024-09-12" | ticks$date == "2024-09-13" | ticks$date == "2024-09-16" | ticks$date == "2024-12-10" | ticks$date == "2024-12-11" | ticks$date == "2024-12-12" | ticks$date == "2024-12-13" | ticks$date == "2024-12-16" | ticks$date == "2025-03-11" | ticks$date == "2025-03-12" | ticks$date == "2025-03-13" | ticks$date == "2025-03-14" | ticks$date == "2025-03-17" | ticks$date == "2025-06-10" | ticks$date == "2025-06-11" | ticks$date == "2025-06-12" | ticks$date == "2025-06-13" | ticks$date == "2025-06-16"
}
ind_R <- ticks$code == "R"
ind_R[is.na(ind_R)] <- FALSE
netBuy[{
  ind_roll & ind_R
}] <- 0
ticks$trade_other[{
  ind_roll & ind_R
}] <- 0
ticks$trade_bid[{
  ind_roll & ind_R
}] <- 0
ticks$trade_ask[{
  ind_roll & ind_R
}] <- 0

# 6. Roll indicator for 30s data ####

ind_roll_30 <- {
  data30s$date == "2019-12-10" | data30s$date == "2019-12-11" | data30s$date == "2019-12-12" | data30s$date == "2019-12-13" | data30s$date == "2019-12-16" | data30s$date == "2020-03-10" | data30s$date == "2020-03-11" | data30s$date == "2020-03-12" | data30s$date == "2020-03-13" | data30s$date == "2020-03-16" | data30s$date == "2020-06-09" | data30s$date == "2020-06-10" | data30s$date == "2020-06-11" | data30s$date == "2020-06-12" | data30s$date == "2020-06-15" | data30s$date == "2020-09-09" | data30s$date == "2020-09-10" | data30s$date == "2020-09-11" | data30s$date == "2020-09-14" | data30s$date == "2020-09-15" | data30s$date == "2020-12-09" | data30s$date == "2020-12-10" | data30s$date == "2020-12-11" | data30s$date == "2020-12-14" | data30s$date == "2020-12-15" | data30s$date == "2021-03-09" | data30s$date == "2021-03-10" | data30s$date == "2021-03-11" | data30s$date == "2021-03-12" | data30s$date == "2021-03-15" | data30s$date == "2021-06-09" | data30s$date == "2021-06-10" | data30s$date == "2021-06-11" | data30s$date == "2021-06-14" | data30s$date == "2021-06-15" | data30s$date == "2021-09-09" | data30s$date == "2021-09-10" | data30s$date == "2021-09-13" | data30s$date == "2021-09-14" | data30s$date == "2021-09-15" | data30s$date == "2021-12-09" | data30s$date == "2021-12-10" | data30s$date == "2021-12-13" | data30s$date == "2021-12-14" | data30s$date == "2021-12-15" | data30s$date == "2022-03-09" | data30s$date == "2022-03-10" | data30s$date == "2022-03-11" | data30s$date == "2022-03-14" | data30s$date == "2022-03-15" | data30s$date == "2022-06-09" | data30s$date == "2022-06-10" | data30s$date == "2022-06-13" | data30s$date == "2022-06-14" | data30s$date == "2022-06-15" | data30s$date == "2022-09-09" | data30s$date == "2022-09-12" | data30s$date == "2022-09-13" | data30s$date == "2022-09-14" | data30s$date == "2022-09-15" | data30s$date == "2022-12-09" | data30s$date == "2022-12-12" | data30s$date == "2022-12-13" | data30s$date == "2022-12-14" | data30s$date == "2022-12-15" | data30s$date == "2023-03-09" | data30s$date == "2023-03-10" | data30s$date == "2023-03-13" | data30s$date == "2023-03-14" | data30s$date == "2023-03-15" | data30s$date == "2023-06-09" | data30s$date == "2023-06-12" | data30s$date == "2023-06-13" | data30s$date == "2023-06-14" | data30s$date == "2023-06-15" | data30s$date == "2023-09-11" | data30s$date == "2023-09-12" | data30s$date == "2023-09-13" | data30s$date == "2023-09-14" | data30s$date == "2023-09-15" | data30s$date == "2023-12-11" | data30s$date == "2023-12-12" | data30s$date == "2023-12-13" | data30s$date == "2023-12-14" | data30s$date == "2023-12-15" | data30s$date == "2024-03-11" | data30s$date == "2024-03-12" | data30s$date == "2024-03-13" | data30s$date == "2024-03-14" | data30s$date == "2024-03-15" | data30s$date == "2024-06-11" | data30s$date == "2024-06-12" | data30s$date == "2024-06-13" | data30s$date == "2024-06-14" | data30s$date == "2024-06-17" | data30s$date == "2024-09-10" | data30s$date == "2024-09-11" | data30s$date == "2024-09-12" | data30s$date == "2024-09-13" | data30s$date == "2024-09-16" | data30s$date == "2024-12-10" | data30s$date == "2024-12-11" | data30s$date == "2024-12-12" | data30s$date == "2024-12-13" | data30s$date == "2024-12-16" | data30s$date == "2025-03-11" | data30s$date == "2025-03-12" | data30s$date == "2025-03-13" | data30s$date == "2025-03-14" | data30s$date == "2025-03-17" | data30s$date == "2025-06-10" | data30s$date == "2025-06-11" | data30s$date == "2025-06-12" | data30s$date == "2025-06-13" | data30s$date == "2025-06-16"
}

# 7. Separate to make faster - first filter on date, then on time ####

allDates <- unique(ticks$date)[reOrder]
for (i in 1:nDates) {
  netBuy_date <- netBuy[dat$date == allDates[i]]
  lastPrice_date <- lastPrice[dat$date == allDates[i]]
  dat_date <- dat[dat$date == allDates[i], ]
  ind_roll_date <- ind_roll[dat$date == allDates[i]]
  ind1_date <- ind1[dat$date == allDates[i]]
  ind2_date <- ind2[dat$date == allDates[i]]
  for (j in 2:nTimes) {
    ind <- dat_date$time > data30s$time[((i - 1) * nTimes + j - 1)] & dat_date$time <= data30s$time[((i - 1) * nTimes + j)]
    data30s$netBuy1[((i - 1) * nTimes + j)] <- sum(netBuy_date[{
      ind & ind1_date
    }])
    try(data30s$lastPrice1[((i - 1) * nTimes + j)] <- tail(lastPrice_date[{
      ind & ind1_date
    }], 1), silent = TRUE)
    if (sum(ind_roll_date) > 0) {
      data30s$netBuy2[((i - 1) * nTimes + j)] <- sum(netBuy_date[{
        ind & ind2_date
      }])
      try(data30s$lastPrice2[((i - 1) * nTimes + j)] <- tail(lastPrice_date[{
        ind & ind2_date
      }], 1), silent = TRUE)
    }
  }
  print(100 * i / nDates)
}

# 8. Fill in lastPrice by day ####

for (i in 1:length(allDates)) {
  data30s[data30s$date == allDates[i], ] <- data30s[data30s$date == allDates[i], ] %>% fill(lastPrice1)
  data30s[data30s$date == allDates[i], ] <- data30s[data30s$date == allDates[i], ] %>% fill(lastPrice2)
}
data30s$dPrice1[2:nObs] <- diff(data30s$lastPrice1)
data30s$dPrice2[2:nObs] <- diff(data30s$lastPrice2)

# 9. Turn 30s data into 5m data ####

subIndex <- seq(11, nTimes, 10)
for (i in 2:nDates) {
  subIndex <- c(subIndex, subIndex[1:((nTimes - 1) / 10)] + nTimes * (i - 1))
}

# 10. Add columns to data30s for other data we want ####

data30s <- tibble(data.frame(data30s, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
data30s <- rename(data30s, BidAsk1 = "NA.", BidAsk2 = "NA..1", bDepth1 = "NA..2", bDepth2 = "NA..3", aDepth1 = "NA..4", aDepth2 = "NA..5", tDepth1 = "NA..6", tDepth2 = "NA..7", bTurnover1 = "NA..8", bTurnover2 = "NA..9", aTurnover1 = "NA..10", aTurnover2 = "NA..11", tTurnover1 = "NA..12", tTurnover2 = "NA..13", beta1 = "NA..14", beta2 = "NA..15", stdError1 = "NA..16", stdError2 = "NA..17", df1 = "NA..18", df2 = "NA..19")

# 11. Calculate the price impact - 1.5h ####

pHours <- 1.5 # Window length (hours)
pWindow <- pHours * 60 / inc
for (i in subIndex) {
  # Only run regression if we have at least 1.5 hours = 180 obs of data
  if (data30s$time[i] >= 36000) {
    # Always do first contract
    try(
      {
        tmp <- summary(lm(data30s$dPrice1[(i - pWindow + 1):i] ~ 1 + data30s$netBuy1[(i - pWindow + 1):i]), na.rm = TRUE)
        data30s$beta1[i] <- tmp$coefficients[2, 1]
        data30s$stdError1[i] <- tmp$coefficients[2, 2]
        data30s$df1[i] <- tmp$df[2]
      },
      silent = TRUE
    )
    # Do 2nd contract in roll period
    if (ind_roll_30[i]) {
      try(
        {
          tmp <- summary(lm(data30s$dPrice2[(i - pWindow + 1):i] ~ 1 + data30s$netBuy2[(i - pWindow + 1):i]), na.rm = TRUE)
          data30s$beta2[i] <- tmp$coefficients[2, 1]
          data30s$stdError2[i] <- tmp$coefficients[2, 2]
          data30s$df2[i] <- tmp$df[2]
        },
        silent = TRUE
      )
      # Pool the two together and do in roll period
      try(
        {
          tmp <- summary(lm(c(data30s$dPrice1[(i - pWindow + 1):i], data30s$dPrice2[(i - pWindow + 1):i]) ~ 1 + c(data30s$netBuy1[(i - pWindow + 1):i], data30s$netBuy2[(i - pWindow + 1):i])), na.rm = TRUE)
          data30s$beta[i] <- tmp$coefficients[2, 1]
          data30s$stdError[i] <- tmp$coefficients[2, 2]
          data30s$tValue[i] <- tmp$coefficients[2, 3]
          data30s$pValue[i] <- tmp$coefficients[2, 4]
          data30s$df[i] <- tmp$df[2]
        },
        silent = TRUE
      )
    }
  }
  print(100 * i / nObs)
}

# 12. Create data to use, and reformat so they are just numbers ####

BidAsk <- rowSums(cbind(ticks$bid_offer, rep(0, n1[1])))
bDepth <- rowSums(cbind(ticks$bid_size, rep(0, n1[1])))
aDepth <- rowSums(cbind(ticks$ask_size, rep(0, n1[1])))
tTurnover <- rowSums(cbind(ticks$trade_other, rep(0, n1[1])), na.rm = TRUE)
bTurnover <- rowSums(cbind(ticks$trade_bid, rep(0, n1[1])), na.rm = TRUE)
aTurnover <- rowSums(cbind(ticks$trade_ask, rep(0, n1[1])), na.rm = TRUE)
time1 <- ticks$time
date1 <- ticks$date
# Counter to tell when subIndex comes to a new day
newDay <- (1:nDates - 1) * 96 + 1

# 13. Calculate the price impact - 1.5h and other series of interest ####

pHours <- 1.5 # Window length (hours)
pWindow <- pHours * 60 / inc
for (i in subIndex) {
  if (sum(i == subIndex[newDay]) == 1) { # If it's a new day, refresh storage vectors
    ind_date <- date1 == data30s$date[i]
    ind_roll_date <- ind_roll[ind_date]
    ind1_date <- ind1[ind_date]
    ind2_date <- ind2[ind_date]
    time1_date <- time1[ind_date]
    BidAsk_date <- BidAsk[ind_date]
    bDepth_date <- bDepth[ind_date]
    aDepth_date <- aDepth[ind_date]
    tTurnover_date <- tTurnover[ind_date]
    bTurnover_date <- bTurnover[ind_date]
    aTurnover_date <- aTurnover[ind_date]
    print(100 * i / nObs)
  }
  ind <- time1_date > data30s$time[i - 10] & time1_date <= data30s$time[i]
  data30s$BidAsk1[i] <- mean(BidAsk_date[{
    ind & ind1_date
  }], na.rm = TRUE)
  data30s$bDepth1[i] <- mean(bDepth_date[{
    ind & ind1_date
  }], na.rm = TRUE)
  data30s$aDepth1[i] <- mean(aDepth_date[{
    ind & ind1_date
  }], na.rm = TRUE)
  data30s$tTurnover1[i] <- sum(tTurnover_date[{
    ind & ind1_date
  }])
  data30s$bTurnover1[i] <- sum(bTurnover_date[{
    ind & ind1_date
  }])
  data30s$aTurnover1[i] <- sum(aTurnover_date[{
    ind & ind1_date
  }])
  if (sum(ind_roll_date) > 0) {
    data30s$BidAsk2[i] <- mean(BidAsk_date[{
      ind & ind2_date
    }], na.rm = TRUE)
    data30s$bDepth2[i] <- mean(bDepth_date[{
      ind & ind2_date
    }], na.rm = TRUE)
    data30s$aDepth2[i] <- mean(aDepth_date[{
      ind & ind2_date
    }], na.rm = TRUE)
    data30s$tTurnover2[i] <- sum(tTurnover_date[{
      ind & ind2_date
    }])
    data30s$bTurnover2[i] <- sum(bTurnover_date[{
      ind & ind2_date
    }])
    data30s$aTurnover2[i] <- sum(aTurnover_date[{
      ind & ind2_date
    }])
  }
}

# 14. Fill in by day ####

for (i in 1:length(allDates)) {
  data30s[data30s$date == allDates[i], ] <- data30s[data30s$date == allDates[i], ] %>% fill(BidAsk1)
  data30s[data30s$date == allDates[i], ] <- data30s[data30s$date == allDates[i], ] %>% fill(aDepth1)
  data30s[data30s$date == allDates[i], ] <- data30s[data30s$date == allDates[i], ] %>% fill(bDepth1)
  data30s[data30s$date == allDates[i], ] <- data30s[data30s$date == allDates[i], ] %>% fill(BidAsk2)
  data30s[data30s$date == allDates[i], ] <- data30s[data30s$date == allDates[i], ] %>% fill(aDepth2)
  data30s[data30s$date == allDates[i], ] <- data30s[data30s$date == allDates[i], ] %>% fill(bDepth2)
}
data30s$tDepth1 <- data30s$bDepth1 + data30s$aDepth1
data30s$tDepth2 <- data30s$bDepth2 + data30s$aDepth2

# 15. Produce outputs ####

save(data30s, ind_roll_30, file = paste0("Intermediates/Full/data_full_30s_", ags_futures_tenor, ".Rdata"))

data5m <- data30s[subIndex, ]
ind_roll <- ind_roll_30[subIndex]
save(data5m, ind_roll, file = paste0("Intermediates/Full/data_full_5m_", ags_futures_tenor, ".Rdata"))

write.csv(data5m[, 1:13], file = paste0("Intermediates/Full/data", ags_futures_tenor, "y5m_1.csv"))
write.csv(data5m[, 14:18], file = paste0("Intermediates/Full/data", ags_futures_tenor, "y5m_2.csv"))
write.csv(data5m[, 19:24], file = paste0("Intermediates/Full/data", ags_futures_tenor, "y5m_3.csv"))
write.csv(data5m[, 25:33], file = paste0("Intermediates/Full/data", ags_futures_tenor, "y5m_4.csv"))
