# Process Dropped Data
# Richard Finlay / NY Fed
# May 2025

# 0. Preliminaries ####

library(tidyr)
library(dplyr)
library(chron)

# 1. First exclude 11.30 ####

load(paste0("Intermediates/Full/data_full_30s_", ags_futures_tenor, ".Rdata"))

events <- (1:21) * 30 - 330 + 41400
for (i in 1:length(events)) {
  data30s[data30s[, 2] == events[i], 3:33] <- NA
}

## 1.1. Calculate the price impact - 1.5h ####

pHours <- 1.5 # Window length (hours)
inc <- 0.5
pWindow <- pHours * 60 / inc
nObs <- length(ind_roll_30)

time <- data30s$time
dPrice1 <- data30s$dPrice1
netBuy1 <- data30s$netBuy1
dPrice2 <- data30s$dPrice2
netBuy2 <- data30s$netBuy2
beta <- rep(0, nObs)
stdError <- rep(0, nObs)
df <- rep(0, nObs)
tValue <- rep(0, nObs)
pValue <- rep(0, nObs)

for (i in 1:nObs) {
  # Only run regression if we have at least 1.5 hours = 180 obs of data
  if (time[i] >= 36000) {
    if (!ind_roll_30[i]) { # Just first contract outside of rolls
      try(
        {
          tmp <- summary(lm(dPrice1[(i - pWindow + 1):i] ~ 1 + netBuy1[(i - pWindow + 1):i]), na.rm = TRUE)
          beta[i] <- tmp$coefficients[2, 1]
          stdError[i] <- tmp$coefficients[2, 2]
          tValue[i] <- tmp$coefficients[2, 3]
          pValue[i] <- tmp$coefficients[2, 4]
          df[i] <- tmp$df[2]
        },
        silent = TRUE
      )
    }
    if (ind_roll_30[i]) { # Pool both contracts in roll periods
      try(
        {
          tmp <- summary(lm(c(dPrice1[(i - pWindow + 1):i], dPrice2[(i - pWindow + 1):i]) ~ 1 + c(netBuy1[(i - pWindow + 1):i], netBuy2[(i - pWindow + 1):i])), na.rm = TRUE)
          beta[i] <- tmp$coefficients[2, 1]
          stdError[i] <- tmp$coefficients[2, 2]
          tValue[i] <- tmp$coefficients[2, 3]
          pValue[i] <- tmp$coefficients[2, 4]
          df[i] <- tmp$df[2]
        },
        silent = TRUE
      )
    }
  }
  print(100 * i / nObs)
}

## 1.2. Turn 30s data into 5m data ####

nDates <- length(unique(data30s$date))
start <- 8.5
end <- 16.5
nTimes <- (end - start) * 60 / inc + 1
subIndex <- seq(11, nTimes, 10)
for (i in 2:nDates) {
  subIndex <- c(subIndex, subIndex[1:((nTimes - 1) / 10)] + nTimes * (i - 1))
}

data <- cbind(time, beta, df, stdError)
data5m <- data[subIndex, ]

save(data5m, file = paste0("Intermediates/Dropped/data_dropped_1130_", ags_futures_tenor, ".Rdata"))
write.csv(data[subIndex, ], file = paste0("Intermediates/Dropped/beta_", ags_futures_tenor, "y_exc_1130.csv"))
write.csv(data30s$date[subIndex], file = paste0("Intermediates/Dropped/", ags_futures_tenor, "y_date.csv"))

# 2. Then exclude 14.30 ####

load(paste0("Intermediates/Full/data_full_30s_", ags_futures_tenor, ".Rdata"))

events <- (1:21) * 30 - 330 + 52200
for (i in 1:length(events)) {
  data30s[data30s[, 2] == events[i], 3:33] <- NA
}

## 2.1. Calculate the price impact - 1.5h ####

pHours <- 1.5 # Window length (hours)
inc <- 0.5
pWindow <- pHours * 60 / inc
nObs <- length(ind_roll_30)

time <- data30s$time
dPrice1 <- data30s$dPrice1
netBuy1 <- data30s$netBuy1
dPrice2 <- data30s$dPrice2
netBuy2 <- data30s$netBuy2
beta <- rep(0, nObs)
stdError <- rep(0, nObs)
df <- rep(0, nObs)
tValue <- rep(0, nObs)
pValue <- rep(0, nObs)

for (i in 1:nObs) {
  # Only run regression if we have at least 1.5 hours = 180 obs of data
  if (time[i] >= 36000) {
    if (!ind_roll_30[i]) { # Just first contract outside of rolls
      try(
        {
          tmp <- summary(lm(dPrice1[(i - pWindow + 1):i] ~ 1 + netBuy1[(i - pWindow + 1):i]), na.rm = TRUE)
          beta[i] <- tmp$coefficients[2, 1]
          stdError[i] <- tmp$coefficients[2, 2]
          tValue[i] <- tmp$coefficients[2, 3]
          pValue[i] <- tmp$coefficients[2, 4]
          df[i] <- tmp$df[2]
        },
        silent = TRUE
      )
    }
    if (ind_roll_30[i]) { # Pool both contracts in roll periods
      try(
        {
          tmp <- summary(lm(c(dPrice1[(i - pWindow + 1):i], dPrice2[(i - pWindow + 1):i]) ~ 1 + c(netBuy1[(i - pWindow + 1):i], netBuy2[(i - pWindow + 1):i])), na.rm = TRUE)
          beta[i] <- tmp$coefficients[2, 1]
          stdError[i] <- tmp$coefficients[2, 2]
          tValue[i] <- tmp$coefficients[2, 3]
          pValue[i] <- tmp$coefficients[2, 4]
          df[i] <- tmp$df[2]
        },
        silent = TRUE
      )
    }
  }
  print(100 * i / nObs)
}

## 2.2. Turn 30s data into 5m data ####

nDates <- length(unique(data30s$date))
start <- 8.5
end <- 16.5
nTimes <- (end - start) * 60 / inc + 1
subIndex <- seq(11, nTimes, 10)
for (i in 2:nDates) {
  subIndex <- c(subIndex, subIndex[1:((nTimes - 1) / 10)] + nTimes * (i - 1))
}

data <- cbind(time, beta, df, stdError)
data5m <- data[subIndex, ]

save(data5m, file = paste0("Intermediates/Dropped/data_dropped_1430_", ags_futures_tenor, ".Rdata"))
write.csv(data[subIndex, ], file = paste0("Intermediates/Dropped/beta_", ags_futures_tenor, "y_exc_1430.csv"))
