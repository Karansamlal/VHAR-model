rm(list=ls())


### Estimation of the multivariate VHAR model introduced by by Bubak et al. (2011)

# Libraries
library(readxl)
library(zoo)

# Reads in data of the Open-to-close returns, and the daily realized variance
Returns <- read_excel("ECTR_CASE_RV_4_assets.xlsx", sheet = "OTC returns")
RV <- read_excel("ECTR_CASE_RV_4_assets.xlsx", sheet = "RV") 

# Transform the Date column, such that we can use this for plotting
Returns <- transform(Returns, Tijd = as.Date(as.character(Tijd), "%Y%m%d"))

# Define model
Size.in.sample <- 1500
RV1 = as.matrix(RV[1:Size.in.sample, 2:5])
RVol_simple_day_t1d <-RV1[-(1:22),] 
RVol_simple_day_t <-RV1[-c(1:21,length(RV1[,1])),]
RVol_simple_week_t <- rollmean(RV1[-c(1:17,length(RV1[,1])),], 5)
RVol_simple_month_t <- rollmean(RV1[-length(RV1[,1]),], 22)

# Estimation
vHAR <-lm(RVol_simple_day_t1d ~ RVol_simple_day_t + RVol_simple_week_t + RVol_simple_month_t)

# Estimated model
summary(vHAR)
