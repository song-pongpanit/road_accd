library(tseries)
library(forecast)
library(zoo)

#### EDA

data <- read.csv("C:/Users/song-/Documents/STAT-CHULA/TS-2022-1/accd.csv", header = TRUE)
colnames(data) <- c('date','dead','injury')
class(data)
str(data)
data <- data[,c(1,3)]

index <- tsoutliers(ts(data[,2])) # 326  367  727  736  752  753 1203 2193

data[index$index,] # Not replace

# ------------- no seasonal -------------------

tsdata <- ts(data[,2], start=c(1,1))
plot(tsdata, main="Injury in Thailand", xlab="t", ylab="Zt")

Acf(tsdata, lag.max = 49) # Every 7 lags; freq = 7
Pacf(tsdata)

# ----------- seasonal with freq = 7  -------------------

tsdata2 <- ts(data[,2], start=c(2016,1), frequency= 7)
boxplot(tsdata2~cycle(tsdata2)) # highest on monday
plot(tsdata2, main="Injury in Thailand", xlab="t", ylab="Zt")
dec <- decompose(tsdata2)
plot(dec)

fit1 <- auto.arima(tsdata2, lambda = "auto", method = "ML")
checkresiduals(fit1) # reject in Ljung-box
adf.test(tsdata2) # stationary
ks.test(fit1$residuals, "pnorm") #  not normal in WN

summary(fit1)
plot(forecast(fit1, h = 600, level = 95))

# ---------------- ETS  -------------------

etsmodel.mse <- ets(tsdata2, model="ZZZ",opt.crit = "mse")
etsmodel.mse

checkresiduals(etsmodel.mse) # reject in Ljung-box
adf.test(tsdata2) # stationary
ks.test(etsmodel.mse$residuals, "pnorm") #  not normal in WN

forecast(etsmodel.mse, h =14 , level = 95)
plot(forecast(etsmodel.mse, h =12 , level = 95))

### Compare model

# ---- RMSE ----
sqrt(mean(fit1$residuals^2))
sqrt(mean(etsmodel.mse$residuals^2))


