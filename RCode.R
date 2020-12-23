#libraries####
library(date)
library(aTSA)
library(forecast)

#graphic parameter settings####
par(cex.main = 1, family = "sans", 
    mar = c(4,4,2,2), ps = 8, 
    las = 1, tcl = -0.3, xpd = F, bty = "o")

#functions####
collapse_by_hour <- function(data,percentile){
  start <- 1
  counter <- 1
  index <- 1
  hour <- as.factor(floor((data$timestamp)%%(3600*24)/3600))
  column <- length(data)
  len <- length(data$min)
  prices <- rep(-1,len/10)
  collapse_variable <- rep(-1,len/10)
  startblocks <- rep(-1,len/10)
  date <- rep(-1, len/10)
  while(start < len){
    if(counter < len){
      while(hour[start]==hour[counter]){
        if(counter == len){
          break
        }
        counter = counter + 1
      }
    }
    prices[index] <- quantile(data$lap[start:(counter-1)],percentile)/10^9
    startblocks[index] <- data$blocknumber[start]
    date[index] <- data$date_time[start]
    start <- counter
    index = index + 1
  }
  pos <- min(which(prices == -1))-1
  prices <- prices[0:pos]
  startblocks <- startblocks[0:pos]
  date <- date[0:pos]
  collapsed_data <- cbind(startblocks,date,prices)
  colnames(collapsed_data) <- c("blocknumber","date","price")
  return(data.frame(collapsed_data))
}
far <- function(dataset, p.order, p.seasonal, h){
  forecast(Arima(dataset, order = p.order, seasonal = p.seasonal, lambda = "auto"), h=h)
}
P.Error <- function(predicted, observed){
  return((observed-predicted)/observed)
}
CVts_res <- function(dataset, forecast_function, forecast_horizon, 
                     error_function, initial, p.order, p.seasonal){
  day_time = sample(0:23, size = 1)/24
  end = sample(c((initial+day_time):(round(length(dataset)/24)-1+day_time)), size = 1)
  print(end)
  predicted <- forecast_function(window(dataset, end = end), h = forecast_horizon, 
                                 p.order = p.order, p.seasonal = p.seasonal)$mean
  observed <-  window(dataset, start = end + 1/24, end = end + forecast_horizon/24)
  return(error_function(predicted, observed))
}
CVts_rep <- function(dataset, forecast_function, forecast_horizon,
                     error_function, p.order, p.seasonal,
                     n, initial, seed){
  set.seed(seed)
  return(replicate(n, CVts_res(dataset = dataset, forecast_function = forecast_function, 
                               forecast_horizon = forecast_horizon, 
                               error_function = error_function,
                               initial = initial,
                               p.order = p.order, p.seasonal = p.seasonal)))
}
CVts_res_rol <- function(dataset, forecast_function, forecast_horizon, 
                         error_function, initial, p.order, p.seasonal){
  day_time = sample(0:23, size = 1)/24
  end = sample(c((initial+day_time):(round(length(dataset)/24)-1+day_time)), size = 1)
  print(end)
  predicted <- forecast_function(window(dataset, start = end - 30, end = end), h = forecast_horizon, 
                                 p.order = p.order, p.seasonal = p.seasonal)$mean
  observed <-  window(dataset, start = end + 1/24, end = end + forecast_horizon/24)
  return(error_function(predicted, observed))
}
CVts_rep_rol <- function(dataset, forecast_function, forecast_horizon,
                         error_function, p.order, p.seasonal,
                         n, initial, seed){
  set.seed(seed)
  return(replicate(n, CVts_res_rol(dataset = dataset, forecast_function = forecast_function, 
                                   forecast_horizon = forecast_horizon, 
                                   error_function = error_function,
                                   initial = initial,
                                   p.order = p.order, p.seasonal = p.seasonal)))
}

#load and extract data####
s_path <- "xxx"
setwd(s_path)
filename <- "statistics_5000000-8799999.txt"
tx_data <- read.csv(paste(s_path,filename, sep = "/"), sep = "\t", header = T)
tx_data$date_time <- as.POSIXct(tx_data$timestamp, origin="1970-01-01")
tx_data$date <- as.Date(tx_data$date_time)
tx_data$hour <- as.factor(floor((tx_data$timestamp)%%(3600*24)/3600))
tx_data$utilr <- round(tx_data$gas_used/tx_data$gas_limit,2)
tx_data$blocktime <- c(15,tx_data$timestamp[-1]-tx_data$timestamp[-length(tx_data$timestamp)])
tx_data$daytime <- rep("night",length(tx_data$blocktime))
tx_data$daytime[which(as.numeric(tx_data$hour)>= 7 & as.numeric(tx_data$hour)<= 18)] <- "day"
tx_data$daytime <- factor(tx_data$daytime,levels= c("day","night"))
tx_data$lap <- tx_data$min
tx_data$lap[which(tx_data$min <= 10^9)] <- tx_data$Q1[which(tx_data$min <= 10^9)]
gp_hourly <- collapse_by_hour(tx_data,0.5)
ts_gp <- ts(gp_hourly$price, frequency = 24)


#calculate mean, standard deviation and median of the threshold gas price####
mean(tx_data$lap)/10^9
sd(tx_data$lap/10^9)
median(tx_data$lap)/10^9



#plot 1####
n <- 1000
tx_strategies <- data.frame(c(751/n, 86/n, 53/n, 110/n), 
                            row.names = c("Default strategies\n  without\n prioritization", 
                                          "Default strategies\n with prioritization\n < 0.25",
                                          "Default strategies\n with prioritization\n >= 0.25",
                                          "Other strategies\n\n"))
colnames(tx_strategies) <- "strat"
barplot(tx_strategies$strat, names.arg = rownames(tx_strategies), ylim = c(0,0.8))
axis(side = 2, at = seq(0,1, by = 0.1))
title(ylab = "Empirical relative frequency", line = 2.5)
box()

#plot 2####
lo_smooth <- loess(price~blocknumber, data = gp_hourly, span = 0.05)
date <- as.POSIXct(gp_hourly$date, origin="1970-01-01")
plot(date, gp_hourly$price, xaxt = "n", 
     ylim = c(0,80), pch = 20, 
     xlab = "", ylab = "")
lines(date, predict(lo_smooth), col = "azure4", lwd = 2)
r <- as.POSIXct(round(range(date), "month"))
axis(side = 1, at = seq(r[1], r[2], by = "month"), labels = F)
ticks <- seq(r[1], r[2], by = "quarter")
mtext(strftime(ticks, format = "%Y-%m"), 
      side = 1, line = 0.5, at = ticks)
title(ylab = "Hourly median threshold gas price (Gwei)", xlab = "Date", line = 2)

#fraction of integer gas prices####
length(which(tx_data$min/10^9 == floor(tx_data$min/10^9)))/length(tx_data$min)
length(which(tx_data$Q2/10^9 == floor(tx_data$Q2/10^9)))/length(tx_data$Q2)

#plot 3####
n <- 10^9
foo <- hist(ifelse(tx_data$lap/n> 50,52,tx_data$lap/n), freq = F, 
            xaxt = "n", breaks = c(0:53),
            main = NULL, xlab = "", ylab = "", 
            right = F)
axis(side = 1, at = foo$mids, labels = F)
ticks <- foo$mids[c(T,F,F,F)]
mtext(paste(c(seq(0,50, by = 4), ">50")), 
      side = 1, line = 0.5, at = ticks)
title(ylab = "Relative frequency", line = 2.5)
title(xlab = "Threshold gas price", line = 2)
box(lwd = 0.5)
#plot 4####
n <- 10^9
boxplot(tx_data$lap/n~tx_data$hour, outline = F, main = NULL, xaxt ="n", ylim = c(0,25), xlab = "", ylab = "")
axis(1, at=c(1:24), labels=c(1:24), mgp = c(3,0.3,0))
title(ylab = "Treshold gas price", line = 2.5)
title(xlab = "Coordinated universal time", line = 1.5)
#plot 5####
boxplot(tx_data$utilr~tx_data$hour, outline = F, main = NULL, xaxt ="n", ylim = c(0,1), xlab = "", ylab = "")
axis(1, at=c(1:24), labels=c(1:24), mgp = c(3,0.3,0))
title(ylab = "Utilization rate", line = 2.5)
title(xlab = "Coordinated universal time", line = 1.5)

#Table 1####
fit <- Arima(ts_gp,order = c(2,0,1), seasonal = c(0,1,1), lambda = "auto")
summary(fit)

#plot 6####
res_mape1 <- CVts_rep(dataset = ts_gp, forecast_function = far, forecast_horizon = 24,
                      error_function = P.Error, p.order = c(2,0,1), 
                      p.seasonal = c(0,1,1), n = 500, initial = 60, seed = 1)
res_mape2 <- CVts_rep(dataset = ts_gp, forecast_function = far, forecast_horizon = 24,
                      error_function = P.Error, p.order = c(0,1,0), 
                      p.seasonal = c(0,0,0), n = 500, initial = 60, seed = 1)
res_mape3 <- CVts_rep(dataset = ts_gp, forecast_function = far, forecast_horizon = 24,
                      error_function = P.Error, p.order = c(0,0,0), 
                      p.seasonal = c(0,1,0), n = 500, initial = 60, seed = 1)

plot(c(1:24),rowMeans(abs(res_mape1)), type = "l", ylim = c(0.1,0.7), 
     xlab = "", ylab = "", xaxt = "n")
points(c(1:24), y = rowMeans(abs(res_mape2)), type = "l", lty = 2)
points(c(1:24), y = rowMeans(abs(res_mape3)), type = "l", lty = 3)
axis(1, mgp = c(3,0.5,0))
legend("bottomright", lty = c(1:3), 
       legend = c("(2,0,1)(0,1,1)[24]","(0,1,0)(0,0,0)[24]","(0,0,0)(0,1,0)[24]"))
title(ylab = "Mean absolute percentage error (MAPE)", line = 2.5)
title(xlab = "Number of hours forecast ahead of time",line = 1.5)
#plot 7####
gp_hourly25 <- collapse_by_hour(tx_data,0.25)
gp_hourly75 <- collapse_by_hour(tx_data,0.75)
ts_gp25 <- ts(gp_hourly25$price, frequency = 24)
ts_gp75 <- ts(gp_hourly75$price, frequency = 24)
res_mape1 <- CVts_rep(dataset = ts_gp, forecast_function = far, forecast_horizon = 24,
                      error_function = P.Error, p.order = c(2,0,1), 
                      p.seasonal = c(0,1,1), n = 500, initial = 60, seed = 1)
res_mape25 <- CVts_rep(dataset = ts_gp25, forecast_function = far, forecast_horizon = 24,
                       error_function = P.Error, p.order = c(2,0,1), 
                       p.seasonal = c(0,1,1), n = 500, initial = 60, seed = 1)
res_mape75 <- CVts_rep(dataset = ts_gp75, forecast_function = far, forecast_horizon = 24,
                       error_function = P.Error, p.order = c(2,0,1), 
                       p.seasonal = c(0,1,1), n = 500, initial = 60, seed = 1)
plot(c(1:24),rowMeans(abs(res_mape1)), type = "l", ylim = c(0.1,0.5), 
     xlab = "", ylab = "", xaxt = "n")
axis(1, mgp = c(3,0.5,0))
points(c(1:24), y = rowMeans(abs(res_mape25)), type = "l", lty = 2)
points(c(1:24), y = rowMeans(abs(res_mape75)), type = "l", lty = 3)
temp <- legend("bottomright", lty = c(1:3), 
       legend = c("","",""), xjust = 1, yjust = 1, text.width = 12)
text(temp$rect$left + temp$rect$w, temp$text$y,
     c("median hourly threshold gas price",
       "1st quartile hourly threshold gas price","3rd quartile hourly threshold gas price"), pos = 2)
title(ylab = "Mean absolute percentage error (MAPE)", line = 2.5)
title(xlab = "Number of hours forecast ahead of time",line = 1.5)
#plot 8####
plot(acf(BoxCox(ts_gp, lambda = "auto"), lag = 7*24), xlab = "", ylab = "", main = "", ci.col = "black", xaxt = "n")
axis(1, mgp = c(3,0.5,0))
title(ylab = "Autocorrelation function (ACF)", line = 2.5)
title(xlab = "Lag in days", line = 1.5)
#Table 2####
adf.test(diff(BoxCox(ts_gp, lambda = "auto"),24))
#plot 9####
plot(pacf(diff(BoxCox(ts_gp, lambda = "auto"),24), lag = 7*24), xlab = "", ylab = "", main = "", ci.col = "black", xaxt = "n")
axis(1, mgp = c(3,0.5,0))
title(ylab = "Partial autocorrelation function (PACF)", line = 2.5)
title(xlab = "Lag in days", line = 1.5)
#Table 3####
fit1 <- Arima(ts_gp,order = c(1,0,0), seasonal = c(0,1,1), lambda = "auto")
fit2 <- Arima(ts_gp,order = c(2,0,0), seasonal = c(0,1,1), lambda = "auto")
fit3 <- Arima(ts_gp,order = c(2,0,1), seasonal = c(0,1,1), lambda = "auto")
fit4 <- Arima(ts_gp,order = c(2,0,1), seasonal = c(0,1,2), lambda = "auto")
fit5 <- Arima(ts_gp,order = c(2,0,1), seasonal = c(1,1,1), lambda = "auto")
fit6 <- Arima(ts_gp,order = c(3,0,1), seasonal = c(0,1,1), lambda = "auto")

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
#plot 10####
fit <- Arima(ts_gp,order = c(2,0,1), seasonal = c(0,1,1), lambda = "auto")
res <- (fit$residuals)^2
plot(acf(res, lag = 7*24), xlab = "", ylab = "", main = "", ci.col = "black", xaxt = "n")
axis(1, mgp = c(3,0.5,0))
title(ylab = "Autocorrelation function (ACF)", line = 2.5)
title(xlab = "Lag in days", line = 1.5)
