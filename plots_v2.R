#libraries
library(date)
library(aTSA)
library(forecast)

#functions
collapse_by_hour <- function(data,percentile){
  start <- 1
  counter <- 1
  index <- 1
  hour <- as.factor(floor((data$timestamp)%%(3600*24)/3600))
  column <- length(data)
  len <- length(data$min)
  prices <- rep(-1,len/10)
  collapse_variable <- rep(-1,len/10)
  startblocks <- rep(-1,len/100)
  date <- rep(-1, len/100)
  while(start < len){
    if(counter < len){
      while(hour[start]==hour[counter]){
        if(counter == len){
          break
        }
        counter = counter + 1
      }
    }
    prices[index] <- quantile(data$min[start:counter-1],percentile)/10^9
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
collapse_by_hour2 <- function(data,percentile){
  start <- 1
  counter <- 1
  index <- 1
  hour <- as.factor(floor((data$timestamp)%%(3600*24)/3600))
  column <- length(data)
  len <- length(data$min)
  prices <- rep(-1,len/10)
  collapse_variable <- rep(-1,len/10)
  startblocks <- rep(-1,len/100)
  date <- rep(-1, len/100)
  while(start < len){
    if(counter < len){
      while(hour[start]==hour[counter]){
        if(counter == len){
          break
        }
        counter = counter + 1
      }
    }
    prices[index] <- quantile(data$Q1[start:counter-1],percentile)/10^9
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
collapse_by_hour3 <- function(data,percentile){
  start <- 1
  counter <- 1
  index <- 1
  hour <- as.factor(floor((data$timestamp)%%(3600*24)/3600))
  column <- length(data)
  len <- length(data$min)
  prices <- rep(-1,len/10)
  collapse_variable <- rep(-1,len/10)
  startblocks <- rep(-1,len/100)
  date <- rep(-1, len/100)
  while(start < len){
    if(counter < len){
      while(hour[start]==hour[counter]){
        if(counter == len){
          break
        }
        counter = counter + 1
      }
    }
    prices[index] <- quantile(data$lap[start:counter-1],percentile)/10^9
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
  forecast(Arima(dataset, order = p.order, seasonal = p.seasonal, lambda = 0), h=h)
}
Error <- function(predicted, observed){
  return(observed - predicted)
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


#load and extract data
s_path <- "C:/Users/Dave/Desktop/Studium/Bachelorarbeit/statistic_data_one_file"
setwd(s_path)
filename <- "statistics_5000000-8799999.txt"
tx_data <- read.csv(paste(s_path,filename, sep = "/"), sep = "\t", header = T)
tx_data$date_time <- as.POSIXct(tx_data$timestamp, origin="1970-01-01")
tx_data$date <- as.Date(tx_data$date_time)
tx_data$hour <- as.factor(floor((tx_data$timestamp)%%(3600*24)/3600))
tx_data$utilr <- round(tx_data$gas_used/tx_data$gas_limit,2)
tx_data$blocktime <- c(0,tx_data$timestamp[-1]-tx_data$timestamp[-length(tx_data$timestamp)])
tx_data$daytime <- rep("night",length(tx_data$blocktime))
tx_data$daytime[which(as.numeric(tx_data$hour)>= 7 & as.numeric(tx_data$hour)<= 18)] <- "day"
tx_data$daytime <- factor(tx_data$daytime,levels= c("day","night"))
tx_data$lap <- tx_data$min
tx_data$lap[which(tx_data$min <= 10^9)] <- tx_data$Q1[which(tx_data$min <= 10^9)]
gp_hourly <- collapse_by_hour(tx_data,0.5)
gp_hourly2 <- collapse_by_hour2(tx_data,0.5)
gp_hourly3 <- collapse_by_hour3(tx_data,0.5)
ts_gp <- ts(gp_hourly$price, frequency = 24)
ts_gp2 <- ts(gp_hourly2$price, frequency = 24)
ts_gp3 <- ts(gp_hourly3$price, frequency = 24)
ts_gp_log <- ts(log(gp_hourly$price), frequency = 24)
ts_gp_log2 <- ts(log(gp_hourly2$price), frequency = 24)
ts_gp_log3 <- ts(log(gp_hourly3$price), frequency = 24)

#graphic parameter settings
par(cex.main = 1, family = "sans", 
    mar = c(4,4,2,2), ps = 8, 
    las = 1, tcl = -0.3, xpd = F, bty = "o")

#plot 1
lo_smooth <- loess(price~blocknumber, data = gp_hourly3, span = 0.05)
date <- as.POSIXct(gp_hourly3$date, origin="1970-01-01")
plot(date, gp_hourly3$price, xaxt = "n", 
     ylim = c(0,80), pch = 20, 
     xlab = "", ylab = "")
lines(date, predict(lo_smooth), col = "azure4", lwd = 2)
r <- as.POSIXct(round(range(date), "month"))
axis(side = 1, at = seq(r[1], r[2], by = "month"), labels = F)
ticks <- seq(r[1], r[2], by = "quarter")
mtext(strftime(ticks, format = "%Y-%m"), 
      side = 1, line = 0.5, at = ticks)
title(ylab = "Hourly Median Threshold Price", xlab = "Date", line = 2)

#plot 2
n <- 10^9
foo <- hist(ifelse(tx_data$lap/n> 50,52,tx_data$lap/n), freq = F, 
            xaxt = "n", breaks = c(0:53),
            main = NULL, xlab = "", ylab = "", 
            right = F)
axis(side = 1, at = foo$mids, labels = F)
ticks <- foo$mids[c(T,F,F,F)]
mtext(paste(c(seq(0,50, by = 4), ">50")), 
      side = 1, line = 0.5, at = ticks)
title(ylab = "Density", line = 2.5)
title(xlab = "Threshold Price", line = 2)
box(lwd = 0.5)


#plot 3
boxplot(tx_data$lap/n~tx_data$daytime, outline = F, main = NULL, xaxt ="n", ylim = c(0,20))
axis(1, at=c(1,2), labels=c("Day", "Night"))

#plot 4
boxplot(tx_data$utilr~tx_data$daytime, outline = F, main = NULL, xaxt = "n")
axis(1, at=c(1,2), labels=c("Day", "Night"))

#plot 5
date <- as.POSIXct(gp_hourly3$date, origin="1970-01-01")
r <- as.POSIXct(round(range(date), "month"))
plot(date, ts_gp_log3, xaxt = "n", 
     xlab = "", ylab = "", type = "l")
axis(side = 1, at = seq(r[1], r[2], by = "month"), labels = F)
ticks <- seq(r[1], r[2], by = "quarter")
mtext(strftime(ticks, format = "%Y-%m"), 
      side = 1, line = 0.5, at = ticks)
title(ylab = "Log Threshold Price", xlab = "Date",
      line = 2)

#plot 6
plot(acf(ts_gp_log3, lag = 7*24), xlab = "", ylab = "", main = "", ci.col = "black")
title(ylab = "ACF", line = 2.5)
title(xlab = "Lag in Days", line = 2)

#plot 7
date <- as.POSIXct(gp_hourly3$date, origin="1970-01-01")
r <- as.POSIXct(round(range(date), "month"))
plot(date[25:length(date)],diff(ts_gp_log3,24), xaxt = "n", 
     xlab = "", ylab = "", type = "l")
axis(side = 1, at = seq(r[1], r[2], by = "month"), labels = F)
ticks <- seq(r[1], r[2], by = "quarter")
mtext(strftime(ticks, format = "%Y-%m"), at = ticks, side = 1, line = 0.5)
title(ylab = "Seasonal Threshold Price", xlab = "Date",
      line = 2, cex.lab = 1)

#adf test
adf.test(diff(ts_gp_log3,24))

#plot 8
plot(pacf(diff(ts_gp_log3,24), lag = 7*24), xlab = "", ylab = "", main = "", ci.col = "black")
title(ylab = "PACF of the Threshold Price", line = 2.5)
title(xlab = "Lag in Days", line = 2)

#Model estimation
fit1 <- Arima(ts_gp3,order = c(1,0,0), seasonal = c(0,1,1), lambda = 0)
fit2 <- Arima(ts_gp3,order = c(2,0,0), seasonal = c(0,1,1), lambda = 0)
fit3 <- Arima(ts_gp3,order = c(2,0,1), seasonal = c(0,1,1), lambda = 0)
fit4 <- Arima(ts_gp3,order = c(2,0,1), seasonal = c(0,1,2), lambda = 0)
fit5 <- Arima(ts_gp3,order = c(2,0,1), seasonal = c(1,1,1), lambda = 0)
fit6 <- Arima(ts_gp3,order = c(3,0,1), seasonal = c(0,1,1), lambda = 0)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

#plot 9
fit <- Arima(ts_gp3,order = c(2,0,1), seasonal = c(0,1,1), lambda = 0)
qqnorm(fit$residuals, xlim = c(-4,4), ylim = c(-4,4), 
       main = "", xlab = "", ylab = "")
abline(a = 0, b = 1)
title(ylab = "Sample Quantiles", xlab = "Theoretical Quantiles (Normal Distribution)",
      line = 2)

#plot 10
res_mape1 <- CVts_rep(dataset = ts_gp3, forecast_function = far, forecast_horizon = 24,
                      error_function = P.Error, p.order = c(2,0,1), 
                      p.seasonal = c(0,1,1), n = 500, initial = 60, seed = 1)
res_mape2 <- CVts_rep(dataset = ts_gp3, forecast_function = far, forecast_horizon = 24,
                     error_function = P.Error, p.order = c(0,1,0), 
                     p.seasonal = c(0,0,0), n = 500, initial = 60, seed = 1)
res_mape3 <- CVts_rep(dataset = ts_gp3, forecast_function = far, forecast_horizon = 24,
                     error_function = P.Error, p.order = c(0,0,0), 
                     p.seasonal = c(0,1,0), n = 500, initial = 60, seed = 1)
res_mae1 <- CVts_rep(dataset = ts_gp, forecast_function = far, forecast_horizon = 24,
                      error_function = Error, p.order = c(2,0,1), 
                      p.seasonal = c(0,1,1), n = 500, initial = 60, seed = 1)
res_mae2 <- CVts_rep(dataset = ts_gp, forecast_function = far, forecast_horizon = 24,
                     error_function = Error, p.order = c(0,1,0), 
                     p.seasonal = c(0,0,0), n = 500, initial = 60, seed = 1)
res_mae3 <- CVts_rep(dataset = ts_gp, forecast_function = far, forecast_horizon = 24,
                     error_function = Error, p.order = c(0,0,0), 
                     p.seasonal = c(0,1,0), n = 500, initial = 60, seed = 1)

plot(c(1:24),rowMeans(abs(res_mape1)), type = "l", ylim = c(0.1,0.9))
points(c(1:24), y = rowMeans(abs(res_mape2)), type = "l", col = "blue")
points(c(1:24), y = rowMeans(abs(res_mape3)), type = "l", col = "red")
plot(c(1:24),rowMeans(abs(res_mae1)), type = "l", ylim = c(1,4))
points(c(1:24), y = rowMeans(abs(res_mae2)), type = "l", col = "blue")
points(c(1:24), y = rowMeans(abs(res_mae3)), type = "l", col = "red")
mean(ts_gp)

res_mape1.3 <- CVts_rep(dataset = ts_gp3, forecast_function = far, forecast_horizon = 24,
                      error_function = P.Error, p.order = c(2,0,1), 
                      p.seasonal = c(0,1,1), n = 500, initial = 60, seed = 1)
res_mape2.3 <- CVts_rep(dataset = ts_gp3, forecast_function = far, forecast_horizon = 24,
                      error_function = P.Error, p.order = c(0,1,0), 
                      p.seasonal = c(0,0,0), n = 500, initial = 60, seed = 1)
res_mape3.3 <- CVts_rep(dataset = ts_gp3, forecast_function = far, forecast_horizon = 24,
                      error_function = P.Error, p.order = c(0,0,0), 
                      p.seasonal = c(0,1,0), n = 500, initial = 60, seed = 1)
plot(c(1:24),rowMeans(abs(res_mape1.3)), type = "l", ylim = c(0.1,0.8))
points(c(1:24), y = rowMeans(abs(res_mape2.3)), type = "l", col = "blue")
points(c(1:24), y = rowMeans(abs(res_mape3.3)), type = "l", col = "red")
#plot 11

#plot 12

#plot 13


##Apendix
s_path2 <- "C:/Users/Dave/Desktop/Studium/Etheruem Gas Price Statistics/mining strategies"
setwd(s_path2)
filename2 <- "tx_data.txt"
filename3 <- "different_mining_strategy.csv"
tx_data2 <- read.csv(filename2, header = T, sep = "\t")
tx_data2$gasPrice <- tx_data2$gasPrice/10^9
blocks <- read.csv(filename3, header = F)
blocks <- as.vector(data.matrix(blocks))

block_strategies <- data.frame(unique(tx_data2$number))
block_strategies$strategy <- rep(1,1000)
colnames(block_strategies) <- c("number", "strategy")

assign_strategies <- function(data, blocks){
  strategies <- data
  for(block in blocks){
    strategies$strategy[which(strategies$number == block)] <- 0
  }
  return(strategies[order(strategies$number),])
}

block_strategy <- assign_strategies(block_strategies, blocks)

#plot 1
lo <- loess(strategy ~ number, data = block_strategy, span = 0.7)
k <- 60
plot(block_strategy$number[(k/2):(1000-k/2)], block_strategy$strategy[(k/2):(1000-k/2)], 
     xlab = "Block Number", ylab = "Mining Strategy", pch = 20)
lines(block_strategy$number[(k/2):(1000-k/2)],predict(lo)[(k/2):(1000-k/2)], col='azure4', lwd=2)


#plot 2
subs_utilr <- subset(tx_data, utilr > 0.9)
hist(tx_data$date, breaks = "weeks", format = "%y-%m", freq = T, 
     main = NULL, xlab = "", ylab = "", ylim = c(0,50000))
hist(subs_utilr$date, "weeks", add = T, col = "grey", freq = T, axes = F)
title(ylab = "Number of Blocks", line = 3)
title(xlab = "Date", line = 2.5)
box(lwd = 0.5)


