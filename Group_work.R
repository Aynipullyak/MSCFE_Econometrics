# Basic Time Plots:
# Load necessary packages
library(tidyverse)
library(ggplot2)
#load the library
# Read the excel and assign data
library(readxl)
library(moments)
library(readxl)
library(stats)
library(tseries)
library(forecast)

#Load data

AssetRet <- read_excel("C:/Econometrics/Group_Work/Data/AssetRet.xlsx", 
                       col_types = c("date", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))
View(AssetRet)

View(AssetRet)
attach(AssetRet)

# view time plots of the closing price of Microsoft:

ggplot (data=AssetReturns) + geom_point(mapping = aes(x = Date , y = WMT),
                                   color = "darkblue") + 
  labs (x = "year" , y = "Walmart -Daily Closing Price")

ggsave ("C:/Econometrics/Group_Work/Graphs/Walmart_closing_price.jpeg")

library(kdensity)
hist(AssetRet$WMT_lr, breaks = 100, freq = FALSE,
     main = "Walmart",xlab = 'daily log returns')

kde_WMT = kdensity(AssetRet$WMT_lr, start = "normal")
# plot the kernel density estimate
lines(kde_WMT, col = "blue")
# plot the closest normal distribution
lines(kde_WMT, plot_start = TRUE, col = "red")
# add a legend
legend("topright", c("kernel estimate", "normal"),
       lty = c(1, 1), lwd = c(1, 1), col = c("blue", "red"))

mean_wmt = mean(AssetRet$WMT_lr)
sd_wmt = sd(AssetRet$WMT_lr)
min1_wmt = min(AssetRet$WMT_lr)
max_wmt = max(AssetRet$WMT_lr)
# standardize return data to have zero mean and unit variance:
WMT_std = (AssetRet$WMT_lr - mean_wmt)/sd_wmt
# fit quantile quantile plots and
# reference line going through the 25th and 75th percentile
qqnorm(WMT_std, main = "Normal Q-Q Plot - Walmart",
       plot.it = TRUE, datax = TRUE)
qqline(WMT_std, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)

#scatter plots:
# scatterplot of Microsoft vs Intel log daily returns:

jpeg (file="C:/Econometrics/Group_Work/Graphs/SCplot_WMT_AMZ.jpeg",
    width=600, height=350)
plot(AssetRet$WMT_lr,AssetRet$AMZN_lr, xlab = "Walmart log returns", 
     ylab = "Amazonlog returns" , 
     main = "Scatter plot of log retruns of Walmart and Amazon")
dev.off()

jpeg (file="C:/Econometrics/Group_Work/Graphs/SCplot_WMT_GS.jpeg",
      width=600, height=350)

plot(AssetRet$WMT_lr,AssetRet$GS_lr, xlab = "Walmart log returns", 
     ylab = "GS log returns" ,
     main = "Scatter plot of log retruns of Walmart and GS")
dev.off()

jpeg (file="C:/Econometrics/Group_Work/Graphs/SCplot_WMT_Citi.jpeg",
      width=600, height=350)
plot(AssetRet$WMT_lr,AssetRet$C_lr, xlab = "Walmart log returns", 
     ylab = "Citi log returns" ,
     main = "Scatter plot of log retruns of Walmart and Citi")
dev.off()


jpeg (file="C:/Econometrics/Group_Work/Graphs/SCplot_WMT_BAC.jpeg",
      width=600, height=350)
plot(AssetRet$WMT_lr,AssetRet$BAC_lr, xlab = "Walmart log returns", 
     ylab = "BAC log returns" ,
     main = "Scatter plot of log retruns of Walmart and BAC")
dev.off()

kurtosis(WMT_lr,na.rm = FLASE)
skewness(WMT_lr)
var(WMT_lr)
sd(WMT_lr)
mean_wmt
sd_wmt



ggplot(data = AssetRet, mapping = aes(x=AssetRet$Date,
                                       y=AssetRet$BAC)) + geom_line(color ="darkblue") +
  labs(x="",y="Price Series - compounded annual rate of change")


# Stationary analysis , ARMA Prediction for Price, Return ,Square of return for Bank of America

BAC_ts <-  ts(AssetRet_ex$BAC, start=2007,frequency = 251)

plot(BAC_ts,xlab="Date-time",ylab="Price")

acf(BAC_ts,lag.max = NULL)
pacf(BAC_ts,lag.max = NULL)
adf.test(BAC_ts)
kpss.test(BAC_ts)
ar_bac = auto.arima(BAC_ts)

summary(ar_bac)


BAC_rt_ts <-  ts(AssetRet$BAC_lr, start=2007,frequency = 251)

BAC_rt_ts_ord1 <- ts(diff(diff(AssetRet$BAC_lr)),start=2007,frequency = 251)

acf(BAC_rt_ts,lag.max = NULL)
pacf(BAC_rt_ts,lag.max = NULL)
adf.test(BAC_rt_ts)
kpss.test(BAC_rt_ts)

arima(BAC_rt_ts, order=c(2, 0, 2))

 final.aic <- Inf
final.order <- c(0,0,0)
 for (i in 0:4) for (j in 0:4) {
     current.aic <- AIC(arima(BAC_rt_ts, order=c(i, 0, j)))
     if (current.aic < final.aic) {
         final.aic <- current.aic
         final.order <- c(i, 0, j)
       final.arma <- arima(BAC_rt_ts, order=final.order)
       }
 }

final.arma
final.aic

Box.test(final.arma$residuals, lag = 3)

bac_ts_model <-auto.arima(BAC_rt_ts)

summary(bac_ts_model)

Box.test(bac_ts_model$residuals, lag = 3)



plot(BAC_rt_ts,xlab="Date-time",ylab="log returns")

acf(BAC_rt_ts,lag.max = NULL)
pacf(BAC_rt_ts,lag.max = NULL)
adf.test(BAC_rt_ts)
kpss.test(BAC_rt_ts)

BAC_rt_sq_ts <-  ts(AssetRet$BAC_lr_sq, start=2007,frequency = 251)
plot(BAC_rt_sq_ts,xlab="Date-time",ylab="log returns")

acf(BAC_rt_sq_ts,lag.max = NULL)
pacf(BAC_rt_sq_ts,lag.max = NULL)
adf.test(BAC_rt_sq_ts)
kpss.test(BAC_rt_sq_ts)

bac_lr_sq_model <- auto.arima(BAC_rt_sq_ts)

summary(bac_lr_sq_model)

Box.test(bac_lr_sq_model$residuals, lag = 5)

final1.aic <- Inf
final1.order <- c(0,0,0)
for (i in 0:5) for (j in 0:5) {
  current1.aic <- AIC(arima(BAC_rt_sq_ts, order=c(i, 0, j)))
  if (current1.aic < final1.aic) {
    final1.aic <- current1.aic
    final1.order <- c(i, 0, j)
    final1.arma <- arima(BAC_rt_sq_ts, order=final.order)
  }
}

final1.arma
final1.aic

# Stationary analysis , ARMA Prediction for Price, Return ,Square of return for Amazon

Amazon_ts <-  ts(AssetRet$AMZN, start=2007,frequency = 251)

plot(Amazon_ts,xlab="Date-time",ylab="Price")



acf(Amazon_ts,lag.max = NULL)
pacf(Amazon_ts,lag.max = NULL)
adf.test(Amazon_ts)
kpss.test(Amazon_ts)
ar_bac = auto.arima(BAC_ts)

Amazon_rt_ts <-  ts(AssetRet$AMZN_lr, start=2007,frequency = 251)

plot(Amazon_rt_ts)

amzn_rt_model <- auto.arima(Amazon_rt_ts)

summary(amzn_rt_model)

Box.test(amzn_rt_model$residuals, lag = 2)

acf(Amazon_rt_ts,lag.max = NULL)
pacf(Amazon_rt_ts,lag.max = NULL)
adf.test(Amazon_rt_ts)
kpss.test(Amazon_rt_ts)
ar_bac = auto.arima(Amazon_rt_ts)

Amazon_rt_sq_ts <-  ts(AssetRet$AMZN_lr_sq, start=2007,frequency = 251)

acf(Amazon_rt_sq_ts,lag.max = NULL)
pacf(Amazon_rt_sq_ts,lag.max = NULL)
adf.test(Amazon_rt_sq_ts)
kpss.test(Amazon_rt_sq_ts)
ar_bac = auto.arima(Amazon_rt_ts)

amzn_rt_sq_model <- auto.arima(Amazon_rt_sq_ts)

summary(amzn_rt_sq_model)
Box.test(amzn_rt_sq_model$residuals, lag = 1)

# Stationary analysis , ARMA Prediction for Price, Return ,Square of return for Citi Group

Citi_ts <-  ts(AssetRet$C, start=2007,frequency = 251)

plot(Amazon_ts,xlab="Date-time",ylab="Price")

?acf

acf(Citi_ts,lag.max = NULL)
pacf(Citi_ts,lag.max = NULL)
adf.test(Citi_ts)
kpss.test(Citi_ts)
ar_bac = auto.arima(Citi_ts)

Citi_rt_ts <-  ts(AssetRet$C_lr, start=2007,frequency = 251)
acf(Citi_rt_ts,lag.max = NULL)
pacf(Citi_rt_ts,lag.max = NULL)
adf.test(Citi_rt_ts)
kpss.test(Citi_rt_ts)
ar_bac = auto.arima(Citi_ts)

plot(Citi_rt_ts)

citi_rt_model <- auto.arima(Citi_rt_ts)

summary(citi_rt_model)
Box.test(citi_rt_model$residuals, lag = 5)


Citi_rt_sq_ts <-  ts(AssetRet$C_lr_SQ, start=2007,frequency = 251)
plot(Citi_rt_sq_ts)
citi_rt_sq_model <- auto.arima(Citi_rt_sq_ts)
summary(citi_rt_sq_model)

Box.test(citi_rt_sq_model$residuals, lag = 3)

acf(Citi_rt_sq_ts,lag.max = NULL)
pacf(Citi_rt_sq_ts,lag.max = NULL)
adf.test(Citi_rt_sq_ts)
kpss.test(Citi_rt_sq_ts)

# Stationary analysis , ARMA Prediction for Price, Return ,Square of return for Gold man Sachs 


GS_ts <-  ts(AssetRet$GS, start=2007,frequency = 251)
acf(GS_ts,lag.max = NULL)
pacf(GS_ts,lag.max = NULL)
adf.test(GS_ts)
kpss.test(GS_ts)

GS_rt_ts <-  ts(AssetRet$GS_lr, start=2007,frequency = 251)
acf(GS_rt_ts,lag.max = NULL)
pacf(GS_rt_ts,lag.max = NULL)
adf.test(GS_rt_ts)
kpss.test(GS_rt_ts)

plot(GS_rt_ts)
GS_rt_ts_model <- auto.arima(GS_rt_ts)
summary(GS_rt_ts_model)

Box.test(GS_rt_ts_model$residuals, lag = 2)

GS_rt_sq_ts <-  ts(AssetRet$GS_lr_sq, start=2007,frequency = 251)
plot(GS_rt_sq_ts)

acf(GS_rt_sq_ts,lag.max = NULL)
pacf(GS_rt_sq_ts,lag.max = NULL)
adf.test(GS_rt_sq_ts)
kpss.test(GS_rt_sq_ts)

plot(GS_rt_ts)
GS_rt_sq_ts_model <- auto.arima(GS_rt_sq_ts)
summary(GS_rt_sq_ts_model)

Box.test(GS_rt_sq_ts_model$residuals, lag = 2)

# Stationary analysis , ARMA Prediction for Price, Return ,Square of return for Walmart 

WMT_ts <-  ts(AssetRet$WMT, start=2007,frequency = 251)
acf(WMT_ts,lag.max = NULL)
pacf(WMT_ts,lag.max = NULL)
adf.test(WMT_ts)
kpss.test(WMT_ts)


WMT_rt_ts <-  ts(AssetRet$WMT_lr, start=2007,frequency = 251)
acf(WMT_rt_ts,lag.max = NULL)
pacf(WMT_rt_ts,lag.max = NULL)
adf.test(WMT_rt_ts)
kpss.test(WMT_rt_ts)

plot(WMT_rt_ts)
WMT_rt_ts_model <- auto.arima(WMT_rt_ts)
summary(WMT_rt_ts_model)

Box.test(WMT_rt_ts_model$residuals, lag = 1)


WMT_rt_sq_ts <-  ts(AssetRet$WMT_lr_sq, start=2007,frequency = 251)
acf(WMT_rt_sq_ts,lag.max = NULL)
pacf(WMT_rt_sq_ts,lag.max = NULL)
adf.test(WMT_rt_sq_ts)
kpss.test(WMT_rt_sq_ts)

plot(WMT_rt_sq_ts)
WMT_rt_ts_sq_model <- auto.arima(WMT_rt_sq_ts)
summary(WMT_rt_ts_sq_model)

Box.test(WMT_rt_ts_sq_model$residuals, lag = 1)

ar_bac = auto.arima(Citi_rt_sq_ts)

#Code for forecating

BAC_ts_act <-  ts(AssetRet$BAC, start=2007,frequency = 251)#,frequency=30)

AR_forecast <- predict(ar_bac,n.ahead= 100,se.fit=TRUE)

plot(BAC_ts)
lines(AR_forecast$pred,col="blue")
lines(AR_forecast$pred+2*AR_forecast$se,col="cornflowerblue",lty = "dashed")
lines(AR_forecast$pred-2*AR_forecast$se,col="cornflowerblue",lty = "dashed")



