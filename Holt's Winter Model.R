#==============================================
# 7 Holts' winter model for random fluctuations
#==============================================

rain<- scan("https://robjhyndman.com/tsdldata/hurst/precip1.dat", skip=1)
rainseries<- ts(rain, start = c(1813))
plot.ts(rainseries)

rsfc<- HoltWinters(rainseries, beta = FALSE, gamma = FALSE)
rsfc

rsfc$fitted
plot(rsfc)

rsfc2<-forecast:::forecast.HoltWinters(rsfc, h=8)
rsfc2

forecast:::plot.forecast(rsfc2)

#==============================================
#Checking for Autocorrelation
#==============================================

#1- Ljung-Box Test
Box.test(rsfc2$residuals, lag = 20, type = "Ljung-Box")

#2- ACF plot
acf(rsfc2$residuals,lag.max = 20, na.action = na.pass)

#3- Residual plot
plot.ts(rsfc2$residuals)
#no particular pattern in residuals #good model

#4- histogram
hist(rsfc2$residuals, col = "red", freq = FALSE)
#almost normal
#density plot fits on the model
#residuals are good
#model forecasts are dependable

#==============================================
# 8 Holts' winter model for trend
#==============================================
skirts<- scan("https://robjhyndman.com/tsdldata/roberts/skirts.dat", skip=5)
skirtseries<- ts(skirts, start = c(1866))
plot.ts(skirtseries)
View(skirts)

ssfc<- HoltWinters(skirtseries, gamma = FALSE)
ssfc

ssfc$fitted
plot(ssfc, legend("topright", legend = c("fitted", "actual"),
                  lwd = 3, col = c("red", "black")))

ssfc2<-forecast:::forecast.HoltWinters(ssfc, h=19)
ssfc2

forecast:::plot.forecast(ssfc2)
forecast:::plot.forecast(ssfc2, shaded = TRUE, shadecols = c("yellow", "orange"), fcol = "blue")

#==============================================
#Checking for Autocorrelation
#==============================================

#1- Ljung-Box Test
Box.test(ssfc2$residuals, lag = 20, type = "Ljung-Box")
#residuals are not correlated
#data is smoothened; good to go

#2- ACF plot
acf(ssfc2$residuals,lag.max = 20, na.action = na.pass)
#residuals are not correlated
#data is smoothened; good to go

#3- Residual plot
plot.ts(ssfc2$residuals)
#no particular pattern in residuals #good model

#4- histogram
hist(ssfc2$residuals, col = "red", freq = FALSE)
#positive errors= negative errors
#density plot fits on the model
#residuals are good
#model forecasts are dependable

#==============================================
# 9 Holts' winter model for trend and seasonality
#==============================================
sales<- (scan("https://robjhyndman.com/tsdldata/data/fancy.dat"))
summary(sales)
View(sales)
saleseries<- ts(log(sales), start=c(1987,1),end=c(1993,12),frequency=12)
plot.ts(saleseries)
View(saleseries)

#forecast using holtwinters function
sstfc<- HoltWinters(saleseries)
sstfc
#gives info about alpha, beta, gamma

sstfc$fitted
plot(sstfc, legend("bottomright", legend = c("forecasted line", "actual line"),
                 lwd = 3, col = c("red", "black")))


sstfc2<-forecast:::forecast.HoltWinters(sstfc, h=48)
sstfc2

forecast:::plot.forecast(sstfc2)
forecast:::plot.forecast(sstfc2, shaded = TRUE, shadecols = c("yellow", "orange"), fcol = "blue")

#OR
#Using the HW func

sstfc1<- hw(saleseries,h=48, seasonal = "multiplicative")
sstfc1
forecast:::plot.forecast(sstfc1)
forecast:::plot.forecast(sstfc1, shaded = TRUE, shadecols = c("yellow", "orange"), fcol = "blue")

#==============================================
#Checking for Autocorrelation
#==============================================

#1- Ljung-Box Test
Box.test(sstfc2$residuals, lag = 20, type = "Ljung-Box")
#residuals are not correlated as p value greater than 0.5
#data is smoothened; good to go

#2- ACF plot
acf(sstfc2$residuals,lag.max = 20, na.action = na.pass)
#residuals are not correlated
#data is smoothened; good to go

#3- Residual plot
plot.ts(sstfc2$residuals)
#no particular pattern in residuals #good model

#4- histogram
hist(sstfc2$residuals, col = "red", freq = FALSE)
#normal curve
#density plot fits on the model
#residuals are good
#model forecasts are dependable

