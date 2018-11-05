library(sqldf)
library("forecast")
library(MASS)
library(xlsx)
library(fma)
library(lubridate)
library(tseries)
library(pracma)

##### Data Read #####
data = read.table("data.txt",header=TRUE,sep=",")

##### Examine the Data #####
data$UygunFormatGun <- as.Date(data$Gun,"%d-%m-%Y")
query = "select Gun ,Magaza,Lokasyon,Kod,SaticiurunAdi,ANAGRUP,ALTGRUP,URUNCESIDI, SUM(SatisMiktari) as Miktar, UygunFormatGun from data where ANAGRUP = 'anagrup1' and ALTGRUP ='alt-grup-2' group by UygunFormatGun order by UygunFormatGun"
result <- sqldf(query)

##### Time Series #####
inds <- seq(as.Date(min(result$UygunFormatGun)), as.Date(max(result$UygunFormatGun)), by = "day")
result.timeseries <- ts(result$Miktar,start = c(2016, as.numeric(format(inds[1], "%j"))), frequency = 365)
x = time(result.timeseries)
y = result.timeseries


plot(x,y,type="l", xlab = "Time", ylab = "Quantity", main = "Alt Grup 2")

##### Check Seosanality #####
ets(y)
fit <- tbats(y)
seasonal <- !is.null(fit$seasonal)
seasonal
fit1 <- ets(y)
fit2 <- ets(y,model="ANN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df
#P value
#1-pchisq(deviance,df)

##### Auto-Correlation #####
Acf(result.timeseries, lag.max=50)
Pacf(result.timeseries, lag.max=50)

##### Stationary Test #####
Box.test(result.timeseries,lag=20,type="Ljung-Box")
adf.test(result.timeseries,alternative = "stationary")
kpss.test(result.timeseries)
##### Fitting #####

##### Linear Regression #####
#relation <- lm(y~x)
#a <- data.frame(y)
#result$prediction <-  predict(relation,a)

#arima
auto.arima(y)

fit_arima <- Arima(y, order=c(2,0,0))
fit_val <- residuals(fit_arima)
fit_val <- fit_val + 10
summary(fit_arima)
result$prediction <- fit_val
tsdisplay(fit_val)

arimaforecasts <- forecast.Arima(fit_arima, h=25)

plot.forecast(arimaforecasts)

##### Chi Square #####
tbl = table(y, fit_val)
chisq.test(tbl)

##### Plot the result #####
png(file = "GunlereBagliMikar_Altgrup2.jpg")

plot(x,y, ylim=range(c(y,fit_val)),type = "l", col = "red", xlab = "", ylab = "", main = "")
par(new=T)
plot(x,fit_val, ylim=range(c(y,fit_val)), type = "o", col = "green", xlab = "Time", ylab = "Quantity", main = "Created Data + Arima Fit Data")
dev.off()


write.xlsx(result, file = "results.xlsx",
           sheetName = "TestSheet", row.names = FALSE)


write.xlsx(arimaforecasts, file = "forecasts.xlsx",
           sheetName = "TestSheet", row.names = FALSE)
