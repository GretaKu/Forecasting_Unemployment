library(tseries)  
library(xts)      # handling of time-series data (is it needed at all? check!!!)
library(forecast) 
library(tsbox)    
library(seasonal)
library(mFilter)
library(rio)
library(tidyverse)

#import
RawData <- import(here::here("swiss_unemployment.xlsx"))
SNB <- import(here::here("snb-data-arbeitslose-00-20.xlsx"))

#filtering for registred unemployed
RawData <- filter(RawData,unemployment == "Registrierte Arbeitslose")
RawData <- RawData %>% dplyr::select(time,swiss)

#transforming to timeseries data
head(SNB)
data <- as.xts(x = SNB[-1], order.by = as.Date(as.yearmon(SNB$time)))
head(data)
str(data)
names(data)

#visualize
plot.xts(data$arbeitslose,  main = "Swiss Unemployment")
addLegend("topleft", on=1, 
          legend.names = c("Seasonally adjusted", "Not seasonally adjusted"), 
          lty=c(1, 1),
          col=c("black", "red"))



## Deterministic trend
model_Trend <- lm(log(data$arbeitslose) ~ c(1:length(data$arbeitslose)))
plot.xts(cbind(log(data$arbeitslose),model_Trend$fitted.values), main = "Unemployment and linear trend")

#plot residuals
plot.xts(as.xts(model_Trend$residuals),  main = "Unemployment: deviations from linear trend")

## Stochastic Trend
dlnData = diff.xts(as.xts(log(data)),1,1,na.pad=FALSE,log=TRUE)
plot.xts(dlnData$arbeitslose,  main = "Unemployment: quarterly growth, non-seasonally adjusted")


## Deterministic season
quarterlyDummies <- forecast::seasonaldummy(ts_ts(data$arbeitslose))
model_Season <- lm(log(data$arbeitslose) ~ quarterlyDummies)

#plot
plot.xts(as.xts(model_Trend$residuals),  main = "Unemployment: deviations from linear trend and seasonal dummy")




