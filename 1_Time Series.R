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

