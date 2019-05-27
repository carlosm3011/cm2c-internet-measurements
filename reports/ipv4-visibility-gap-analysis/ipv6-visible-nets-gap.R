# IPv4 Runout Phase 3 Modeling: carlos@lacnic.net
# v1: 2018-02-15 @Brisbane

# dependencies
# install.packages("jsonlite")
# install.packages("curl")
library("jsonlite")

# clean environment
rm(list=ls())

# helper function: converts a "mongo date" string into an Unix-like timestamp
tsFromMongoDate <- function(x) {
  a = gsub("Date(", "", x, fixed=TRUE)
  b = gsub(")", "", a, fixed=TRUE)
  e = unlist(strsplit(b, ","), use.names = FALSE)
  
  f = paste(e[1],as.integer(e[2])+1,e[3], sep="-") 
  r = gsub(" ", "", f)
  #return(r)
  p = as.numeric(as.POSIXct(r, tz="UTC"))
}
# -- end function --

# Getting and cleaning data
# setwd("~/scratch")

getIPv6VisibilityData <- function() {
  d <- fromJSON("http://opendata.labs.lacnic.net/ipv6stats/ipv6pfx-alloc-over-time/ALL/ALL")
  # d <- fromJSON("freeipv4_lacnic.json")
  e <- unlist(d[1], use.names=FALSE, recursive=FALSE)
  f <- t(data.frame(e))
  
  dates = lapply(f[,1], tsFromMongoDate)
  ipv6_assign = lapply(f[,2], as.integer)
  ipv6_visible = lapply(f[,3], as.integer)
  
  ipv6_assign = t ( as.matrix( as.data.frame(ipv6_assign)))
  ipv6_assign = as.numeric(ipv6_assign[,1])
  
  ipv6_visible = t ( as.matrix( as.data.frame(ipv6_visible)))
  ipv6_visible = as.numeric(ipv6_visible[,1])
  
  dates = t ( as.matrix( as.data.frame(dates)))
  dates = as.numeric(dates[,1])
  
  ipv6vis = NULL
  ipv6vis = cbind(ipv6_assign, ipv6_visible, dates)
  # ipv4 = cbind(ipv4, free4)
  # ipv4 = as.data.frame(ipv4)
  
  return(ipv6vis)
}

ipv6vis <- as.data.frame( getIPv6VisibilityData() )
ipv6vis <- cbind(ipv6vis$ipv6_visible / ipv6vis$ipv6_assign * 100, ipv6vis )

# scatter.smooth( head(ipv6vis[,1], 120) )

backdays = 365
scatter.smooth(head(ipv6vis[,4], backdays), head(ipv6vis[,1], backdays), ylim=range(20,43) )

## Modeling and prediction
# m5 = lm(free4 ~ dates_free4, data = ipv4)
#m5 = lm(free4 ~ poly(dates_free4, 1), data = ipv4)
#pr2 = predict(m5, interval="confidence")

# plot(dates_free4, free4)
# lines(dates_free4, pr2[,'fit'], col="green", lwd=3)

# Forecasting ----------------------------------------------------
#secsday = 86400
#future_horizon_days = 365*3
#future_dates_free4 = seq(ipv4$dates_free4[1], ipv4$dates_free4[1]+secsday*future_horizon_days, secsday)
#future_free4 = predict(m5, newdata = data.frame(dates_free4=future_dates_free4))
#fcast = as.data.frame( cbind(future_dates_free4, future_free4))
#cutoff = which(fcast$future_free4 < 0)
#cutoff_offset = cutoff[1]
#cutoff_ts = future_dates_free4[cutoff_offset]
#cutoff_date = as.Date(as.POSIXct(cutoff_ts, origin="1970-01-01"))
# -----------------------------------------------------------------

# Plotting
#plot(c(future_dates_free4, ipv4$dates_free4), c(future_free4, pr2[,'fit']), 
#     col='green', lwd=1, xlab='', ylab='', xaxt="n", yaxt="n")
#axis(side=2, seq(from=0, to=5)*1e6, c("0", "1M", "2M", "3M", "4M", "5M"))
#ttlabels = c("2017-1-1","2018-1-1", "2019-1-1", "2020-1-1", "2021-1-1")
#tticks = as.numeric(as.POSIXct(ttlabels, tz="UTC"))
#axis(side=1, tticks, ttlabels)
#title("LACNIC IPv4 Exhaustion Phase 3 Model", sub="Linear model", xlab="Time", ylab="Free IPv4 Addresses (millions)")
#lines(ipv4$dates_free4, ipv4$free4, col='black', lwd=3, xlab='')
#lines(c(future_dates_free4, ipv4$dates_free4), numeric(length(c(future_dates_free4, ipv4$dates_free4))), col='yellow', xlab='')
#points(future_dates_free4[cutoff_offset], 0, col='red', lwd=5, xlab='')
#text(cutoff_ts, 300000, labels=c(cutoff_date))

#print(summary(m5))

## end
