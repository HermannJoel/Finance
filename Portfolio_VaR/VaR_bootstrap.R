library(mosaic)
library(fImport)
library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
library(quantmod)
library(Quandl)
Quandl.api_key("HeSenjS7xyCu9Kw3-sBs")

mystocks =c("SPY", "TLT", "LQD", "DBC", "VNQ")
myprices = yahooSeries(mystocks, from="2010-12-31", to="2020-12-31")
getwd()
myreturns <- read.csv("log_returns.csv", header=TRUE, row.names="Date")
head(myreturns)
#To view the returns as a joint distribution
pairs(myreturns)
plot(myreturns[,3], type = "l")

#Simulate one day change in our port
#we will allocate 20% to each asset.
totalwealth = 200000
weights = c(0.090, 0.080, 0.083, 0.083, 0.083, 0.083, 
            0.083, 0.083, 0.083, 0.083, 0.083, 0.083)#percentage of the wealth we put in each stock
sum(weights)

#how much money do we have in each stock
holdings= weights * totalwealth
#sample a random return from the empirical joint dist
#this simulate a random day
return.today = resample(myreturns, 1, orig.ids=FALSE)
return.today
#update the value of holding
holdings = holdings + holdings*return.today
holdings

#We loop over 20 trading day(4 weeks)
totalwealth = 200000
horizon = 20
weights = c(0.090, 0.080, 0.083, 0.083, 0.083, 0.083, 
            0.083, 0.083, 0.083, 0.083, 0.083, 0.083)
holdings = weights * totalwealth
wealthtracker = rep(0, horizon)

for (today in 1:horizon) {
  return.today = resample(myreturns, 1, orig.ids=FALSE)
  holdings = holdings + holdings*return.today
  totalwealth = sum(holdings)
  wealthtracker[today] = totalwealth
}
totalwealth
plot(wealthtracker)
title(main = "wealth tracker",col = "red", font = 1)
abline(h=240000, col="green")

#Now simulate many different possible 4 weeks trading period(20 days)
#we out pout the total wealth after 4 weeks
sim = do(2000)*{
  totalwealth = 200000
  weights = c(0.090, 0.080, 0.083, 0.083, 0.083, 0.083, 
              0.083, 0.083, 0.083, 0.083, 0.083, 0.083)
  holdings = weights * totalwealth
  for (today in 1:horizon) {
    return.today = resample(myreturns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
  }
 totalwealth 
}
#visualize and summarize the results
hist(sim$result, 50)
mean(sim$result)
sd(sim$result)
#We can calculate an expected utility
#if u(w) = log(w), then:
mu_utility = mean(log(sim$result))
mu_utility

#VaR
profit = sim$result - 200000
hist(profit, 50)
#We calculate the 5% VaR
#It represent the 5% quantile of the p/l distribution
qdata(profit, 0.05)

VaR05 = qdata(profit, 0.05)[1]
abline(v=VaR05, col="red", lwd=2)
abline(v=mean(profit), col="green", lwd=2)#mean Expected return of the port

?getSymbols
mydata = Quandl("FRED/GDP.1", start_date="2015-01-01", 
                end_date="2021-12-31")
tail(mydata)
mydata = Quandl("FRED/GDP", start_date="2015-01-01", 
                end_date="2021-12-31")
mydata = Quandl(c("WIKI/AAPL.11", "WIKI/MSFT.11"), start_date="2015-01-01", 
                end_date="2021-12-31")
head(mydata)
tail(mydata)



