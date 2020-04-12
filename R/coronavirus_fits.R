#### Title ####
# Exponential and Logistic Fits for Coronavirus Deaths and Cases
# Sam Clark
# 2020-03-28
# 2020-03-29 
# 2020-03-31 
# 2020-04-05
# 2020-04-12- latest


#### Start up ####

rm(list=ls())

#### Packages ####

list.of.packages <- c(
  "readr"
  ,"lubridate"
)

# identify required packages that are not already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install required packages that are not already installed
if(length(new.packages)) install.packages(new.packages,type="binary")
# load the packages
lapply(list.of.packages, require, character.only = TRUE)


#### Data ####

# download data from COVID Tracking Project Github site
# URL is https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/us_daily.csv
# For Github downloads, you need to use the link to the 'Raw' file
urlfile="https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/us_daily.csv"
tp.us.daily <- read_csv(url(urlfile))
# View(tp.us.daily)
str(tp.us.daily) 
rm('urlfile')
# save the cases and deaths data
tp.us.daily.basic <- tp.us.daily[,c('date','death','positive')]
colnames(tp.us.daily.basic) <- c('date','deaths','cases')
write_csv(tp.us.daily.basic, sprintf("./Data/tp_covid19_data_%s.csv", Sys.Date()))

# prepare CV tracking project data
# deaths
tp.us.death.daily <- tp.us.daily[,c('date','death')] # grab only date and deaths
tp.us.death.daily <- tp.us.death.daily[!is.na(tp.us.death.daily$death),] # remove rows with deaths=NA
# tp.us.death.daily <- cbind(tp.us.death.daily[,1],(tp.us.death.daily$date-min(tp.us.death.daily$date)),tp.us.death.daily[,2]) # create a field for days since first deaths
tp.us.death.daily <- cbind(tp.us.death.daily[,1]
                           ,as.numeric(as.Date(as.character(tp.us.death.daily$date),format="%Y%m%d")-min(as.Date(as.character(tp.us.death.daily$date),format="%Y%m%d")))
                           ,tp.us.death.daily[,2]) # create a field for days since first deaths
colnames(tp.us.death.daily) <- c('date','days','deaths')
head(tp.us.death.daily) # have a look

# cases
tp.us.case.daily <- tp.us.daily[,c('date','positive')] # grab only date and cases
tp.us.case.daily <- tp.us.case.daily[!is.na(tp.us.case.daily$positive),] # remove rows with cases=NA
tp.us.case.daily <- cbind(tp.us.case.daily[,1]
                          ,as.numeric(as.Date(as.character(tp.us.death.daily$date),format="%Y%m%d")-min(as.Date(as.character(tp.us.death.daily$date),format="%Y%m%d")))
                          ,tp.us.case.daily[,2]) # create a field for days since first cases
colnames(tp.us.case.daily) <- c('date','days','cases')
head(tp.us.case.daily) # have a look

# data from Johns Hopkins via R script 'get_coronavirus_data.R'
file <- sprintf("./Data/jh_covid19_data_%s.csv", Sys.Date())
jh.cv19 <- read_csv(file)
# View(jh.cv19)

# prepare JH data
# deaths
jh.us.death.daily <- jh.cv19[which(jh.cv19$iso3c=='USA'),c('date','deaths')] # grab only date and deaths
jh.us.death.daily <- jh.us.death.daily[!(jh.us.death.daily$deaths==0),] # remove rows with deaths=0
jh.us.death.daily <- cbind(jh.us.death.daily[,1],as.numeric(jh.us.death.daily$date-min(jh.us.death.daily$date)),jh.us.death.daily[,2])
colnames(jh.us.death.daily) <- c('date','days','deaths')
head(jh.us.death.daily)
# cases
jh.us.case.daily <- jh.cv19[which(jh.cv19$iso3c=='USA'),c('date','confirmed')] # grab only date and cases
jh.us.case.daily <- jh.us.case.daily[!(jh.us.case.daily$confirmed==0),] # remove rows with cases=0
jh.us.case.daily <- cbind(jh.us.case.daily[,1],as.numeric(jh.us.case.daily$date-min(jh.us.case.daily$date)),jh.us.case.daily[,2])
colnames(jh.us.case.daily) <- c('date','days','cases')
head(jh.us.case.daily)


#### Functions for exponential and logistic fits ####

# exponential fit
expFit <- function (data.df,y.var,x.var) {
  
  y <- data.df[,y.var]
  x <- data.df[,x.var]
  
  # exponential model: y = ae^(bx) + c  >>  log(y-c) = log(a) + bx 
  
  # select an approximate c, since c must be lower than min(y), and greater than zero
  c.0 <- min(y) * 0.5
  
  # estimate the rest parameters using a linear model
  model.0 <- lm(log(y - c.0) ~ x)  
  a.0 <- exp(coef(model.0)[1])
  b.0 <- coef(model.0)[2]
  
  # starting parameters
  start <- list(a = a.0, b = b.0, c = c.0)

  # run the model
  model <- nls(y ~ a * exp(b * x) + c, start = start)

  # parameters
  coeffs <- summary(model)$coefficients
  a.est <- coeffs[1,1]
  b.est <- coeffs[2,1]
  c.est <- coeffs[3,1]
  
  # residuals and predictions
  res <- as.vector(residuals(model))
  pred <- as.vector(predict(model))
  
  # plot
  plot(x, y, pch=20, cex=1.5)
  lines(x,pred, col='skyblue', lwd=3)
  
  ret <- list(
    a = a.est
    ,b = b.est
    ,c = c.est
    ,residuals = res
    ,predictions = pred
  )
  
  return(ret)
  
}

# logistic fit function
logFit <- function (data.df,y.var,x.var) {
  
  y <- data.df[,y.var]
  x <- data.df[,x.var]
  
  # logistic model: y = asymptote / (1 + exp((xmid-x)/scale) )
  # a = asymptote
  # b = xmid
  # c = scale

  # fit model
  # at this point we know reasonable starting values, given that we are before the inflection point in the epidemic
  model <- nls(y ~ SSlogis(x, a, b, c), start = c(a=2*max(y), b = max(x), c = 1))
  
  # parameters
  coeffs <- summary(model)$coefficients
  a.est <- coeffs[1,1]
  b.est <- coeffs[2,1]
  c.est <- coeffs[3,1]
  
  # residuals and predictions
  res <- as.vector(residuals(model))
  pred <- as.vector(predict(model))
  
  # plot
  plot(x, y, pch=20, cex=1.5)
  lines(x, pred, col='skyblue', lwd=3)
  
  ret <- list(
    asymptote = a.est
    ,xmid = b.est
    ,scale = c.est
    ,residuals = res
    ,predictions = pred
  )
  
  return(ret)
  
}


#### exponential fits of deaths and cases ####

# TP data
# deaths
tp.expMod.death <- expFit(tp.us.death.daily,'deaths','days')
tp.expMod.death
plot(tp.us.death.daily$days, tp.us.death.daily$deaths, pch=20, cex=1.5, xlab='Days after first case', ylab='Cases')
lines(tp.us.death.daily$days, tp.expMod.death$predictions, col='skyblue', lwd=3)

# cases
tp.expMod.case <- expFit(tp.us.case.daily,'cases','days')
tp.expMod.case
plot(tp.us.case.daily$days, tp.us.case.daily$cases, pch=20, cex=1.5, xlab='Days after first case', ylab='Cases')
lines(tp.us.case.daily$days, tp.expMod.case$predictions, col='skyblue', lwd=3)

# JH data
# deaths
jh.expMod.death <- expFit(jh.us.death.daily,'deaths','days')
jh.expMod.death
plot(jh.us.death.daily$days, jh.us.death.daily$deaths, pch=20, cex=1.5, xlab='Days after first case', ylab='Cases')
lines(jh.us.death.daily$days, jh.expMod.death$predictions, col='skyblue', lwd=3)

# cases
jh.expMod.case <- expFit(jh.us.case.daily,'cases','days')
jh.expMod.case
plot(jh.us.case.daily$days, jh.us.case.daily$cases, pch=20, cex=1.5, xlab='Days after first case', ylab='Cases')
lines(jh.us.case.daily$days, jh.expMod.case$predictions, col='skyblue', lwd=3)


#### logistic fits of deaths and cases ####

# TP data
# deaths
tp.logMod.death <- logFit(tp.us.death.daily,'deaths','days')
tp.logMod.death
plot(tp.us.death.daily$days, tp.us.death.daily$deaths, pch=20, cex=1.5, xlab='Days after first case', ylab='Cases')
lines(tp.us.death.daily$days, tp.logMod.death$predictions, col='skyblue', lwd=3)

a <- tp.logMod.death$asymptote
b <- tp.logMod.death$xmid
c <- tp.logMod.death$scale

x.pred <- seq(0,2*b,1)
y.pred <- a / (1 + exp((b-x.pred)/c))
plot(x.pred,y.pred,type="l",col="skyblue")
points(tp.us.death.daily$days, tp.us.death.daily$deaths, pch=20, cex=1.5)

# cases
tp.logMod.case <- logFit(tp.us.case.daily,'cases','days')
tp.logMod.case
plot(tp.us.case.daily$days, tp.us.case.daily$cases, pch=20, cex=1.5, xlab='Days after first case', ylab='Cases')
lines(tp.us.case.daily$days, tp.logMod.case$predictions, col='skyblue', lwd=3)

a <- tp.logMod.case$asymptote
b <- tp.logMod.case$xmid
c <- tp.logMod.case$scale

x.pred <- seq(0,2*b,1)
y.pred <- a / (1 + exp((b-x.pred)/c))
plot(x.pred,y.pred,type="l",col="skyblue")
points(tp.us.case.daily$days, tp.us.case.daily$cases, pch=20, cex=1.5)

rm(list=c('a','b','c','x.pred','y.pred'))

# residual comparisons: exponential model / logistic model
# deaths, no difference >> still in exponential phase
sum(abs(tp.expMod.death$residuals))/sum(abs(tp.logMod.death$residuals))
sum(abs(tp.expMod.death$residuals))
sum(abs(tp.logMod.death$residuals))
# cases, logistic is better >> maybe not exponential anymore 
sum(abs(tp.expMod.case$residuals))/sum(abs(tp.logMod.case$residuals))
sum(abs(tp.expMod.case$residuals))
sum(abs(tp.logMod.case$residuals))

# JH data
# deaths
jh.logMod.death <- logFit(jh.us.death.daily,'deaths','days')
jh.logMod.death
plot(jh.us.death.daily$days, jh.us.death.daily$deaths, pch=20, cex=1.5, xlab='Days after first case', ylab='Cases')
lines(jh.us.death.daily$days, jh.logMod.death$predictions, col='skyblue', lwd=3)

a <- jh.logMod.death$asymptote
b <- jh.logMod.death$xmid
c <- jh.logMod.death$scale

x.pred <- seq(0,2*b,1)
y.pred <- a / (1 + exp((b-x.pred)/c))
plot(x.pred,y.pred,type="l",col="skyblue")
points(jh.us.death.daily$days, jh.us.death.daily$deaths, pch=20, cex=1.5)

# cases
jh.logMod.case <- logFit(jh.us.case.daily,'cases','days')
jh.logMod.case
plot(jh.us.case.daily$days, jh.us.case.daily$cases, pch=20, cex=1.5, xlab='Days after first case', ylab='Cases')
lines(jh.us.case.daily$days, jh.logMod.case$predictions, col='skyblue', lwd=3)

a <- jh.logMod.case$asymptote
b <- jh.logMod.case$xmid
c <- jh.logMod.case$scale

x.pred <- seq(0,2*b,1)
y.pred <- a / (1 + exp((b-x.pred)/c))
plot(x.pred,y.pred,type="l",col="skyblue")
points(jh.us.case.daily$days, jh.us.case.daily$cases, pch=20, cex=1.5)

rm(list=c('a','b','c','x.pred','y.pred'))

# residual comparisons: exponential model / logistic model
# deaths
sum(abs(jh.expMod.death$residuals))/sum(abs(jh.logMod.death$residuals))
sum(abs(jh.expMod.death$residuals))
sum(abs(jh.logMod.death$residuals))
# cases
sum(abs(jh.expMod.case$residuals))/sum(abs(jh.logMod.case$residuals))
sum(abs(jh.expMod.case$residuals))
sum(abs(jh.logMod.case$residuals))

