# Use this R script to analyze wind speed and wind generation data:
# import raw data from .csv
# check for missing values
# remove missing and/or erroneous values
# plot the 'clean' data
# fit an appropriate statistical distribution to the 'clean' data
# estimate the operational power curve
# calculate the operational capacity factor

# let's get started!
# load libraries and set global options
# Note: avoid using factors until you become more familiar with object classes.
options(stringsAsFactors=FALSE)
library(plyr)
library(ggplot2)
library(reshape2)

# import raw data
# dat<-read.csv(file="WINDFARM_VXE_2013.csv", header=TRUE)
dat<-read.csv(file="WIND_VXE_2013.csv", header=TRUE)

# preview the data and data structure
head(dat)
str(dat)

# assign new column names to be more succint (avoid spaces, use _ or . to seperate words)
new.names<-c("date_time","T1_Possible_Power","T2_Possible_Power","T3_Possible_Power","T4_Possible_Power","T5_Possible_Power","T6_Possible_Power","T7_Possible_Power","T1_Total_Active_Power","T2_Total_Active_Power","T3_Total_Active_Power","T4_Total_Active_Power","T5_Total_Active_Power","T6_Total_Active_Power","T7_Total_Active_Power","mean_wind_mps", "min_wind_mps", "max_wind_mps", "cum_energy_delivered_kwh")

# make sure the new names lineup properly with the ones they're replacing...
cbind(names(dat), new.names)

# if yes, replace column names with new names
names(dat)<-new.names

# select the columns we need for various analyses
Cumulative<-subset(dat, select=c(1,19))
Possible<-subset(dat, select=1:8)
Active<-subset(dat, select=c(1,9:15))
Wind<-subset(dat, select=c(1,16:18))

# for the time being, we only need the cumulative energy and wind data...
dat<-merge(Wind, Cumulative, by="date_time")

# check dimensions of the data
dim(dat) # 52560 observations (rows) of 5 variables (columns)

# check for missing values
sum(is.na(dat)) # 68 NA

# row and column index of missing values
look<-which(is.na(dat), arr.ind=TRUE)

# visually inspect rows containing missing values
dat[look[,1], ]

# check distribution of NAs, column-wise
hist(look[,2],xlab="Column", freq=TRUE, breaks=1:dim(dat)[2], main="Column-wise distribution of missing values")

# omit rows containing missing values
dat<-na.omit(dat)

# # visually inspect the data
# # CAUTION: SLOW TO PLOT BECAUSE OF LARGE N
# Cumulative<-melt(Cumulative, id.vars="date_time")
# Possible<-melt(Possible, id.vars="date_time")
# Active<-melt(Active, id.vars="date_time")
# Wind<-melt(Wind, id.vars="date_time")
# ggplot(Cumulative, aes(x=date_time, y=value, group=variable, colour=variable)) + geom_line()
# ggplot(Possible, aes(x=date_time, y=value, group=variable, colour=variable)) + geom_line()
# ggplot(Active, aes(x=date_time, y=value, group=variable, colour=variable)) + geom_line()
# ggplot(Wind, aes(x=date_time, y=value, group=variable, colour=variable)) + geom_line()

# compute energy sentout in each timeblock (10min)
n<-length(dat$cum_energy_delivered_kwh)
a<-dat$cum_energy_delivered_kwh[1:n-1]
b<-dat$cum_energy_delivered_kwh[2:n]
diff<-b-a
dat$energy_sentout_10min_kwh<-c(diff,0)

# check if we introduced any NA's
sum(is.na(dat$energy_sentout_10min_kwh))

# check for outliers in wind data
summary(dat$mean_wind_mps)
hist(dat$mean_wind_mps)  # looks good

# check for outliers in energy data
summary(dat$energy_sentout_10min_kwh)
hist(dat$energy_sentout_10min_kwh) # looks funny...

# remove erroneous values:
# remove negative energy values (impossible)
dat<-subset(dat, dat$energy_sentout_10min_kwh > 0)

# remove values beyond what's possible given installed capacity
cap<-850*7 #KW
cap_10min_kwh<-cap*(10/60) #KWh
dat<-subset(dat, dat$energy_sentout_10min_kwh < cap_10min_kwh)

# # remove outliers: retain 1st to 99th percentiles
# range<-quantile(dat$energy_sentout_10min_kwh, probs=c(0.01,0.99))
# dat<-subset(dat, dat$energy_sentout_10min_kwh > range[1])
# dat<-subset(dat, dat$energy_sentout_10min_kwh < range[2])

# now the histogram looks better...
summary(dat$energy_sentout_10min_kwh)
hist(dat$energy_sentout_10min_kwh)

# plot wind speed vs power output
plot(dat$mean_wind_mps, dat$energy_sentout_10min_kwh)

# windspeed vs power output looks okay, but there are erroneous wind speed values...
# how could so much power be produced at such low wind speeds?  Assume erroneous.
# subset data to windspeed cut-in/cut-out set points
# dat<-subset(dat, dat$mean_wind_mps > 0) # TEST

dat<-subset(dat, dat$min_wind_mps > 3)
dat<-subset(dat, dat$max_wind_mps < 25)

# plot the data again... Looks much better now.
plot(dat$mean_wind_mps, dat$energy_sentout_10min_kwh)

# Now that we have 'cleaned' the data, we can estimate the distributional properties of the wind resource and estimate the power curve.
# fit a distribution to the wind data
library(fitdistrplus)
descdist(dat$mean_wind_mps) # heuristic

# based on heuristic, and knowledge of the system, fit a weibull distribution
weibull.fit<-fitdist(dat$mean_wind_mps, distr="weibull")
summary(weibull.fit)
plot(weibull.fit, demp=TRUE)

# Estimate the power curve. To do so, we find the maximum power output at each windspeed. Since we have continuous data, we need to bin the data into discrete segments first.
# choose bin range based on range of the data, and bin size based on the number of observations available.  Since we have lots of data, we can use small bins.
range(dat$mean_wind_mps)
bins<-seq(3, 20, by=0.2)

# summarize the data by wind bin
# NOTE: the ddply function doesn't work with POSIX objects... because POSIX objects are really lists...
# use factor or character string representations of date_time when summarizing with ddply()
dat$date_time<-as.character(dat$date_time)
dat$wind.bin<-cut(x=dat$mean_wind_mps, breaks=bins)
power.curve<-ddply(dat, .(wind.bin), summarize, max.kwh=max(energy_sentout_10min_kwh), max.kw=max(energy_sentout_10min_kwh)*6)
#ddply(dat, .(wind.bin), function(x) quantile(x$energy_sentout_10min_kwh))


# Now we can re-assign the power output for all observations in a given bin to the max energy ouput observed for that bin.
# This yields the *theoretical* energy output assuming all energy can be accepted by the grid.
# let's choose the 99 percentile (near max) power output at a given windspeed to define the power curve.
dat<-merge(dat, power.curve, by="wind.bin")
plot(x=dat$mean_wind_mps, y=dat$max.kw)

# finally, let's aggregate the data and compute HOURLY energy output and number of hours in the dataset.
# to do so, we must convert date_time into a POSIXlt or POSIXct date-time class.
# POSIX date-time objects represent calendar dates and times to the nearest second.
# At their core, POSIX objects are lists containing date-time components.

# To convert a character representation of date-time into a POSIX object, each entry must conform to a standard, unambiguous format.
# In the raw data, midnight values are missing the hour and minute. Fix these:
get<-which(is.na(as.POSIXlt(dat$date_time, format="%m/%d/%y %H:%M"))) # return the row index of date_time values that cannot be coerced to a POSIX object
dat$date_time[get]<-paste(dat$date_time[get], "00:00", sep=" ") # ammend those with the missing hour:time info.

# check if the modified date_time character string can be coerced to a POSIX object without generating NA values.
sum(is.na(as.POSIXlt(dat$date_time, format="%m/%d/%y %H:%M"))) # zero NAs

# covert modified date_time character string to POSIX
dat$date_time<-as.POSIXlt(dat$date_time, format="%m/%d/%y %H:%M")
dat$hour <- cut(dat$date_time, breaks = "hour")

# convert moditifed date_time POSIX back to charcater for compatability with ddply()
dat$date_time<-as.character(dat$date_time)

# aggregate from 10-min to hourly
hourly <- ddply(dat, .(hour), summarize,
                mean_wind_mps=mean(mean_wind_mps),
                actual_energy_sentout_kwh=sum(energy_sentout_10min_kwh),
                possible_energy_sentout_kwh=sum(max.kwh),
                possible_power_kw=mean(max.kw))

# Now we can compute the capacity factor:
actual.cf<-sum(hourly$actual_energy_sentout_kwh)/(cap*dim(hourly)[1])
actual.cf
best.cf<-sum(hourly$possible_energy_sentout_kwh)/(cap*dim(hourly)[1])
best.cf
alt.best.cf<-mean(hourly$possible_power_kw)/cap
alt.best.cf

## compare with the OBSERVED capacity factor
# 850 KW x 7 turbines rated capacity
# 8760 hours/year

# KWh sentout over one year
# simply subtract last cumulative value from the first....
n<-length(dat$cum_energy_delivered_kwh)
cum.sum<-dat$cum_energy_delivered_kwh[n]-dat$cum_energy_delivered_kwh[1]

# ... But beware, the counter is reset each time it reaches 10 million!
dat$date_time<-as.POSIXlt(dat$date_time)
plot(y=dat$cum_energy_delivered_kwh, x=dat$date_time)

# add the missing 2x 10 million back in...
cum.sum<-cum.sum+(2*10^7)

# potential energy given 7 turbines with 850 KW rated capacity over one year
pe<-(850*7)*(365*24)
cum.sum/pe
# The operational capacity factor is between 35% and 42%
# the POTENTIAL capacity factor given the available wind resource is between 72% and 77%!

# fit a local-polynomial distribution to the power curve
library(locfit)
mod<-locfit(energy_sentout_10min_kwh ~ mean_wind_mps, data=dat)
summary(mod)
plot(mod)
points(y=dat$energy_sentout_10min_kwh, x=dat$mean_wind_mps, cex=0.1, pch=20, col="red")

# predict energy generation given windspeeds...
# assume a random distribution of windspeeds with mean 10 and sd=5. Generate one-year of hourly windspeeds, with random noise.
expected.wind=rnorm(8760, mean=10, sd=5)

# estimate generation given expected windspeeds.
expected.gen<-predict(mod, newdata=expected.wind)

# plot the new (expected) power curve
plot(expected.gen ~ expected.wind)

# convert date_time character strings into POSIXlt or POSIXct date-time classes.
# POSIX date-time objects represent calendar dates and times to the nearest second.
# At their core, POSIX objects are lists containing date-time components.

# To convert a character representation of date-time into a POSIX object, each entry must conform to a standard, unambiguous format.
# In the raw data, midnight values are missing the hour and minute. Fix these:
get<-which(is.na(as.POSIXlt(dat$date_time, format="%m/%d/%y %H:%M"))) # return the row index of date_time values that cannot be coerced to a POSIX object
dat$date_time[get]<-paste(dat$date_time[get], "00:00", sep=" ") # ammend those with the missing hour:time info.

# check if the modified date_time character string can be coerced to a POSIX object without generating NA values.
sum(is.na(as.POSIXlt(dat$date_time, format="%m/%d/%y %H:%M"))) # zero NAs

# covert modified date_time character string to POSIX
dat$date_time<-as.POSIXlt(dat$date_time, format="%m/%d/%y %H:%M")
dat$hour <- cut(dat$date_time, breaks = "hour")