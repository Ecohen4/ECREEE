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
dat<-read.csv(file="WINDFARM_VXE_2013.csv", header=TRUE)

# preview the data and data structure
head(dat)
str(dat)

# you can change the column names to be more succint (avoid spaces, use _ or . to seperate words)
# names(dat)<-c("date_time", "mean_wind_mps", "min_wind_mps", "max_wind_mps", "cum_energy_delivered_kwh", "energy_sentout_10min_kwh")

# check dimensions of the data
dim(dat) # 52560 observations (rows) of 6 variables (columns)

# check for missing values
sum(is.na(dat)) # 68 missing values

# visually inspect rows containing missing values
look<-which(is.na(dat), arr.ind=TRUE) # row and column index of missing values
dat[look[,1], ] # use the row index to show rows of `dat` containing  missing values

# omit rows containing missing values
# (Note: only 68 of 52,560 records contain missing values --> OK to omit them)
dat<-na.omit(dat)

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

# double check for missing values
sum(is.na(dat))  # zero NAs --> Proceed with analysis!

# check for outliers
hist(dat$mean_wind_mps)  # looks good
hist(dat$energy_sentout_10min_kwh) # looks funny...

# remove outliers: use the subset() function
dat<-subset(dat, dat$energy_sentout_10min_kwh > 0) # keep records associated with positive energy_sentout values only

# now the histogram looks better...
hist(dat$energy_sentout_10min_kwh)

# plot wind speed vs power output
plot(dat$mean_wind_mps, dat$energy_sentout_10min_kwh)

# windspeed vs power output looks okay, but there are erroneous wind speed values...
# how could so much power be produced at such low wind speeds?  Assume erroneous.

# subset data to windspeed cut-in/cut-out set points
dat<-subset(dat, dat$min_wind_mps > 3)
dat<-subset(dat, dat$max_wind_mps < 25)

# plot the data again... Looks much better now.
plot(dat$mean_wind_mps, dat$energy_sentout_10min_kwh)

# Now that we have 'cleaned' the data, we can estimate the power curve.
# We want to maximize the power-windspeed function.  To do so, we find the maximum power output at each windspeed. Since we have continuous data, we need to bin the data into discrete segments first.
# choose bin range based on range of the data, and bin size based on the number of observations available.  Since we have lots of data, we can use small bins.
range(dat$mean_wind_mps)
bins<-seq(3, 20, by=0.2)

# summarize the data by wind bin
# NOTE: the ddply function doesn't work with POSIX objects... because POSIX objects are lists at their core.
# convert POSIX to character string...
# but first, let's extract the hour component from the POSIX object (we'll use this later).
dat$hour <- cut(dat$date_time, breaks = "hour")
# now convert POSIX to character string
dat$date_time<-as.character(dat$date_time)

dat$wind.bin<-cut(x=dat$mean_wind_mps, breaks=bins)
power.curve<-ddply(dat, .(wind.bin), summarize,
                   max.kw=max(energy_sentout_10min_kwh)*6,
                   near.max.kw=quantile(energy_sentout_10min_kwh, probs=0.99)*6,
                   median.kw=quantile(energy_sentout_10min_kwh, probs=0.50)*6,
                   near.min.kw=quantile(energy_sentout_10min_kwh, probs=0.01)*6,
                   min.kw=quantile(energy_sentout_10min_kwh, probs=0.0)*6
)

# 'melt' the data frame to make one long data.frame
test<-melt(power.curve, id.var="wind.bin")

# plot the data
ggplot(test, aes(x=wind.bin, y=value, group=variable, colour=variable, linetype=variable)) +
  geom_line() +
  scale_y_continuous(name="Power Gen (KW)") +
  scale_x_discrete(name="WindSpeed (mps)", breaks=levels(dat$wind.bin)[seq(1, length(levels(dat$wind.bin)),by=5)]) +
  labs(title="Power Curve for Isla Santiago Wind Farm") +
  theme_classic()

# Now we can re-assign the power output for all observations in a given bin to the max energy ouput observed for that bin.
# This yields the *theoretical* energy output assuming all energy can be accepted by the grid.
# let's choose the 99 percentile (near max) power output at a given windspeed to define the power curve.
dat<-merge(dat, subset(power.curve, select=c("wind.bin","near.max.kw")), by="wind.bin")
plot(x=dat$mean_wind_mps, y=dat$near.max.kw)

# finally, let's aggregate the data and compute the total energy output and number of hours in the dataset.

# aggregate from 10-min to hourly
hourly <- ddply(dat, .(hour), summarize,
                mean_wind_mps=mean(mean_wind_mps),
                energy_sentout_kwh=sum(energy_sentout_10min_kwh),
                potential_power_kw=mean(near.max.kw))

# Now we can compute the capacity factor:
cf=sum(hourly$potential_power_kw)/(850*7*dim(hourly)[1])
cf

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
# The operational capacity factor is 40%
# the POTENTIAL capacity factor given the available wind resource is nearly 70%!

# fit a distribution to the data
library(fitdistrplus)
descdist(dat$mean_wind_mps) # heuristic

# based on heuristic, and knowledge of the system, fit a weibull distribution
weibull.fit<-fitdist(dat$mean_wind_mps, distr="weibull")
summary(weibull.fit)
plot(weibull.fit, demp=TRUE)

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