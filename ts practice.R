##https://www.r-exercises.com/2018/06/06/intro-to-time-series-analysis-part-1/



boxplot(gold)$out
OutVals = boxplot(gold, plot=FALSE)$out
outvals

# Exercise 1
# 
# Install the Forecast package. The first step in any analysis is to see the data and use the auto-plot to plot the gold data-set available in the forecast package.

library(forecast)
require(forecast)

autoplot(gold)
data(gold)
# Exercise 2
# Find the outlier in the gold data. Find how far it is from the median.

summary(gold)
gold_out = 593.7 - 403.2
which.max(gold)

 
# Exercise 3
# 
# Use ggseasonalplot to plot the gas data. See how each year stacks with each other.
data(gas)
autoplot(gas)
seasonplot(gas)
?seasonplot
ggseasonplot(forecast::gas)
gas
library(ggplot2)

# 
# Exercise 4
# 
# Now I want to plot the gas data for the year 1990 and after. How do I achieve that?

ggseasonplot(window(gas,start=1990))

ts.plot(gas[470:476])

#   
#   Exercise 5
# 
# ggseasonalplot can be used to see seasonal plots as polar coordinates. Plot the same data as above, but use
# 
# polar coordinates instead.

ggseasonplot(window(gas,start=1990), polar = TRUE)


# 
# Exercise 6
# How can I see a monthly time series of the gas price?
#   Hint: use monthplot or ggsubseriesplot.

monthplot(gas)
ggsubseriesplot(gas)

# Exercise 7
# 
# For a time series, we can find the dominant frequency of that time series and use it for our analysis on how to find the frequency of gas data (assume that it is a long /big data and you don't have a clue about the frequency.)

frequency(gas)
forecast::findfrequency(gas)


# Exercise 1
# 
# Load the AirPassengers data. Check its class and see the start and end of the series.

data("AirPassengers")

# 
# Exercise 2
# Check the cycle of the Time-Series AirPassengers.

cycle(AirPassengers)

 
# Exercise 3
# 
# Create a lag-plot using the gglag-plot from the forecast package. Check how the relationship changes as the lag increases.

library(forecast)
gglagplot(AirPassengers, lags=10)

# 
# Exercise 4
# 
# Also, plot the correlation for each of the lags. You can see when the lag is above 6, the correlation drops, climbs up in 12 and again drops in 18.

ggAcf(AirPassengers)

# Exercise 5
# 
# Plot the histogram of the AirPassengers using a gghistogram from the forecast.
# 
# Exercise 6

forecast::gghistogram(AirPassengers)

# 
# Use tsdisplay to plot auto-correlation, time-series and partial auto-correlation together in the same plot.

tsdisplay(AirPassengers)


# 
# Exercise 7
# 
# 

Find the outliers in the time-series.

which.max(AirPassengers)
tsoutliers(AirPassengers)
