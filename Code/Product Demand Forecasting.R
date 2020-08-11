library(ggplot2)
library(forecast)
library(tseries)
library(dplyr)
library(imputeTS)
library(DMwR)
library(lubridate)

options(scipen = 999)

setwd("C:/Users/Admin/Documents")

#LOAD DATA
data = read.csv('historical_product_demand.csv', header=TRUE, stringsAsFactors=FALSE)
summary(data)
str(data)
head(data)
nrow(data)
ncol(data)


# Check for any missing values:
sapply(data, function(x) sum(is.na(x)))



# is.numeric(data$Order_Demand)
# When converting Order_Demand to numeric, we are getting missing values.
# Exploring further resulted in the numbers that are stored in the format of "(100)"



# Cleaning the data:
data$Order_Demand = gsub("[()]", "", data$Order_Demand)
data$Order_Demand = as.numeric(as.character(data$Order_Demand))
sapply(data, function(x) sum(is.na(x)))

data %>% group_by(Product_Code) %>% summarise(demand = sum(Order_Demand)) %>% arrange(desc(demand))



# The product "Product_1359" is of very high demand over the entire period.
# Lets go ahead in forecasting the demand for this product
product_demand <- data[which(data$Product_Code == 'Product_1359'), ]
head(product_demand)
str(product_demand)
sapply(product_demand, function(x) sum(is.na(x)))
# There are no missing values for the product which are forecasting for.




# CONVERTING DATE FROM STRING TO DATE FORMAT 
product_demand$Date = as.Date(product_demand$Date, "%Y/%m/%d")
str(product_demand)


# CHECK FOR DUPLICATES
# There could be multiple orders on a day. Hence no need to bother about the duplicates:
table(duplicated(product_demand))

###### DAILY DATA ######

# Get the Date and Order_Demand fields, group the data and get the demand at a day level:
daily_data <- product_demand[, c('Date', 'Order_Demand')]
daily_data = data.frame(daily_data %>% group_by(Date) %>% summarise(total_demand = sum(Order_Demand)))



# Add the missing dates to the timeseries:
full_dates <- seq(min(daily_data$Date), max(daily_data$Date), by = "1 day")
full_dates <- data.frame(Date = full_dates)
daily_data <- merge(full_dates, daily_data, by.x = 'Date',  all.x = TRUE)
tail(daily_data)



# Missing value imputation:
daily_data <- na.interpolation(daily_data)
head(daily_data)


# last 6 months data is moved to test set:
train <- daily_data[which(daily_data$Date <= as.Date('2016-06-30')), ]
test <- daily_data[which(daily_data$Date > as.Date('2016-06-30')), ]


tail(train)
tail(test)

ggplot(train, aes(Date, total_demand)) + 
  geom_line() + 
  scale_x_date('Day') +
  ylab("Product Orders") + 
  ggtitle("Daily Product Demand")




# Convert into Timeseries, Clean and plot again
demand_ts = ts(train[, c('total_demand')])
train$clean_demand = tsclean(demand_ts)
ggplot() +
  geom_line(data = train, aes(x = Date, y = clean_demand)) + 
  ylab('Cleaned Order Demand')




# As we are forecasting the demand of a product for a manufacturing company, 
# It makes much sense to forecast the monthly sales of the product.
train$demand_ma30 = ma(train$clean_demand, order = 30)
ggplot() +
  geom_line(data = train, aes(x = Date, y = clean_demand, colour = "Demand")) +
  geom_line(data = train, aes(x = Date, y = demand_ma30, colour = "Monthly Moving Average")) + 
  ylab('Order Demand')


demand_ma30 = ts(na.omit(train$demand_ma30), frequency = 30)
decomp = stl(demand_ma30, s.window = "periodic")
plot(decomp)



# De-Seasonlise the decomposed data which is required for time-series modeling.
deseasonal_dmd <- seasadj(decomp)
plot(deseasonal_dmd)



# Checking for stationarity using Dicky-Fuller Test: 
adf.test(demand_ma30, alternative = "stationary")


# Plotting ACF & PACF:
par(mfrow=c(1,2))
Acf(demand_ma30, main='ACF of Monthly Demand')
Pacf(demand_ma30, main='PACF of Monthly Demand')
plot(demand_ma30)



# Perform the differencing to remove existing Stationarity
demand_ma30_d1 = diff(deseasonal_dmd, differences = 1)
plot(demand_ma30_d1)



adf.test(demand_ma30_d1, alternative = "stationary")



# Plotting ACF and PACF for Differenced Series:
par(mfrow=c(1,2))
Acf(demand_ma30_d1, main='ACF for Differenced Series')
Pacf(demand_ma30_d1, main='PACF for Differenced Series')



# Fitting an Auto-ARIMA Model:
fit <- auto.arima(deseasonal_dmd, seasonal=FALSE)
fit
tsdisplay(residuals(fit), lag.max=30, main='Auto-ARIMA Model Residuals')



######################################## MONTHLY DATA ########################################################

monthly_data <- product_demand[, c('Date', 'Order_Demand')]
monthly_data = data.frame(monthly_data %>% group_by(Date) %>% summarise(total_demand = sum(Order_Demand)))
min(monthly_data$Date) # "2012-01-05"
max(monthly_data$Date) # "2017-01-06"

# Add the missing dates to the timeseries:
full_dates <- seq(as.Date("2012-02-01"), as.Date("2016-12-31"), by = "1 day")
full_dates <- data.frame(Date = full_dates)
monthly_data <- merge(full_dates, monthly_data, by.x = 'Date',  all.x = TRUE)
head(monthly_data)


monthly_data <- na.interpolation(monthly_data)
monthly_data = data.frame(monthly_data %>% 
                            group_by(Month = floor_date(Date, "month")) %>% 
                            summarise(total_demand = sum(total_demand)))


head(monthly_data)



# last 6 months data is moved to test set:
monthly_train <- monthly_data[which(monthly_data$Month < as.Date('2016-08-01')), ]
monthly_test <- monthly_data[which(monthly_data$Month >= as.Date('2016-07-01')), ]
tail(monthly_train)
tail(monthly_test)

ggplot(monthly_train, aes(Month, total_demand)) + 
  geom_line() + 
  scale_x_date('Months') +
  ylab("Product Orders") + 
  ggtitle("Montly Product Demand")


monthly_demand_ts = ts(na.omit(monthly_train$total_demand), frequency = 12)
monthly_decomp = stl(monthly_demand_ts, s.window = "periodic")
monthly_decomp
plot(monthly_decomp)



# De-Seasonlise the decomposed data which is required for time-series modeling.
monthly_deseasonal_dmd <- seasadj(monthly_decomp)
plot(monthly_deseasonal_dmd)



# Checking for stationarity using Dicky-Fuller Test: 
adf.test(monthly_demand_ts, alternative = "stationary")



# Plotting ACF & PACF:
par(mfrow=c(1,2))
Acf(monthly_demand_ts, main='ACF of Monthly Demand')
Pacf(monthly_demand_ts, main='PACF of Monthly Demand')
plot(monthly_demand_ts)



# Perform the differencing to remove existing Stationarity
monthly_demand_d1 = diff(monthly_deseasonal_dmd, differences = 1)
plot(monthly_demand_d1)



adf.test(monthly_demand_d1, alternative = "stationary")



# Plotting ACF and PACF for Differenced Series:
par(mfrow=c(1,2))
Acf(monthly_demand_d1, main='ACF for Differenced Series')
Pacf(monthly_demand_d1, main='PACF for Differenced Series')


# Fitting an Auto-ARIMA Model:
monthly_fit <- auto.arima(monthly_deseasonal_dmd, seasonal=FALSE)
monthly_fit
tsdisplay(residuals(monthly_fit), lag.max=30, main='Auto-ARIMA Model Residuals')

monthly_fit2 = arima(monthly_deseasonal_dmd, order=c(2,1,1))
monthly_fit2
tsdisplay(residuals(monthly_fit2), lag.max=15, main='Model Order (2,1,1) Residuals')




get_monthly_est <- function(daily_fc) {
  jul <- sum(daily_fc$mean[1:31])
  aug <- sum(daily_fc$mean[32:62])
  sept <- sum(daily_fc$mean[63:92])
  oct <- sum(daily_fc$mean[93:123])
  nov <- sum(daily_fc$mean[124:153])
  decm <- sum(daily_fc$mean[154:184])
  return(c(jul, aug, sept, oct, nov, decm))
}


auto_fit_daily = forecast(fit, h=190)
auto_fit_monthly_est <- get_monthly_est(auto_fit_daily)
regr.eval(monthly_test$total_demand[1:6], auto_fit_monthly_est)


monthly_fit <- forecast(monthly_fit, h=6)
regr.eval(monthly_test$total_demand[1:6], monthly_fit$mean)


monthly_fit2 <- forecast(monthly_fit2, h=6)
regr.eval(monthly_test$total_demand[1:6], monthly_fit2$mean)
