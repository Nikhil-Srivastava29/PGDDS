#-------------------------- Global Mart----------------------------#
# ----------------- Sales & Quantity Forecasting ------------------- #

###############################################################
# Section 1: Business Understanding
# Section 2: Data Understanding
# Section 3: Data Preparation & EDA
# Section 4: Model Building & Forecasting
################################################################

# SECTION 1: Business Understanding

# Total Number of Orders: 51290

# AIM of Case Study

# 1. find the 2 most profitable and consistently profitable segments.
# 2. forecast the Quantity and quantity for the next 6 months.

#----------------------------------------------------------------#

library(forecast)
library(tseries)
require(graphics)
library(dplyr)


# Loading the transaction level order CSV 
orders_df <- read.csv("Global Superstore.csv",stringsAsFactors=F)

# Data Understanding #51290 obs of 24 variables
str(orders_df)
summary(orders_df)
head(orders_df)
#------------------------------------#
# Section 3: Data Preparation & EDA

# Section 3.1: Data Preparation
#------------------------------------#

################################################################

# Section 3.1.1 - Analysis on NA values

################################################################

sapply(orders_df, function(x) sum(is.na(x))) 

# only Postal.Code has NA values. Since it is not a variable of interest hence ignoring the NA values


################################################################

# Section 3.1.2 - Data Preparation:
  # Converting Order Date variable into Date format
  # Creating Order month variable
  # Create 21 subsets based on 7 Market and 3 Segments
  # Aggregrate subsets on Monthly Quantity, Quantity and Profit 
################################################################

orders_df$Order.Date <-strptime(orders_df$Order.Date,"%d-%m-%Y")
orders_df$Month_Yr <- format(orders_df$Order.Date, "%Y-%m")
orders_df$Order.Date <- as.character(orders_df$Order.Date)
# Creating Subsets for Market and Segments

#1. Subset for Market Africa:

# 1.1 Segment Consumer
orders_Africa_Consumers <- subset(orders_df,(Market=="Africa"& Segment=="Consumer"))
# 1.2 Segment Corporate
orders_Africa_Corporate <- subset(orders_df,(Market=="Africa"& Segment=="Corporate"))
# 1.3 Segment Home Office
orders_Africa_HomeOffice <- subset(orders_df,(Market=="Africa"& Segment=="Home Office"))

#2. Subset for Market APAC:

# 2.1 Segment Consumer
orders_APAC_Consumers <- subset(orders_df,(Market=="APAC"& Segment=="Consumer"))
# 2.2 Segment Corporate
orders_APAC_Corporate <- subset(orders_df,(Market=="APAC"& Segment=="Corporate"))
# 2.3 Segment Home Office
orders_APAC_HomeOffice <- subset(orders_df,(Market=="APAC"& Segment=="Home Office"))

#3. Subset for Market Canada:

# 3.1 Segment Consumer
orders_Canada_Consumers <- subset(orders_df,(Market=="Canada"& Segment=="Consumer"))
# 3.2 Segment Corporate
orders_Canada_Corporate <- subset(orders_df,(Market=="Canada"& Segment=="Corporate"))
# 3.3 Segment Home Office
orders_Canada_HomeOffice <- subset(orders_df,(Market=="Canada"& Segment=="Home Office"))


#4. Subset for Market EMEA:

# 4.1 Segment Consumer
orders_EMEA_Consumers <- subset(orders_df,(Market=="EMEA"& Segment=="Consumer"))
# 4.2 Segment Corporate
orders_EMEA_Corporate <- subset(orders_df,(Market=="EMEA"& Segment=="Corporate"))
# 4.3 Segment Home Office
orders_EMEA_HomeOffice <- subset(orders_df,(Market=="EMEA"& Segment=="Home Office"))

#5. Subset for Market EU:

# 5.1 Segment Consumer
orders_EU_Consumers <- subset(orders_df,(Market=="EU"& Segment=="Consumer"))
# 5.2 Segment Corporate
orders_EU_Corporate <- subset(orders_df,(Market=="EU"& Segment=="Corporate"))
# 5.3 Segment Home Office
orders_EU_HomeOffice <- subset(orders_df,(Market=="EU"& Segment=="Home Office"))

#6. Subset for Market LATAM:

# 6.1 Segment Consumer
orders_LATAM_Consumers <- subset(orders_df,(Market=="LATAM"& Segment=="Consumer"))
# 6.2 Segment Corporate
orders_LATAM_Corporate <- subset(orders_df,(Market=="LATAM"& Segment=="Corporate"))
# 6.3 Segment Home Office
orders_LATAM_HomeOffice <- subset(orders_df,(Market=="LATAM"& Segment=="Home Office"))

#7. Subset for Market US:

# 7.1 Segment Consumer
orders_US_Consumers <- subset(orders_df,(Market=="US"& Segment=="Consumer"))
# 7.2 Segment Corporate
orders_US_Corporate <- subset(orders_df,(Market=="US"& Segment=="Corporate"))
# 7.3 Segment Home Office
orders_US_HomeOffice <- subset(orders_df,(Market=="US"& Segment=="Home Office"))


####################################
# Aggregating on Time -
####################################
aggr_fun <- function(df){
  aggr_df <-df %>%
    group_by(Month_Yr)%>%
    summarise(monthlyQuantity=sum(Quantity),monthlySales=sum(Sales),monthlyProfit=sum(Profit))
}

# 1. Market: Africa
# 1.1 Segment: consumers
Africa_Consumers_Aggr <- aggr_fun(orders_Africa_Consumers)
# 1.2 Segment: Corporate
Africa_Corporate_Aggr <- aggr_fun(orders_Africa_Corporate)
# 1.3 Segment: Home Office
Africa_HomeOffice_Aggr <- aggr_fun(orders_Africa_HomeOffice )

# 2. Market: APAC
# 2.1 Segment: consumers
APAC_Consumers_Aggr <- aggr_fun(orders_APAC_Consumers)
# 2.2 Segment: Corporate
APAC_Corporate_Aggr <- aggr_fun(orders_APAC_Corporate)
# 2.3 Segment: Home Office
APAC_HomeOffice_Aggr <- aggr_fun(orders_APAC_HomeOffice)

# 3. Market: Canada
# 3.1 Segment: consumers
Canada_Consumers_Aggr <- aggr_fun(orders_Canada_Consumers)
# 3.2 Segment: Corporate
Canada_Corporate_Aggr <- aggr_fun(orders_Canada_Corporate)
# 3.3 Segment: Home Office
Canada_HomeOffice_Aggr <- aggr_fun(orders_Canada_HomeOffice) 

# 4. Market: EMEA
# 4.1 Segment: consumers
EMEA_Consumers_Aggr <- aggr_fun(orders_EMEA_Consumers)
# 4.2 Segment: Corporate
EMEA_Corporate_Aggr <- aggr_fun(orders_EMEA_Corporate)
# 4.3 Segment: Home Office
EMEA_HomeOffice_Aggr <- aggr_fun(orders_EMEA_HomeOffice)
 
# 5. Market: EU
# 5.1 Segment: consumers
EU_Consumers_Aggr <- aggr_fun(orders_EU_Consumers)
# 5.2 Segment: Corporate
EU_Corporate_Aggr <- aggr_fun(orders_EU_Corporate)
# 5.3 Segment: Home Office
EU_HomeOffice_Aggr <- aggr_fun(orders_EU_HomeOffice)
 
# 6. Market: LATAM
# 6.1 Segment: consumers
LATAM_Consumers_Aggr <- aggr_fun(orders_LATAM_Consumers)
# 6.2 Segment: Corporate
LATAM_Corporate_Aggr <- aggr_fun(orders_LATAM_Corporate)
# 6.3 Segment: Home Office
LATAM_HomeOffice_Aggr <- aggr_fun(orders_LATAM_HomeOffice)

# 7. Market:
# 7.1 Segment: consumers
US_Consumers_Aggr <- aggr_fun(orders_US_Consumers)
# 7.2 Segment: Corporate
US_Corporate_Aggr <- aggr_fun(orders_US_Corporate)
# 7.3 Segment: Home Office
US_HomeOff_Aggr <- aggr_fun(orders_US_HomeOffice)

# Computing coefficient of variation of the Profit for 21 subsets

df.list <- list(Africa_Consumers_Aggr,Africa_Corporate_Aggr,Africa_HomeOffice_Aggr,APAC_Consumers_Aggr,APAC_Corporate_Aggr,
                APAC_HomeOffice_Aggr,Canada_Consumers_Aggr,Canada_Corporate_Aggr,Canada_HomeOffice_Aggr,
                EMEA_Consumers_Aggr,EMEA_Corporate_Aggr,EMEA_HomeOffice_Aggr,EU_Consumers_Aggr,EU_Corporate_Aggr,
                EU_HomeOffice_Aggr,LATAM_Consumers_Aggr,LATAM_Corporate_Aggr,LATAM_HomeOffice_Aggr,US_Consumers_Aggr,
                US_Corporate_Aggr,US_HomeOff_Aggr)
result <- lapply(df.list, function(x) round(sd(x$monthlyProfit,na.rm=TRUE)/mean(x$monthlyProfit,na.rm=TRUE),2) )
result
###################################################################
#         Name                      CV Value
#     Africa_Consumers_Aggr         1.32
#     Africa_Corporate_Aggr         1.78  
#     Africa_HomeOffice_Aggr        1.79
#     APAC_Consumers_Aggr           0.63
#     APAC_Corporate_Aggr           0.70
#     APAC_HomeOffice_Aggr          1.05
#     Canada_Consumers_Aggr         1.40      
#     Canada_Corporate_Aggr         1.55
#     Canada_HomeOffice_Aggr        2.24
#     EMEA_Consumers_Aggr           2.19
#     EMEA_Corporate_Aggr           4.47
#     EMEA_HomeOffice_Aggr          5.88
#     EU_Consumers_Aggr             0.62
#     EU_Corporate_Aggr             0.76
#     EU_HomeOffice_Aggr            1.12
#     LATAM_Consumers_Aggr          0.66
#     LATAM_Corporate_Aggr          0.81
#     LATAM_HomeOffice_Aggr         1.18
#     US_Consumers_Aggr             1.01
#     US_Corporate_Aggr             1.00
#     US_HomeOff_Aggr               1.10

#APAC_Consumers_Aggr(CV value: 0.63) & EU_Consumers_Aggr(CV value: 0.62) are the 2 most profitable 
#and consistently profitable segments 

###################################################################

# Section 4 - Forecasting

# 4.1 Sales Forecasting

###################################################################

# 4.1.1 - Sales Forecasting for Market: APAC & Segment: Consumers

###################################################################

total_timeser_APAC_Consumer <- ts(APAC_Consumers_Aggr$monthlySales)
indata_APAC_Consumer <- APAC_Consumers_Aggr[1:42,]
out_data_APAC_Consumer <- APAC_Consumers_Aggr[43:48,]

# create and plot timeseries
# no seasonality but increasing trend detected.
timeser_APAC_Consumer_Sales <- ts(indata_APAC_Consumer$monthlySales)
tsdisplay(timeser_APAC_Consumer_Sales,lag.max = 42)


#---------------------------------------------------------------#

# Use Classical Decomposition 

#---------------------------------------------------------------#

# Creating a generic function for smoothening
smoothening_function <- function(timeser){
w <-1
smoothedseries <- stats::filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
return(smoothedseries)

}

# Smoothing the timeser_APAC_Consumer_Sales using Moving Average Smoothing 
smoothedseries_APAC_Consumer_Sales <- smoothening_function(timeser_APAC_Consumer_Sales)


#Plot the smoothed time series
plot(timeser_APAC_Consumer_Sales, ylab="Sales" , main="APAC Consumer Sales",col="red")
lines(smoothedseries_APAC_Consumer_Sales, col="blue", lwd=2)

# Find global predictable (trend) part of series
timevals_indata_APAC_Consumer_Sales <- c(1:nrow(indata_APAC_Consumer))
indata_APAC_Consumer_Sales_newDF <- as.data.frame(cbind(timevals_indata_APAC_Consumer_Sales,smoothedseries_APAC_Consumer_Sales))
colnames(indata_APAC_Consumer_Sales_newDF) <- c("Month","Sales")

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APAC_Consumer_Sales <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
+ Month, data=indata_APAC_Consumer_Sales_newDF)
global_pred_APAC_Consumer_Sales <- predict(lmfit_APAC_Consumer_Sales, Month=timevals_indata_APAC_Consumer_Sales)
summary(global_pred_APAC_Consumer_Sales)
plot(timeser_APAC_Consumer_Sales, ylab="Sales" , main="APAC Consumer Sales",col="black")
lines(smoothedseries_APAC_Consumer_Sales, col="blue", lwd=2)
lines(timevals_indata_APAC_Consumer_Sales, global_pred_APAC_Consumer_Sales, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_Consumer_Sales <- timeser_APAC_Consumer_Sales-global_pred_APAC_Consumer_Sales
plot(local_pred_APAC_Consumer_Sales, col='red', type = "l")
acf(local_pred_APAC_Consumer_Sales)
acf(local_pred_APAC_Consumer_Sales, type="partial")

# model localpred as ARMA series
 
#ARIMA(0,0,1) with zero mean 
#AIC=890.03
# this is a stationary series 
armafit_APAC_Consumer_Sales <- auto.arima(local_pred_APAC_Consumer_Sales)
tsdiag(armafit_APAC_Consumer_Sales)
armafit_APAC_Consumer_Sales

#check if the residual series is white noise
resi_APAC_Consumer_Sales <- local_pred_APAC_Consumer_Sales-fitted(armafit_APAC_Consumer_Sales)

# ADF test
# p-value: .02
adf.test(resi_APAC_Consumer_Sales,alternative = "stationary")

# KPSS test
# p-value: .1
kpss.test(resi_APAC_Consumer_Sales)

# qqplot also suggest that resi_APAC_Consumer_Sales is white noise.
qqnorm(scale(resi_APAC_Consumer_Sales))
abline(coef=c(0,1),col="red")

# The entire Timeseries is Split into:
# 1. Global Variable
# 2. Local Variable
# 3. White Noise

# Predict the OutData based using MAPE
timevals_outdata_APAC_Consumer_Sales <- c(nrow(indata_APAC_Consumer)+1:nrow(out_data_APAC_Consumer))
forecast_outdata_APAC_Consumer <- predict(lmfit_APAC_Consumer_Sales,data.frame(Month =timevals_outdata_APAC_Consumer_Sales))
# forecast values for test data for local predictable
forecast_local_APAC_Consumer_Sales <- predict(armafit_APAC_Consumer_Sales, n.ahead = nrow(out_data_APAC_Consumer))


# total forecast model: Additive model of global and local
total_forecast_APAC_Consumer_Sales <- forecast_outdata_APAC_Consumer + forecast_local_APAC_Consumer_Sales$pred

#Now, let's compare our prediction with the actual values, using MAPE
# MAPE Accuracy: 34.11461
MAPE_class_dec_APAC_Consumer_Sales <- accuracy(total_forecast_APAC_Consumer_Sales,out_data_APAC_Consumer$monthlySales)[5]
MAPE_class_dec_APAC_Consumer_Sales

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_Apac_Consumer_Sales <- c(ts(global_pred_APAC_Consumer_Sales),ts(total_forecast_APAC_Consumer_Sales))
plot(total_timeser_APAC_Consumer, col = "black")
lines(class_dec_pred_Apac_Consumer_Sales, col = "red")
lines(class_dec_pred_Apac_Consumer_Sales[1:42],col="blue", lwd = 2)


# Using Auto-Arima
# ARIMA: 0,1,1
#AIC: 898.23
autoarima_APAC_Consumer_Sales <- auto.arima(timeser_APAC_Consumer_Sales)
autoarima_APAC_Consumer_Sales

tsdiag(autoarima_APAC_Consumer_Sales)

# plot actual versus fitted timeseries 
plot(autoarima_APAC_Consumer_Sales$x, col="green")
lines(fitted(autoarima_APAC_Consumer_Sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_APAC_Consumer_Sales - fitted(autoarima_APAC_Consumer_Sales)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima_APAC_Consumer_Sales, n.ahead = 6)

# MAPE Accuracy: 27.68952
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,out_data_APAC_Consumer$monthlySales)[5]
MAPE_auto_arima

#Plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima_APAC_Consumer_Sales),ts(fcast_auto_arima$pred))
plot(total_timeser_APAC_Consumer, col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(autoarima_APAC_Consumer_Sales), col = "blue", lwd = 2)


#################################################################
# 4.1.2 - Sales Forecasting for Market: EU & Segment: Consumers
#################################################################

total_timeser_EU_Consumer <- ts(EU_Consumers_Aggr$monthlySales)
indata_EU_Consumer <- EU_Consumers_Aggr[1:42,]
out_data_EU_Consumer <- EU_Consumers_Aggr[43:48,]

# create and plot timeseries
# no seasonality but increasing trend detected.
timeser_EU_Consumer_Sales <- ts(indata_EU_Consumer$monthlySales)
tsdisplay(timeser_EU_Consumer_Sales,lag.max = 42)


#---------------------------------------------------------------#

# Use Classical Decomposition 

#---------------------------------------------------------------#
# Smoothing the timeser_EU_Consumer_Sales using Moving Average Smoothing 
smoothedseries_EU_Consumer_Sales <- smoothening_function(timeser_EU_Consumer_Sales)


#Plot the smoothed time series
plot(timeser_EU_Consumer_Sales, ylab="Sales" , main="EU Consumer Sales",col="red")
lines(smoothedseries_EU_Consumer_Sales, col="blue", lwd=2)

# Find global predictable (trend) part of series
timevals_indata_EU_Consumer_Sales <- c(1:nrow(indata_EU_Consumer))
indata_EU_Consumer_Sales_newDF <- as.data.frame(cbind(timevals_indata_EU_Consumer_Sales,smoothedseries_EU_Consumer_Sales))
colnames(indata_EU_Consumer_Sales_newDF) <- c("Month","Sales")

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_Consumer_Sales <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
                                + Month, data=indata_EU_Consumer_Sales_newDF)
global_pred_EU_Consumer_Sales <- predict(lmfit_EU_Consumer_Sales, Month=timevals_indata_EU_Consumer_Sales)
summary(global_pred_EU_Consumer_Sales)
plot(timeser_EU_Consumer_Sales, ylab="Sales" , main="EU Consumer Sales")
lines(smoothedseries_EU_Consumer_Sales, col="blue", lwd=2)
lines(timevals_indata_EU_Consumer_Sales, global_pred_EU_Consumer_Sales, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_Consumer_Sales <- timeser_EU_Consumer_Sales-global_pred_EU_Consumer_Sales
plot(local_pred_EU_Consumer_Sales, col='red', type = "l")
acf(local_pred_EU_Consumer_Sales)
acf(local_pred_EU_Consumer_Sales, type="partial")

# model localpred as ARMA series

#ARIMA(0,0,1) with zero mean 
#AIC=891.35
# this is a stationary series 
armafit_EU_Consumer_Sales <- auto.arima(local_pred_EU_Consumer_Sales)
tsdiag(armafit_EU_Consumer_Sales)
armafit_EU_Consumer_Sales

#check if the residual series is white noise
resi_EU_Consumer_Sales <- local_pred_EU_Consumer_Sales-fitted(armafit_EU_Consumer_Sales)

# ADF test
# p-value: .04
adf.test(resi_EU_Consumer_Sales,alternative = "stationary")

# KPSS test
# p-value: .1
kpss.test(resi_EU_Consumer_Sales)

# qqplot also suggest that resi_EU_Consumer_Sales is white noise.
qqnorm(scale(resi_EU_Consumer_Sales))
abline(coef=c(0,1),col="red")

# The entire Timeseries is Split into:
# 1. Global Variable
# 2. Local Variable
# 3. White Noise

# Predict the OutData based using MAPE
timevals_outdata_EU_Consumer_Sales <- c(nrow(indata_EU_Consumer)+1:nrow(out_data_EU_Consumer))
forecast_outdata_EU_Consumer <- predict(lmfit_EU_Consumer_Sales,data.frame(Month =timevals_outdata_EU_Consumer_Sales))
# forecast values for test data for local predictable
forecast_local_EU_Consumer_Sales <- predict(armafit_EU_Consumer_Sales, n.ahead = nrow(out_data_EU_Consumer))


# total forecast model: Additive model of global and local
total_forecast_EU_Consumer_Sales <- forecast_outdata_EU_Consumer + forecast_local_EU_Consumer_Sales$pred

#Now, let's compare our prediction with the actual values, using MAPE
# MAPE Accuracy: 32.74032
MAPE_class_dec_EU_Consumer_Sales <- accuracy(total_forecast_EU_Consumer_Sales,out_data_EU_Consumer$monthlySales)[5]
MAPE_class_dec_EU_Consumer_Sales

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_EU_Consumer_Sales <- c(ts(global_pred_EU_Consumer_Sales),ts(total_forecast_EU_Consumer_Sales))
plot(total_timeser_EU_Consumer, col = "black")
lines(class_dec_pred_EU_Consumer_Sales, col = "red")
lines(class_dec_pred_EU_Consumer_Sales[1:42],col="blue", lwd = 2)

# Auto Arima:
# ARIMA(2,1,0) 
# AIC: 897.67
autoarima_EU_Consumer_Sales <- auto.arima(timeser_EU_Consumer_Sales)
autoarima_EU_Consumer_Sales

tsdiag(autoarima_EU_Consumer_Sales)

# plot actual versus fitted timeseries 
plot(autoarima_EU_Consumer_Sales$x, col="green")
lines(fitted(autoarima_EU_Consumer_Sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_EU_Consumer_Sales - fitted(autoarima_EU_Consumer_Sales)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima_EU_Consumer_Sales, n.ahead = 6)

# MAPE Accuracy: 28.9226
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,out_data_EU_Consumer$monthlySales)[5]
MAPE_auto_arima

#Plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima_EU_Consumer_Sales),ts(fcast_auto_arima$pred))
plot(total_timeser_EU_Consumer, col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(autoarima_EU_Consumer_Sales), col = "blue", lwd = 2)

######################################################

# 4.2 Quantity Forecasting

######################################################

# 4.2.1 - Quantity Forecasting for Market: APAC & Segment: Consumers

######################################################
total_timeser_APAC_Consumer_Quantity <- ts(APAC_Consumers_Aggr$monthlyQuantity)

# create and plot timeseries

timeser_APAC_Consumer_Quantity <- ts(indata_APAC_Consumer$monthlyQuantity)
tsdisplay(timeser_APAC_Consumer_Quantity,lag.max = 42)


#---------------------------------------------------------------#

# Use Classical Decomposition 

#---------------------------------------------------------------#


# Smoothing the timeser_APAC_Consumer_Quantity using Moving Average Smoothing 
smoothedseries_APAC_Consumer_Quantity <- smoothening_function(timeser_APAC_Consumer_Quantity)


#Plot the smoothed time series
plot(timeser_APAC_Consumer_Quantity, ylab="Quantity" , main="APAC Consumer Quantity",col="red")
lines(smoothedseries_APAC_Consumer_Quantity, col="blue", lwd=2)

# Find global predictable (trend) part of series
timevals_indata_APAC_Consumer_Quantity <- c(1:nrow(indata_APAC_Consumer))
indata_APAC_Consumer_Quantity_newDF <- as.data.frame(cbind(timevals_indata_APAC_Consumer_Quantity,smoothedseries_APAC_Consumer_Quantity))
colnames(indata_APAC_Consumer_Quantity_newDF) <- c("Month","Quantity")

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APAC_Consumer_Quantity <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
                                + Month, data=indata_APAC_Consumer_Quantity_newDF)
global_pred_APAC_Consumer_Quantity <- predict(lmfit_APAC_Consumer_Quantity, Month=timevals_indata_APAC_Consumer_Quantity)
summary(global_pred_APAC_Consumer_Quantity)
plot(timeser_APAC_Consumer_Quantity, ylab="Quantity" , main="APAC Consumer Quantity")
lines(smoothedseries_APAC_Consumer_Quantity, col="blue", lwd=2)
lines(timevals_indata_APAC_Consumer_Quantity, global_pred_APAC_Consumer_Quantity, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_Consumer_Quantity <- timeser_APAC_Consumer_Quantity-global_pred_APAC_Consumer_Quantity
plot(local_pred_APAC_Consumer_Quantity, col='red', type = "l")
acf(local_pred_APAC_Consumer_Quantity)
acf(local_pred_APAC_Consumer_Quantity, type="partial")

# model localpred as ARMA series

#ARIMA(0,0,0) with zero mean 
#AIC=511.14
# this is a stationary series 
armafit_APAC_Consumer_Quantity <- auto.arima(local_pred_APAC_Consumer_Quantity)
tsdiag(armafit_APAC_Consumer_Quantity)
armafit_APAC_Consumer_Quantity

#check if the residual series is white noise
resi_APAC_Consumer_Quantity <- local_pred_APAC_Consumer_Quantity-fitted(armafit_APAC_Consumer_Quantity)

# ADF test
# p-value: .01
adf.test(resi_APAC_Consumer_Quantity,alternative = "stationary")

# KPSS test
# p-value: .1
kpss.test(resi_APAC_Consumer_Quantity)

# qqplot also suggest that resi_APAC_Consumer_Quantity is white noise.
qqnorm(scale(resi_APAC_Consumer_Quantity))
abline(coef=c(0,1),col="red")

# The entire Timeseries is Split into:
# 1. Global Variable
# 2. Local Variable
# 3. White Noise

# Predict the OutData based using MAPE
timevals_outdata_APAC_Consumer_Quantity <- c(nrow(indata_APAC_Consumer)+1:nrow(out_data_APAC_Consumer))
forecast_outdata_APAC_Consumer <- predict(lmfit_APAC_Consumer_Quantity,data.frame(Month =timevals_outdata_APAC_Consumer_Quantity))
# forecast values for test data for local predictable
forecast_local_APAC_Consumer_Quantity <- predict(armafit_APAC_Consumer_Quantity, n.ahead = nrow(out_data_APAC_Consumer))


# total forecast model: Additive model of global and local
total_forecast_APAC_Consumer_Quantity <- forecast_outdata_APAC_Consumer + forecast_local_APAC_Consumer_Quantity$pred

#Now, let's compare our prediction with the actual values, using MAPE
# MAPE Accuracy: 33.79438
MAPE_class_dec_APAC_Consumer_Quantity <- accuracy(total_forecast_APAC_Consumer_Quantity,out_data_APAC_Consumer$monthlyQuantity)[5]
MAPE_class_dec_APAC_Consumer_Quantity

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_Apac_Consumer_Quantity <- c(ts(global_pred_APAC_Consumer_Quantity),ts(total_forecast_APAC_Consumer_Quantity))
plot(total_timeser_APAC_Consumer_Quantity, col = "black")
lines(class_dec_pred_Apac_Consumer_Quantity, col = "red")
lines(class_dec_pred_Apac_Consumer_Quantity[1:42],col="blue", lwd = 2)

# Performing AUTO-ARIMA
# ARIMA(0,1,0) 
# AIC: 534.14
autoarima_APAC_Consumer_Quantity <- auto.arima(timeser_APAC_Consumer_Quantity)
autoarima_APAC_Consumer_Quantity

tsdiag(autoarima_APAC_Consumer_Quantity)

# plot actual versus fitted timeseries 
plot(autoarima_APAC_Consumer_Quantity$x, col="green")
lines(fitted(autoarima_APAC_Consumer_Quantity), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_APAC_Consumer_Quantity - fitted(autoarima_APAC_Consumer_Quantity)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima_APAC_Consumer_Quantity, n.ahead = 6)

# MAPE Accuracy: 26.24458
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,out_data_APAC_Consumer$monthlyQuantity)[5]
MAPE_auto_arima

#Plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima_APAC_Consumer_Quantity),ts(fcast_auto_arima$pred))
plot(total_timeser_APAC_Consumer_Quantity, col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(autoarima_APAC_Consumer_Quantity), col = "blue", lwd = 2)

########################################################################
# 4.1.2 - Quantity Forecasting for Market: EU & Segment: Consumers
########################################################################

total_timeser_EU_Consumer_Quantity <- ts(EU_Consumers_Aggr$monthlyQuantity)
# create and plot timeseries
# no seasonality but increasing trend detected.
timeser_EU_Consumer_Quantity <- ts(indata_EU_Consumer$monthlyQuantity)
tsdisplay(timeser_EU_Consumer_Quantity,lag.max = 42)


#---------------------------------------------------------------#

# Use Classical Decomposition 

#---------------------------------------------------------------#
# Smoothing the timeser_EU_Consumer_Quantity using Moving Average Smoothing 
smoothedseries_EU_Consumer_Quantity <- smoothening_function(timeser_EU_Consumer_Quantity)


#Plot the smoothed time series
plot(timeser_EU_Consumer_Quantity, ylab="Quantity" , main="EU Consumer Quantity",col="red")
lines(smoothedseries_EU_Consumer_Quantity, col="blue", lwd=2)

# Find global predictable (trend) part of series
timevals_indata_EU_Consumer_Quantity <- c(1:nrow(indata_EU_Consumer))
indata_EU_Consumer_Quantity_newDF <- as.data.frame(cbind(timevals_indata_EU_Consumer_Quantity,smoothedseries_EU_Consumer_Quantity))
colnames(indata_EU_Consumer_Quantity_newDF) <- c("Month","Quantity")

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_Consumer_Quantity <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
                              + Month, data=indata_EU_Consumer_Quantity_newDF)
global_pred_EU_Consumer_Quantity <- predict(lmfit_EU_Consumer_Quantity, Month=timevals_indata_EU_Consumer_Quantity)
summary(global_pred_EU_Consumer_Quantity)
plot(timeser_EU_Consumer_Quantity, ylab="Quantity" , main="EU Consumer Quantity")
lines(smoothedseries_EU_Consumer_Quantity, col="blue", lwd=2)
lines(timevals_indata_EU_Consumer_Quantity, global_pred_EU_Consumer_Quantity, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_Consumer_Quantity <- timeser_EU_Consumer_Quantity-global_pred_EU_Consumer_Quantity
plot(local_pred_EU_Consumer_Quantity, col='red', type = "l")
acf(local_pred_EU_Consumer_Quantity)
acf(local_pred_EU_Consumer_Quantity, type="partial")

# model localpred as ARMA series

#ARIMA(2,0,0) with zero mean 
#AIC=502.66
# this is a stationary series 
armafit_EU_Consumer_Quantity <- auto.arima(local_pred_EU_Consumer_Quantity)
tsdiag(armafit_EU_Consumer_Quantity)
armafit_EU_Consumer_Quantity

#check if the residual series is white noise
resi_EU_Consumer_Quantity <- local_pred_EU_Consumer_Quantity-fitted(armafit_EU_Consumer_Quantity)

# ADF test
# p-value: .028
adf.test(resi_EU_Consumer_Quantity,alternative = "stationary")

# KPSS test
# p-value: .1
kpss.test(resi_EU_Consumer_Quantity)

# qqplot also suggest that resi_EU_Consumer_Quantity is white noise.
qqnorm(scale(resi_EU_Consumer_Quantity))
abline(coef=c(0,1),col="red")

# The entire Timeseries is Split into:
# 1. Global Variable
# 2. Local Variable
# 3. White Noise

# Predict the OutData based using MAPE
timevals_outdata_EU_Consumer_Quantity <- c(nrow(indata_EU_Consumer)+1:nrow(out_data_EU_Consumer))
forecast_outdata_EU_Consumer <- predict(lmfit_EU_Consumer_Quantity,data.frame(Month =timevals_outdata_EU_Consumer_Quantity))
# forecast values for test data for local predictable
forecast_local_EU_Consumer_Quantity <- predict(armafit_EU_Consumer_Quantity, n.ahead = nrow(out_data_EU_Consumer))


# total forecast model: Additive model of global and local
total_forecast_EU_Consumer_Quantity <- forecast_outdata_EU_Consumer + forecast_local_EU_Consumer_Quantity$pred

#Now, let's compare our prediction with the actual values, using MAPE
# MAPE Accuracy: 32.59344
MAPE_class_dec_EU_Consumer_Quantity <- accuracy(total_forecast_EU_Consumer_Quantity,out_data_EU_Consumer$monthlyQuantity)[5]
MAPE_class_dec_EU_Consumer_Quantity

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_EU_Consumer_Quantity <- c(ts(global_pred_EU_Consumer_Quantity),ts(total_forecast_EU_Consumer_Quantity))
plot(total_timeser_EU_Consumer_Quantity, col = "black")
lines(class_dec_pred_EU_Consumer_Quantity, col = "red")
lines(class_dec_pred_EU_Consumer_Quantity[1:42],col="blue", lwd = 2)

# Auto Arima:
# ARIMA(2,1,0) 
# AIC: 529.8
autoarima_EU_Consumer_Quantity <- auto.arima(timeser_EU_Consumer_Quantity)
autoarima_EU_Consumer_Quantity

tsdiag(autoarima_EU_Consumer_Quantity)

# plot actual versus fitted timeseries 
plot(autoarima_EU_Consumer_Quantity$x, col="green")
lines(fitted(autoarima_EU_Consumer_Quantity), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_EU_Consumer_Quantity - fitted(autoarima_EU_Consumer_Quantity)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima_EU_Consumer_Quantity, n.ahead = 6)

# MAPE Accuracy: 30.13319
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,out_data_EU_Consumer$monthlyQuantity)[5]
MAPE_auto_arima

#Plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima_EU_Consumer_Quantity),ts(fcast_auto_arima$pred))
plot(total_timeser_EU_Consumer_Quantity, col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(autoarima_EU_Consumer_Quantity), col = "blue", lwd = 2)

