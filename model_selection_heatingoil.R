###-------------------------------------------------------------------
### Roux Institute
### ALY6980 Spring 2022
### Capstone Project
###-------------------------------------------------------------------

# NOTE:  Prior to running this R script, you must first run the following
#        script(s):
#         - "import_heatingoil_data.R"


######################################################################
#  SETUP
######################################################################

# Packages required for this markdown file
list.of.packages <- c("lubridate", "forecast", "vars", "Boruta", "ggplot2",
                      "gridExtra")

# Check which packages from the list have not been installed on local machine,
# and install those which have not.
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(lubridate)
library(forecast)
library(vars)
library(Boruta)
library(ggplot2)
library(gridExtra)

# Set working directory to folder containing this .Rmd file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Supress warning messages
warn = getOption("warn")
options(warn=-1)

######################################################################
#  BASELINE FORECASTS
######################################################################

# Calculate 1-month, 6-month, and 18-month MAPE for baseline models:
#  - Mean forecast (All forecast values = mean of all historical values)
#  - Random walk forecast (all future values equal last observed value)
#  - Seasonal naive forecast (future value equals last observed value of same 
#    season/period of previous year)
#  - Holt-Winters exponential smoothing
#  - Auto ARIMA

# Start historical time series in Jan 2005 (3 months after start of data)
start_year <- year(startmonth + months(3))
start_month <- month(startmonth + months(3))

# Starting month of rolling window of predictions 
# (make predictions starting 10 years ago and for each month moving forward)
start_preds <- endmonth - years(10)   # t2


## MEAN FORECAST

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(heatingoil$month[heatingoil$month >= start_preds & heatingoil$month <= endmonth])-18 )){
  temp_end <- start_preds + months(m - 1)
  temp_df <- heatingoil[heatingoil$month >= startmonth + months(3) 
                        & heatingoil$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$heatingoil_per_gal, frequency = 12,
                start=c(start_year, start_month))
  # Make forecast
  fcst <- meanf(temp_ts, 18)  
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Dataframe to store errors
df_errors <- data.frame(model = "mf", 
                        MAPE_1 = round(mean(fcst_ape1),3),
                        MAPE_6 = round(mean(fcst_ape6),3),
                        MAPE_18 = round(mean(fcst_ape18),3)
                        )

## RANDOM WALK FORECAST

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(heatingoil$month[heatingoil$month >= start_preds & heatingoil$month <= endmonth])-18 )){
  temp_end <- start_preds + months(m - 1)
  temp_df <- heatingoil[heatingoil$month >= startmonth + months(3) 
                        & heatingoil$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$heatingoil_per_gal, frequency = 12,
                start=c(start_year, start_month))
  # Make forecast
  fcst <- rwf(temp_ts, 18) 
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("rw", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
                                    )

## SEASONAL NAIVE FORECAST

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(heatingoil$month[heatingoil$month >= start_preds & heatingoil$month <= endmonth])-18 )){
  temp_end <- start_preds + months(m - 1)
  temp_df <- heatingoil[heatingoil$month >= startmonth + months(3) 
                        & heatingoil$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$heatingoil_per_gal, frequency = 12,
                start=c(start_year, start_month))
  # Make forecast
  fcst <- snaive(temp_ts, 18)  
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("sn", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
)


## HOLT WINTERS EXPONENTIAL SMOOTHING FORECAST

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(heatingoil$month[heatingoil$month >= start_preds & heatingoil$month <= endmonth])-18 )){
  temp_end <- start_preds + months(m - 1)
  temp_df <- heatingoil[heatingoil$month >= startmonth + months(3) 
                        & heatingoil$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$heatingoil_per_gal, frequency = 12,
                start=c(start_year, start_month))
  # Make forecast
  fcst <- forecast(HoltWinters(temp_ts), 18)
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("hw", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
)


## AUTO ARIMA FORECAST

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(heatingoil$month[heatingoil$month >= start_preds & heatingoil$month <= endmonth])-18 )){
  temp_end <- start_preds + months(m - 1)
  temp_df <- heatingoil[heatingoil$month >= startmonth + months(3) 
                        & heatingoil$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$heatingoil_per_gal, frequency = 12,
                start=c(start_year, start_month))
  # Make forecast
  fcst <- forecast(auto.arima(temp_ts), 18)
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("aa", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
)


######################################################################
#  CUSTOM ARIMA FORECASTS
######################################################################

# Before jumping right into forecast error any further, let's explore the time 
# series a bit more

# Create variable for start year and start month of data
start_yr <- year(min(heatingoil$month))
start_mon <- month(min(heatingoil$month))

# Convert heating oil price data totime series object
heatingoil_ts <- ts(heatingoil$heatingoil_per_gal, 
                    frequency = 12, start=c(start_yr,start_mon))

# Decompose time series
#plot(decompose(heatingoil_ts))

# Look at autocorrelation function and partial correlation function
#tsdisplay(heatingoil_ts)
#tsdisplay(diff(heatingoil_ts)) # 1st order difference
#tsdisplay(diff(heatingoil_ts,2)) # 2nd order difference
#tsdisplay(diff(heatingoil_ts,3)) # 3rd order difference

# Let's try both ARIMA(2,0,0) model and ARIMA(3,2,0) model 
# (p,d,q):  
# p = order of Auto Regressive model
# q = order of differencing
# q = order of moving average

## ARIMA(2,0,0) FORECAST

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(heatingoil$month[heatingoil$month >= start_preds & heatingoil$month <= endmonth])-18 )){
  temp_end <- start_preds + months(m - 1)
  temp_df <- heatingoil[heatingoil$month >= startmonth + months(3) 
                        & heatingoil$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$heatingoil_per_gal, frequency = 12,
                start=c(start_year, start_month))
  # Make forecast
  fcst <- forecast(arima(temp_ts, c(2,0,0)), 18)
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("a200", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
)


## ARIMA(3,2,0) FORECAST

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(heatingoil$month[heatingoil$month >= start_preds & heatingoil$month <= endmonth])-18 )){
  temp_end <- start_preds + months(m - 1)
  temp_df <- heatingoil[heatingoil$month >= startmonth + months(3) 
                        & heatingoil$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$heatingoil_per_gal, frequency = 12,
                start=c(start_year, start_month))
  # Make forecast
  fcst <- forecast(arima(temp_ts, c(3,2,0)), 18)
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- heatingoil[2][heatingoil$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("a320", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
)


######################################################################
#  MULTIVARIATE VAR MODEL
######################################################################

# Before getting to the VAR model itself, let's do some variable importance
# analysis on the external variables to see if there are any we can
# eliminate due to unimportance.

# Filter external variable datasets to start/end at the same dates as the 
# heating oil data

# Find earliest month in heating oil price data to use as basis for other datasets
startmonth <- as.Date(min(heatingoil$month)) 
endmonth <- as.Date(max(heatingoil$month)) 

# Filter based on start/end month
weather <- weather %>% 
  filter(month >= startmonth) %>% filter(month <= endmonth) 
sp500 <- sp500 %>% 
  filter(month >= startmonth) %>% filter(month <= endmonth) 
crude <- crude %>%
  filter(month >= startmonth) %>% filter(month <= endmonth)
diesel <- diesel %>%
  filter(month >= startmonth) %>% filter(month <= endmonth)
gepu <- gepu %>%
  filter(month >= startmonth) %>% filter(month <= endmonth)
gpr <- gpr %>%
  filter(month >= startmonth) %>% filter(month <= endmonth)

# Combine into one dataframe
combined <- heatingoil
combined <- merge(combined, weather, by="month")
combined <- merge(combined, sp500, by="month")
combined <- merge(combined, crude, by="month")
combined <- merge(combined, diesel, by="month")
combined <- merge(combined, gepu, by="month")
combined <- merge(combined, gpr, by="month")


# Since this variable importance analysis is being performed for a time series
# analysis / forecasting model, let's include lag variables in the importance
# analysis.

## How many lags to look at?
# VARselect(dplyr::select(combined, -month), lag.max=12, type="const")$selection
## Two of the selection criteria determined that 2 lags are optimal, 
## so go with that.

# Add lagged values into the dataset
combined_wlags <- combined %>% 
  mutate(avgtemp_lag1 = lag(avgtemp, n=1),
         avgtemp_lag2 = lag(avgtemp, n=2),
         avg_close_lag1 = lag(avg_close, n=1),
         avg_close_lag2 = lag(avg_close, n=2),
         diesel_per_gal_lag1 = lag(diesel_per_gal, n=1),
         diesel_per_gal_lag2 = lag(diesel_per_gal, n=2),
         WTI_per_bbl_lag1 = lag(WTI_per_bbl, n=1),
         WTI_per_bbl_lag2 = lag(WTI_per_bbl, n=2),
         GEPU_lag1 = lag(GEPU, n=1),
         GEPU_lag2 = lag(GEPU, n=2),
         GPR_lag1 = lag(GPR, n=1),
         GPR_lag2 = lag(GPR, n=2),
         heatingoil_per_gal_lag1 = lag(heatingoil_per_gal, n=1),
         heatingoil_per_gal_lag2 = lag(heatingoil_per_gal, n=2))

# Remove first two rows since they now contain some NA's due to lag calcs
combined_wlags <- combined_wlags[-(1:2),]

# Remove "month" column
combined_wlags <- combined_wlags %>% dplyr::select(-month)

# Use the Boruta package to analyze view which vars affect heating oil price the most
set.seed(42)
boruta_output <- Boruta(heatingoil_per_gal~., data=combined_wlags, doTrace=0)
#print(boruta_output)

# Some variables are still tentative -->> Fix/Refine
set.seed(42)
boruta_final <- TentativeRoughFix(boruta_output)
#print(boruta_final)

# Plot
opar <- par()
par(mar=c(5.1, 10.1, 2.1, 2.1))
plot(boruta_final, 
     xlab="Importance Level", ylab="", 
     horizontal=TRUE, las=1, 
     yaxt = "n",
     cex.main=1.5,
     #main="Variable Importance Analysis for Heating Oil Price Forecast"
)
lz <- lapply(1:ncol(boruta_final$ImpHistory), function(i)
  boruta_final$ImpHistory[is.finite(boruta_final$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_final$ImpHistory)
Labels <- sort(sapply(lz, median))
axis(side = 2,las=2,labels = names(Labels),
     at = 1:ncol(boruta_final$ImpHistory), cex.axis = 1, 
     tck=1, lty=2, col="gray90")
par(new=TRUE)
plot(boruta_final, yaxt="n", ylab="", xlab="", 
     horizontal=TRUE, las=1)
par <- opar

# All variables are deemed to have some importance.

#Back to the model building

## VAR FORECAST (w/ 1, 2, and 7 lags - with and without seasonality)

# Start with empty vectors for forecast average % errors
var1_ape1 <- c()
var1_ape6 <- c()
var1_ape18 <- c()

var1_ape1s <- c()
var1_ape6s <- c()
var1_ape18s <- c()

var2_ape1 <- c()
var2_ape6 <- c()
var2_ape18 <- c()

var2_ape1s <- c()
var2_ape6s <- c()
var2_ape18s <- c()

var7_ape1 <- c()
var7_ape6 <- c()
var7_ape18 <- c()

var7_ape1s <- c()
var7_ape6s <- c()
var7_ape18s <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(heatingoil$month[heatingoil$month >= start_preds & heatingoil$month <= endmonth])-18 )){
  temp_end <- start_preds + months(m - 1)
  temp_df <- combined[combined$month >= startmonth + months(3) 
                      & combined$month <= temp_end,]

  # Define temporary time series for each variable
  temp_heatingoil_ts <- ts(temp_df$heatingoil_per_gal, frequency = 12,
                start=c(start_year, start_month))
  temp_avgtemp_ts <- ts(temp_df$avgtemp, frequency = 12,
                           start=c(start_year, start_month))
  temp_sp500_ts <- ts(temp_df$avg_close, frequency = 12,
                      start=c(start_year, start_month))
  temp_crude_ts <- ts(temp_df$WTI_per_bbl, frequency = 12,
                      start=c(start_year, start_month))  
  temp_diesel_ts <- ts(temp_df$diesel_per_gal, frequency = 12,
                      start=c(start_year, start_month))
  temp_gepu_ts <- ts(temp_df$GEPU, frequency = 12,
                      start=c(start_year, start_month))
  temp_gpr_ts <- ts(temp_df$GPR, frequency = 12,
                      start=c(start_year, start_month))
  
  # Use first order differences of each variable's time series
  heatingoilts1 <- diff(temp_heatingoil_ts, differences=1)
  avgtempts1 <- diff(temp_avgtemp_ts, differences=1)
  sp500ts1 <- diff(temp_sp500_ts, differences=1)
  crudets1 <- diff(temp_crude_ts, differences=1)
  dieselts1 <- diff(temp_diesel_ts, differences=1)
  geputs1 <- diff(temp_gepu_ts, differences=1)
  gprts1 <- diff(temp_gpr_ts, differences=1)
  
  # Combine all the 1st order difference times series into a dataframe
  combined_ts <- cbind(heatingoilts1, avgtempts1, sp500ts1, 
                       crudets1, dieselts1, geputs1, gprts1)
  colnames(combined_ts) <- cbind("price_heatingoil", "avg_temp", "sp500",
                                 "price_crudeoil", "price_diesel", 
                                 "GEPU", "GPR")
  
  # Define VAR models
  VAR_model_1 <- VAR(combined_ts, p = 1, type = "const", season = NULL, exog = NULL) 
  VAR_model_1s <- VAR(combined_ts, p = 1, type = "const", season = 4, exog = NULL) 
  VAR_model_2 <- VAR(combined_ts, p = 2, type = "const", season = NULL, exog = NULL)
  VAR_model_2s <- VAR(combined_ts, p = 2, type = "const", season = 4, exog = NULL) 
  VAR_model_7 <- VAR(combined_ts, p = 7, type = "const", season = NULL, exog = NULL)
  VAR_model_7s <- VAR(combined_ts, p = 7, type = "const", season = 4, exog = NULL) 
  
  # Make forecasts
  fcst_var1 <- predict(VAR_model_1, n.ahead = 29, ci = 0.95)
  fcst_var2 <- predict(VAR_model_2, n.ahead = 29, ci = 0.95)
  fcst_var7 <- predict(VAR_model_7, n.ahead = 29, ci = 0.95)
  fcst_var1s <- predict(VAR_model_1s, n.ahead = 29, ci = 0.95)
  fcst_var2s <- predict(VAR_model_2s, n.ahead = 29, ci = 0.95)
  fcst_var7s <- predict(VAR_model_7s, n.ahead = 29, ci = 0.95)
  
  # Predicted 1st order differences:
  pred_1order1 <- unlist(fcst_var1$fcst$price_heatingoil)[,1]
  pred_1order2 <- unlist(fcst_var2$fcst$price_heatingoil)[,1]
  pred_1order7 <- unlist(fcst_var7$fcst$price_heatingoil)[,1]
  pred_1order1s <- unlist(fcst_var1s$fcst$price_heatingoil)[,1]
  pred_1order2s <- unlist(fcst_var2s$fcst$price_heatingoil)[,1]
  pred_1order7s <- unlist(fcst_var7s$fcst$price_heatingoil)[,1]
  
  # Last (most recent) value of historic heating oil prices
  minus1 <- temp_heatingoil_ts[length(temp_heatingoil_ts)]
  
  # Calculate avg. perc. error for 1 month in the future
  f1 <- last(cumsum(c(minus1, pred_1order1[1])))
  f2 <- last(cumsum(c(minus1, pred_1order2[1])))
  f7 <- last(cumsum(c(minus1, pred_1order7[1])))
  f1s <- last(cumsum(c(minus1, pred_1order1s[1])))
  f2s <- last(cumsum(c(minus1, pred_1order2s[1])))
  f7s <- last(cumsum(c(minus1, pred_1order7s[1])))
  
  a <- combined["heatingoil_per_gal"][combined$month == (max(temp_df$month) + months(1)),]
  
  ape11 <- abs((a-f1)/a)*100
  ape21 <- abs((a-f2)/a)*100
  ape71 <- abs((a-f7)/a)*100
  ape11s <- abs((a-f1s)/a)*100
  ape21s <- abs((a-f2s)/a)*100
  ape71s <- abs((a-f7s)/a)*100
  
  var1_ape1 <- c(var1_ape1, ape11)
  var2_ape1 <- c(var2_ape1, ape21)
  var7_ape1 <- c(var7_ape1, ape71)
  var1_ape1s <- c(var1_ape1s, ape11s)
  var2_ape1s <- c(var2_ape1s, ape21s)
  var7_ape1s <- c(var7_ape1s, ape71s)
  
  # Calculate avg. perc. error for 6 months in the future
  f1 <- last(cumsum(c(minus1, pred_1order1[6])))
  f2 <- last(cumsum(c(minus1, pred_1order2[6])))
  f7 <- last(cumsum(c(minus1, pred_1order7[6])))
  f1s <- last(cumsum(c(minus1, pred_1order1s[6])))
  f2s <- last(cumsum(c(minus1, pred_1order2s[6])))
  f7s <- last(cumsum(c(minus1, pred_1order7s[6])))
  
  a <- combined["heatingoil_per_gal"][combined$month == (max(temp_df$month) + months(6)),]
  
  ape16 <- abs((a-f1)/a)*100
  ape26 <- abs((a-f2)/a)*100
  ape76 <- abs((a-f7)/a)*100
  ape16s <- abs((a-f1s)/a)*100
  ape26s <- abs((a-f2s)/a)*100
  ape76s <- abs((a-f7s)/a)*100
  
  var1_ape6 <- c(var1_ape6, ape16)
  var2_ape6 <- c(var2_ape6, ape26)
  var7_ape6 <- c(var7_ape6, ape76)
  var1_ape6s <- c(var1_ape6s, ape16s)
  var2_ape6s <- c(var2_ape6s, ape26s)
  var7_ape6s <- c(var7_ape6s, ape76s)
  
  # Calculate avg. perc. error for 18 months in the future
  f1 <- last(cumsum(c(minus1, pred_1order1[18])))
  f2 <- last(cumsum(c(minus1, pred_1order2[18])))
  f7 <- last(cumsum(c(minus1, pred_1order7[18])))
  f1s <- last(cumsum(c(minus1, pred_1order1s[18])))
  f2s <- last(cumsum(c(minus1, pred_1order2s[18])))
  f7s <- last(cumsum(c(minus1, pred_1order7s[18])))
  
  a <- combined["heatingoil_per_gal"][combined$month == (max(temp_df$month) + months(18)),]
  
  ape118 <- abs((a-f1)/a)*100
  ape218 <- abs((a-f2)/a)*100
  ape718 <- abs((a-f7)/a)*100
  ape118s <- abs((a-f1s)/a)*100
  ape218s <- abs((a-f2s)/a)*100
  ape718s <- abs((a-f7s)/a)*100

  var1_ape18 <- c(var1_ape18, ape118)
  var2_ape18 <- c(var2_ape18, ape218)
  var7_ape18 <- c(var7_ape18, ape718)
  var1_ape18s <- c(var1_ape18s, ape118s)
  var2_ape18s <- c(var2_ape18s, ape218s)
  var7_ape18s <- c(var7_ape18s, ape718s)
  
}


#Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("var1",
                                    round(mean(var1_ape1),3),
                                    round(mean(var1_ape6),3),
                                    round(mean(var1_ape18),3)
)
df_errors[nrow(df_errors) + 1,] = c("var1s",
                                    round(mean(var1_ape1s),3),
                                    round(mean(var1_ape6s),3),
                                    round(mean(var1_ape18s),3)
)
df_errors[nrow(df_errors) + 1,] = c("var2", 
                                    round(mean(var2_ape1),3),
                                    round(mean(var2_ape6),3),
                                    round(mean(var2_ape18),3)
)
df_errors[nrow(df_errors) + 1,] = c("var2s", 
                                    round(mean(var2_ape1s),3),
                                    round(mean(var2_ape6s),3),
                                    round(mean(var2_ape18s),3)
)
df_errors[nrow(df_errors) + 1,] = c("var7",
                                    round(mean(var7_ape1),3),
                                    round(mean(var7_ape6),3),
                                    round(mean(var7_ape18),3)
)
df_errors[nrow(df_errors) + 1,] = c("var7s",
                                    round(mean(var7_ape1s),3),
                                    round(mean(var7_ape6s),3),
                                    round(mean(var7_ape18s),3)
)

# Display table of model MAPEs
dev.off()
grid.table(df_errors)


######################################################################
#  MAKE 30-MONTH PREDICTIONS FOR EACH MODEL
######################################################################

# Define objects for first month and year in the data
startyear <- year(startmonth)
startmonth <- month(startmonth)

# Define time series object w/ heating oil price data
ho_ts <- ts(heatingoil$heatingoil_per_gal, frequency = 12,
              start=c(startyear, startmonth))

# Define time series for external variables
avgtemp_ts <- ts(weather$avgtemp, frequency = 12,
                      start=c(startyear, startmonth))
sp500_ts <- ts(sp500$avg_close, frequency = 12,
                    start=c(startyear, startmonth))
crude_ts <- ts(crude$WTI_per_bbl, frequency = 12,
                    start=c(startyear, startmonth))  
diesel_ts <- ts(diesel$diesel_per_gal, frequency = 12,
                     start=c(startyear, startmonth))
gepu_ts <- ts(gepu$GEPU, frequency = 12,
                   start=c(startyear, startmonth))
gpr_ts <- ts(gpr$GPR, frequency = 12,
                  start=c(startyear, startmonth))

# Use first order differences of each variable's time series
ho_ts1 <- diff(ho_ts, differences=1)
avgtemp_ts1 <- diff(avgtemp_ts, differences=1)
sp500_ts1 <- diff(sp500_ts, differences=1)
crude_ts1 <- diff(crude_ts, differences=1)
diesel_ts1 <- diff(diesel_ts, differences=1)
gepu_ts1 <- diff(gepu_ts, differences=1)
gpr_ts1 <- diff(gpr_ts, differences=1)

# Combine all the 1st order difference times series into a dataframe
combined_ts <- cbind(ho_ts1, avgtemp_ts1, sp500_ts1, 
                     crude_ts1, diesel_ts1, gepu_ts1, gpr_ts1)
colnames(combined_ts) <- cbind("price_heatingoil", "avg_temp", "sp500",
                               "price_crudeoil", "price_diesel", 
                               "GEPU", "GPR")

# Define VAR models
VAR_model_1 <- VAR(combined_ts, p = 1, type = "const", season = NULL, exog = NULL) 
VAR_model_2 <- VAR(combined_ts, p = 2, type = "const", season = NULL, exog = NULL) 
VAR_model_7 <- VAR(combined_ts, p = 7, type = "const", season = NULL, exog = NULL)
VAR_model_1s <- VAR(combined_ts, p = 1, type = "const", season = 4, exog = NULL) 
VAR_model_2s <- VAR(combined_ts, p = 2, type = "const", season = 4, exog = NULL) 
VAR_model_7s <- VAR(combined_ts, p = 7, type = "const", season = 4, exog = NULL)


# Make 30-month forecast for each model
ho_mf <- meanf(ho_ts, 30)  
ho_rw <- rwf(ho_ts, 30)
ho_sn <- snaive(ho_ts, 30)
ho_hw <- forecast(HoltWinters(ho_ts), 30)
ho_aa <- forecast(auto.arima(ho_ts), 30)
ho_a200 <- forecast(arima(ho_ts, c(2,0,0)), 30)
ho_a320 <- forecast(arima(ho_ts, c(3,2,0)), 30)
var1 <- predict(VAR_model_1, n.ahead = 29, ci = 0.95)
var1 <- unlist(var1$fcst$price_heatingoil)[,1]
var1 <- cumsum(c(last(heatingoil$heatingoil_per_gal), var1))
var2 <- predict(VAR_model_2, n.ahead = 29, ci = 0.95)
var2 <- unlist(var2$fcst$price_heatingoil)[,1]
var2 <- cumsum(c(last(heatingoil$heatingoil_per_gal), var2))
var7 <- predict(VAR_model_7, n.ahead = 29, ci = 0.95)
var7 <- unlist(var7$fcst$price_heatingoil)[,1]
var7 <- cumsum(c(last(heatingoil$heatingoil_per_gal), var7))
var1s <- predict(VAR_model_1s, n.ahead = 29, ci = 0.95)
var1s <- unlist(var1s$fcst$price_heatingoil)[,1]
var1s <- cumsum(c(last(heatingoil$heatingoil_per_gal), var1s))
var2s <- predict(VAR_model_2s, n.ahead = 29, ci = 0.95)
var2s <- unlist(var2s$fcst$price_heatingoil)[,1]
var2s <- cumsum(c(last(heatingoil$heatingoil_per_gal), var2s))
var7s <- predict(VAR_model_7s, n.ahead = 29, ci = 0.95)
var7s <- unlist(var7s$fcst$price_heatingoil)[,1]
var7s <- cumsum(c(last(heatingoil$heatingoil_per_gal), var7s))

# Create sequence of dates for the 30 predicted months
start_pred <- heatingoil$month[length(heatingoil$month)] %m+% months(1)
end_pred <- heatingoil$month[length(heatingoil$month)] %m+% months(30)

# Create dataframe for storing predicted values
predictions <- data.frame(
  month = seq(start_pred, end_pred, "months"),
  mean = ho_mf$mean,
  rand_walk = ho_rw$mean,
  seasonal_naive = ho_sn$mean,
  holt_winters = ho_hw$mean,
  auto_arima = ho_aa$mean,
  arima200 = ho_a200$mean,
  arima320 = ho_a320$mean,
  var_lag1 = var1,
  var_lag2 = var2,
  var_lag7 = var7,
  var_lag1s = var1s,
  var_lag2s = var2s,
  var_lag7s = var7s
)

# Get full range of months, historic and predicted
all_months <- data.frame(
  month = c(heatingoil$month, predictions$month))

# Combine historic and predicted data
plot_data <- left_join(all_months, heatingoil, by="month")
plot_data <- left_join(plot_data, predictions, by="month")


######################################################################
#  PLOT(S) TO COMPARE PREDICTIONS
######################################################################

# Pivot from wide to long for ggplot:
plot_data_long <- plot_data %>% 
  pivot_longer(!month, names_to="forecast_model", values_to="dollars_per_gal")

# Provide order for forecast models
plot_data_long$forecast_model <- factor(
  plot_data_long$forecast_model, 
  levels=c("heatingoil_per_gal", "mean", "rand_walk",
           "seasonal_naive", "holt_winters", "auto_arima",
           "arima200", "arima320", "var_lag1", "var_lag2",
           "var_lag7", "var_lag1s", "var_lag2s",
           "var_lag7s"))

# Vector of labels
Labels=c("Historic Price", "Mean Forecast", "Random Walk",
         "Seasonal Naive", "Holt Winters", "Auto ARIMA",
         "ARIMA(2,0,0)", "ARIMA(3,2,0)", "VAR w/ 1 lag", 
         "VAR w/ 2 lags", "VAR w/ 7 lags", "VAR w/ 1L & Seasonality",
         "VAR w/ 2L & Seasonality", "VAR w/ 7L & Seasonality")

# Full Plot
ggplot(data=plot_data_long, 
       aes(x=month, y=dollars_per_gal, 
           size=forecast_model, 
           col=forecast_model, 
           linetype=forecast_model)
       ) +
  geom_line() +
  scale_size_manual(values = c(1.5, 1, 2.5,
                               0.5, 0.5,
                               1, 2, 0.5,
                               1, 1, 0.5,
                               0.5, 0.5, 0.5), 
                    labels = Labels) +
  scale_linetype_manual(values = c(1,2,1,
                                   5,5,
                                   5,1,5,
                                   2,5,5,
                                   2,5,5
                                   ), labels = Labels) +
  scale_color_manual(values=c("black",  "hotpink", "brown", 
                              "seagreen", "cadetblue", 
                              "turquoise", "blue", "lightblue", 
                              "hotpink", "pink", "lightsalmon2",
                              "lightsalmon3", "salmon", "orange"), labels = Labels) +
  #scale_x_date(limits=c(heatingoil$month[1], end_pred),
  scale_x_date(limits=c((heatingoil$month[1] %m+% months(180)), end_pred),
               date_breaks="6 months", date_labels="%b%Y", expand = c(0, 0)) +
  labs(title="Heating Oil Price Forecast for Maine",
       y="Dollars per Gallon") +
  ylim(0,15) +
  guides(col=guide_legend("Prediction Model"),
         size=guide_legend("Prediction Model"),
         linetype=guide_legend("Prediction Model")) +
  theme(legend.key.size =  unit(0.5, "in"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color="gray85", linetype = "dotted"),
        plot.title = element_text(size=16, hjust=0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=8, angle=45, hjust=1),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        legend.key = element_rect(fill = NA, color = NA))


# Plot only top models
#mods <- c("heatingoil_per_gal", "mean", "rand_walk", 
#         "arima200", "var_lag1", "var_lag2s")
top_mods <- plot_data_long %>%
  filter(forecast_model == "heatingoil_per_gal" | 
           forecast_model == "rand_walk" |
           forecast_model == "arima200" )

# Provide order for forecast models
top_mods$forecast_model <- factor(
  top_mods$forecast_model, 
  levels=c("heatingoil_per_gal", "rand_walk",
           "arima200"))

# Vector of labels
Labels=c("Historic Price", "Random Walk",
         "ARIMA(2,0,0)")

# Top Models Plot
ggplot(data=top_mods, 
       aes(x=month, y=dollars_per_gal, 
           size=forecast_model, 
           col=forecast_model, 
           linetype=forecast_model)
) +
  geom_line() +
  scale_size_manual(values = c(1.5, 2.5, 2), 
                    labels = Labels) +
  scale_linetype_manual(values = c(1,1,1), 
                        labels = Labels) +
  scale_color_manual(values=c("black",  "brown", "blue"), 
                     labels = Labels) +
  #scale_x_date(limits=c(heatingoil$month[1], end_pred),
  scale_x_date(limits=c((heatingoil$month[1] %m+% months(180)), end_pred),
               date_breaks="6 months", date_labels="%b%Y", expand = c(0, 0)) +
  labs(title="Heating Oil Price Forecast for Maine",
       y="Dollars per Gallon") +
  ylim(0,15) +
  guides(col=guide_legend("Prediction Model"),
         size=guide_legend("Prediction Model"),
         linetype=guide_legend("Prediction Model")) +
  theme(legend.key.size =  unit(0.5, "in"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color="gray85", linetype = "dotted"),
        plot.title = element_text(size=16, hjust=0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=8, angle=45, hjust=1),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        legend.key = element_rect(fill = NA, color = NA))


### Recommend ARIMA(2,0,0) model


# Reallow warning messages
options(warn=warn)

# Remove extraneous objects
rm(list = c('list.of.packages', 'new.packages', 'warn', 'all_months', 
            'boruta_final', 'boruta_output', 'combined', 'combined_ts',
            'combined_wlags', 'crude', 'df_errors', 
            'fcst', 'fcst_var1', 'fcst_var1s', 'fcst_var2', 'fcst_var2s',
            'fcst_var7', 'fcst_var7s', 'gepu', 'gpr', 'ho_a200', 'ho_a320',
            'ho_aa', 'ho_hw', 'ho_mf', 'ho_rw', 'ho_sn', 'lz', 'plot_data',
            'plot_data_long', 'predictions', 'sp500', 'temp_df', 'top_mods',
            'VAR_model_1', 'VAR_model_1s','VAR_model_2', 'VAR_model_2s',
            'VAR_model_7', 'VAR_model_7s', 'weather', 'a', 'ape11', 'ape118',
            'ape118s', 'ape11s', 'ape16', 'ape16s', 'ape21', 'ape218', 'ape218s',
            'ape21s', 'ape26', 'ape26s', 'ape71', 'ape718', 'ape718s', 'ape71s',
            'ape76', 'ape76s', 'avgtemp_ts', 'avgtemp_ts1', 'avgtempts1', 
            'crude_ts', 'crude_ts1', 'crudets1', 'diesel_ts', 'diesel_ts1',
            'dieselts1', 'end_pred', 'f', 'f1', 'f1s', 'f2', 'f2s',
            'f7', 'f7s', 'fcst_ape1', 'fcst_ape18', 'fcst_ape6', 'gepu_ts',
            'gepu_ts1', 'geputs1', 'gpr_ts', 'gpr_ts1', 'gprts1', 
            'heatingoil_ts', 'heatingoilts1', 'ho_ts', 'ho_ts1', 'Labels', 'm',
            'minus1', 'pred_1order1', 'pred_1order1s', 'pred_1order2', 
            'pred_1order2s', 'pred_1order7', 'pred_1order7s', 'sp500_ts',
            'sp500_ts1', 'sp500ts1', 'start_mon', 'start_month', 'start_pred',
            'start_year', 'startyear', 'temp_ts', 'start_yr', 'var1', 'start_preds',
            'temp_avgtemp_ts', 'temp_crude_ts', 'temp_diesel_ts', 'temp_end', 
            'temp_gepu_ts', 'temp_gpr_ts', 'temp_heatingoil_ts', 'temp_sp500_ts',
            'var1_ape1', 'var1_ape18', 'var1_ape18s', 'var1_ape1s', 'var1_ape6',
            'var1_ape6s', 'var1s', 'var2', 'var2_ape1', 'var2_ape18', 
            'var2_ape18s', 'var2_ape1s', 'var2_ape6', 'var2_ape6s', 'var2s',
            'var7', 'var7_ape1', 'var7_ape18', 'var7_ape18s', 'var7_ape1s',
            'var7_ape6', 'var7_ape6s', 'var7s'))






