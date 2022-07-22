###-------------------------------------------------------------------
### Roux Institute
### ALY6980 Spring 2022
### Capstone Project
###-------------------------------------------------------------------

# NOTE:  Prior to running this R script, you must first run the following
#        script(s):
#         - "import_electricity_data.R"

######################################################################
#  SETUP
######################################################################

# Packages required for this markdown file
list.of.packages <- c("lubridate", "forecast", "ggplot2", "gridExtra")

# Check which packages from the list have not been installed on local machine,
# and install those which have not.
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(lubridate)
library(forecast)
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
#  - Random walk forecast with drift (all future values equal last observed value
#     plus a drift term)
#  - Seasonal naive forecast (future value equals last observed value of same 
#    season/period of previous year)
#  - Holt-Winters exponential smoothing
#  - Auto ARIMA

# What is the earliest month in the electricity data?
start_elect <- electricity$month[1]
end_elect <- last(electricity$month)
month1_elect <- month(start_elect)
year1_elect <- year(start_elect)

# Starting month of rolling window of predictions 
# (make predictions starting 10 years ago and for each month moving forward)
start_preds_elect <- end_elect - years(10)   


## MEAN FORECAST

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(electricity$month[electricity$month >= start_preds_elect & electricity$month <= end_elect])-18 )){
  temp_end <- start_preds_elect + months(m - 1)
  temp_df <- electricity[electricity$month >= start_elect
                        & electricity$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$electricity_per_kwh, frequency = 12,
                start=c(year1_elect, month1_elect))
  # Make forecast
  fcst <- meanf(temp_ts, 18)  
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(18)),]
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
for (m in 1:(length(electricity$month[electricity$month >= start_preds_elect & electricity$month <= end_elect])-18 )){
  temp_end <- start_preds_elect + months(m - 1)
  temp_df <- electricity[electricity$month >= start_elect
                         & electricity$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$electricity_per_kwh, frequency = 12,
                start=c(year1_elect, month1_elect))
  # Make forecast
  fcst <- rwf(temp_ts, 18) 
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("rw", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
)

## RANDOM WALK FORECAST WITH DRIFT

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(electricity$month[electricity$month >= start_preds_elect & electricity$month <= end_elect])-18 )){
  temp_end <- start_preds_elect + months(m - 1)
  temp_df <- electricity[electricity$month >= start_elect
                         & electricity$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$electricity_per_kwh, frequency = 12,
                start=c(year1_elect, month1_elect))
  # Make forecast
  fcst <- rwf(temp_ts, drift=TRUE, 18) 
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("rwd", 
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
for (m in 1:(length(electricity$month[electricity$month >= start_preds_elect & electricity$month <= end_elect])-18 )){
  temp_end <- start_preds_elect + months(m - 1)
  temp_df <- electricity[electricity$month >= start_elect
                         & electricity$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$electricity_per_kwh, frequency = 12,
                start=c(year1_elect, month1_elect))
  # Make forecast
  fcst <- snaive(temp_ts, 18)  
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(18)),]
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
for (m in 1:(length(electricity$month[electricity$month >= start_preds_elect & electricity$month <= end_elect])-18 )){
  temp_end <- start_preds_elect + months(m - 1)
  temp_df <- electricity[electricity$month >= start_elect
                         & electricity$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$electricity_per_kwh, frequency = 12,
                start=c(year1_elect, month1_elect))
  # Make forecast
  fcst <- forecast(HoltWinters(temp_ts), 18)
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(18)),]
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
for (m in 1:(length(electricity$month[electricity$month >= start_preds_elect & electricity$month <= end_elect])-18 )){
  temp_end <- start_preds_elect + months(m - 1)
  temp_df <- electricity[electricity$month >= start_elect
                         & electricity$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$electricity_per_kwh, frequency = 12,
                start=c(year1_elect, month1_elect))
  # Make forecast
  fcst <- forecast(auto.arima(temp_ts), 18)
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(18)),]
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
start_yr <- year(min(electricity$month))
start_mon <- month(min(electricity$month))

# Convert electricity price data to time series object
elect_ts <- ts(electricity$electricity_per_kwh, 
                    frequency = 12, start=c(start_yr,start_mon))

# Decompose time series
#plot(decompose(elect_ts))

# Look at autocorrelation function and partial correlation function
#tsdisplay(elect_ts)
#tsdisplay(diff(elect_ts)) # 1st order difference
#tsdisplay(diff(elect_ts,2)) # 2nd order difference

# Let's try ARIMA(3,1,2) model 
# (p,d,q):  
# p = order of Auto Regressive model
# q = order of differencing
# q = order of moving average

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(electricity$month[electricity$month >= start_preds_elect & electricity$month <= end_elect])-18 )){
  temp_end <- start_preds_elect + months(m - 1)
  temp_df <- electricity[electricity$month >= start_elect
                         & electricity$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$electricity_per_kwh, frequency = 12,
                start=c(year1_elect, month1_elect))
  # Make forecast
  fcst <- forecast(arima(temp_ts, c(3,1,2)), 18)
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("a312", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
)


# Let's try ARIMA(1,1,0) model 
# (p,d,q):  
# p = order of Auto Regressive model
# q = order of differencing
# q = order of moving average

# Start with empty vectors for forecast average % errors
fcst_ape1 <- c()
fcst_ape6 <- c()
fcst_ape18 <- c()

# Loop through from start of predictions 10 years ago to end of data
for (m in 1:(length(electricity$month[electricity$month >= start_preds_elect & electricity$month <= end_elect])-18 )){
  temp_end <- start_preds_elect + months(m - 1)
  temp_df <- electricity[electricity$month >= start_elect
                         & electricity$month <= temp_end,]
  
  # Define temporary time series
  temp_ts <- ts(temp_df$electricity_per_kwh, frequency = 12,
                start=c(year1_elect, month1_elect))
  # Make forecast
  fcst <- forecast(arima(temp_ts, c(1,1,0)), 18)
  
  # Calculate avg. perc. error for 1 month in the future
  f <- fcst$mean[1]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(1)),]
  fcst_ape1 <- c(fcst_ape1, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 6 months in the future  
  f <- fcst$mean[6]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(6)),]
  fcst_ape6 <- c(fcst_ape6, abs((a-f)/a)*100)
  # Calculate avg. perc. error for 18 months in the future
  f <- fcst$mean[18]
  a <- electricity[2][electricity$month == (max(temp_df$month) + months(18)),]
  fcst_ape18 <- c(fcst_ape18, abs((a-f)/a)*100)
}

# Append to errors dataframe
df_errors[nrow(df_errors) + 1,] = c("a110", 
                                    round(mean(fcst_ape1),3),
                                    round(mean(fcst_ape6),3),
                                    round(mean(fcst_ape18),3)
)


# Display table of model MAPEs
dev.off()
grid.table(df_errors)


######################################################################
#  MAKE 30-MONTH PREDICTIONS FOR EACH MODEL
######################################################################

# Define objects for first month and year in the data
startyear <- year(start_elect)
startmon <- month(start_elect)

# Define time series object w/ heating oil price data
el_ts <- ts(electricity$electricity_per_kwh, frequency = 12,
            start=c(startyear, startmon))

# Make 30-month forecast for each model
el_mf <- meanf(el_ts, 30)  
el_rw <- rwf(el_ts, 30)
el_rwd <- rwf(el_ts, drift=TRUE, 30)
el_sn <- snaive(el_ts, 30)
el_hw <- forecast(HoltWinters(el_ts), 30)
el_aa <- forecast(auto.arima(el_ts), 30)
el_a110 <- forecast(arima(el_ts, c(1,1,0)), 30)
el_a312 <- forecast(arima(el_ts, c(3,1,2)), 30)

# Create sequence of dates for the 30 predicted months
start_pred <- electricity$month[length(electricity$month)] %m+% months(1)
end_pred <- electricity$month[length(electricity$month)] %m+% months(30)

# Create dataframe for storing predicted values
predictions <- data.frame(
  month = seq(start_pred, end_pred, "months"),
  mean = el_mf$mean,
  rand_walk = el_rw$mean,
  rand_walk_wdrift = el_rwd$mean,
  seasonal_naive = el_sn$mean,
  holt_winters = el_hw$mean,
  auto_arima = el_aa$mean,
  arima110 = el_a110$mean,
  arima312 = el_a312$mean
)

# Get full range of months, historic and predicted
all_months <- data.frame(
  month = c(electricity$month, predictions$month))

# Combine historic and predicted data
plot_data <- left_join(all_months, electricity, by="month")
plot_data <- left_join(plot_data, predictions, by="month")


######################################################################
#  PLOT(S) TO COMPARE PREDICTIONS
######################################################################

# Pivot from wide to long for ggplot:
plot_data_long <- plot_data %>% 
  pivot_longer(!month, names_to="forecast_model", values_to="dollars_per_kwh")

# Provide order for forecast models
plot_data_long$forecast_model <- factor(
  plot_data_long$forecast_model, 
  levels=c("electricity_per_kwh", "mean", 
           "rand_walk", "rand_walk_wdrift",
           "seasonal_naive", "holt_winters", "auto_arima",
           "arima110", "arima312"))

# Vector of labels
Labels=c("Historic Price", "Mean Forecast", 
         "Random Walk", "Random Walk w Drift",
         "Seasonal Naive", "Holt Winters", "Auto ARIMA",
         "ARIMA(1,1,0)", "ARIMA(3,1,2)")

# Full Plot
ggplot(data=plot_data_long, 
       aes(x=month, y=dollars_per_kwh, 
           size=forecast_model, 
           col=forecast_model, 
           linetype=forecast_model)
) +
  geom_line() +
  scale_size_manual(values = c(1.5, 0.5, 1,
                               2.5, 0.5, 0.5,
                               1, 1.5, 2), 
                    labels = Labels) +
  scale_linetype_manual(values = c(1,5,5,
                                   1,5,5,
                                   5,1,4
  ), labels = Labels) +
  scale_color_manual(values=c("black",  "pink", "lightsalmon2", 
                              "blue","seagreen", "cadetblue", 
                              "lightblue", "red", "brown"), labels = Labels) +
  #scale_x_date(limits=c(electricity$month[1], end_pred),
  scale_x_date(limits=c((electricity$month[1] %m+% months(357)), end_pred),
               date_breaks="6 months", date_labels="%b%Y", expand = c(0, 0)) +
  labs(title="Electricity Price Forecast for Maine",
       y="Dollars per kWh") +
  ylim(0,0.3) +
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
top_mods <- plot_data_long %>%
  filter(forecast_model == "electricity_per_kwh" | 
           forecast_model == "rand_walk_wdrift" |
           forecast_model == "arima110" |
           forecast_model == "arima312")

# Provide order for forecast models
top_mods$forecast_model <- factor(
  top_mods$forecast_model, 
  levels=c("electricity_per_kwh", "rand_walk_wdrift",
           "arima110", "arima312"))

# Vector of labels
Labels=c("Historic Price", "Random Walk w Drift",
         "ARIMA(1,1,0)", "ARIMA(3,1,2)")

# Top Models Plot
ggplot(data=top_mods, 
       aes(x=month, y=dollars_per_kwh, 
           size=forecast_model, 
           col=forecast_model, 
           linetype=forecast_model)
) +
  geom_line() +
  scale_size_manual(values = c(1.5, 2.5, 1,
                               1), 
                    labels = Labels) +
  scale_linetype_manual(values = c(1,1,1,1), 
                        labels = Labels) +
  scale_color_manual(values=c("black",  "blue", "red", 
                              "brown"), 
                     labels = Labels) +
  #scale_x_date(limits=c(heatingoil$month[1], end_pred),
  scale_x_date(limits=c((electricity$month[1] %m+% months(357)), end_pred),
               date_breaks="6 months", date_labels="%b%Y", expand = c(0, 0)) +
  labs(title="Historic and Forecast Electricity Prices for Maine",
       y="Dollars per Gallon") +
  ylim(0,0.3) +
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



# RECOMMEND RANDOM WALK WITH DRIFT

# Reallow warning messages
options(warn=warn)

# Remove extraneous objects
rm(list = c('list.of.packages', 'new.packages', 'warn', 'all_months', 
            'df_errors', 'el_a110', 'el_a312', 'el_aa', 'el_hw', 'el_mf',
            'el_rw', 'el_rwd', 'el_sn', 'fcst', 'plot_data', 'plot_data_long',
            'predictions', 'temp_df', 'top_mods', 'a', 'el_ts', 'end_elect',
            'end_pred', 'f', 'fcst_ape1', 'fcst_ape18', 'fcst_ape6', 'Labels',
            'm', 'month1_elect', 'start_elect', 'start_mon', 'start_pred',
            'start_preds_elect', 'start_yr', 'startyear', 'startmon',
            'temp_end', 'temp_ts', 'year1_elect', 'elect_ts') )
            
