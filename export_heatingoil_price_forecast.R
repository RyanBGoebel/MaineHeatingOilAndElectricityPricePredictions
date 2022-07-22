###-------------------------------------------------------------------
### Roux Institute
### ALY6980 Spring 2022
### Capstone Project
###-------------------------------------------------------------------

# This R script will generate a 30-month forecast for heating oil prices and 
# export it, along with historical prices since Jan 2005.

# NOTE:  Prior to running this R script, you must first run the following
#        script(s):
#         - "import_heatingoil_data.R"

######################################################################
#  SETUP
######################################################################

# Packages required for this markdown file
list.of.packages <- c("lubridate", "forecast")

# Check which packages from the list have not been installed on local machine,
# and install those which have not.
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(lubridate)
library(forecast)

# Set working directory to folder containing this .Rmd file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Supress warning messages
warn = getOption("warn")
options(warn=-1)

# Start date for export
begin <- as.Date("2005-01-01")

# Define objects for first month and year in the data
startyear <- year(begin)
startmon <- month(begin)

# Filter for begin
filtered <- heatingoil %>%
  filter(month >= begin)


# Define time series object w/ heating oil price data
ho_ts <- ts(filtered$heatingoil_per_gal, frequency = 12,
            start=c(startyear, startmon))

# Make 30-month forecast for each model
heatingoil30 <- forecast(arima(ho_ts, c(2,0,0)), 30)

# Create sequence of dates for the 30 predicted months
start_pred <- filtered$month[length(filtered$month)] %m+% months(1)
end_pred <- filtered$month[length(filtered$month)] %m+% months(30)

# Create dataframe for storing predicted values
pred <- data.frame(
  month = seq(start_pred, end_pred, "months"),
  fore = heatingoil30$mean
)

# Get full range of months, historic and predicted
all_months <- data.frame(
  month = c(filtered$month, pred$month))

# Combine historic and predicted data
heatingoil_forecast <- left_join(all_months, filtered, by="month")
heatingoil_forecast <- left_join(heatingoil_forecast, pred, by="month")

# Make column for combined historic and predicted data
heatingoil_forecast$dollars_per_gal <- rowSums(heatingoil_forecast[ 
  ,c("heatingoil_per_gal", "fore")]
  , na.rm=TRUE)

# Change column names
colnames(heatingoil_forecast) <- c("month",
                                   "historic_dollars_per_gallon",
                                   "predicted_dollars_per_gallon",
                                   "dollars_per_gallon")

# Round prices to nearest cent
heatingoil_forecast <- heatingoil_forecast %>% 
  mutate(across(2:4, round, 2)) %>%  # round to 2 decimal places
  mutate(across(2:4, as.character))  # convert to character to preserve decimal places on export

# Export to csv
file <- file.path("export", "heatingoil_price_forecast.csv")
write.csv(heatingoil_forecast, file, row.names = FALSE, quote=FALSE)

# Reallow warning messages
options(warn=warn)

# Remove extraneous objects
rm(list = c('list.of.packages', 'new.packages', 'warn', 'heatingoil_forecast',
            'pred', 'heatingoil30', 'end_pred', 'file', 'filtered', 
            'start_pred', 'startmon', 'startyear', 'all_months', 'ho_ts', 'begin'))

