###-------------------------------------------------------------------
### Roux Institute
### ALY6980 Spring 2022
### Capstone Project
###-------------------------------------------------------------------

# This R script will generate a 30-month forecast for electricity prices and 
# export it, along with calculated average historical prices since Jan 2005.

# NOTE:  Prior to running this R script, you must first run the following
#        script(s):
#         - "import_electricity_data.R"

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
filtered <- electricity %>%
  filter(month >= begin)


# Define time series object w/ heating oil price data
el_ts <- ts(filtered$electricity_per_kwh, frequency = 12,
            start=c(startyear, startmon))

# Make 30-month forecast
electricity30 <- rwf(el_ts, drift=TRUE, 30)

# Create sequence of dates for the 30 predicted months
start_pred <- filtered$month[length(filtered$month)] %m+% months(1)
end_pred <- filtered$month[length(filtered$month)] %m+% months(30)

# Create dataframe for storing predicted values
pred <- data.frame(
  month = seq(start_pred, end_pred, "months"),
  fore = electricity30$mean
)

# Get full range of months, historic and predicted
all_months <- data.frame(
  month = c(filtered$month, pred$month))

# Combine historic and predicted data
electricity_forecast <- left_join(all_months, filtered, by="month")
electricity_forecast <- left_join(electricity_forecast, pred, by="month")

# Make column for combined historic and predicted data
electricity_forecast$dollars_per_kwh <- rowSums(electricity_forecast[ 
  ,c("electricity_per_kwh", "fore")]
  , na.rm=TRUE)

# Change column names
colnames(electricity_forecast) <- c("month",
                                   "historic_dollars_per_kwh",
                                   "predicted_dollars_per_kwh",
                                   "dollars_per_kwh")

# Round prices to nearest hundredth cent
electricity_forecast <- electricity_forecast %>% 
  mutate(across(2:4, round, 4)) %>%  # round to 2 decimal places
  mutate(across(2:4, as.character))  # convert to character to preserve decimal places on export

# Export to csv
file <- file.path("export", "electricity_price_forecast.csv")
write.csv(electricity_forecast, file, row.names = FALSE, quote=FALSE)

# Reallow warning messages
options(warn=warn)

# Remove extraneous objects
rm(list = c('list.of.packages', 'new.packages', 'warn', 'electricity_forecast',
            'pred', 'electricity30', 'end_pred', 'file', 'filtered', 
            'start_pred', 'startmon', 'startyear', 'all_months', 'el_ts', 'begin'))

