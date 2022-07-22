###-------------------------------------------------------------------
### Roux Institute
### ALY6980 Spring 2022
### Capstone Project
###-------------------------------------------------------------------

# NOTES: * This script requires importing both historical heating oil
#          prices and New England diesel prices.  The diesel prices
#          are used to impute the missing values for historic heating
#          oil pricing data (typically from summer months)
#        * Prior to running this R file, the following data file(s)  
#          should already be located in the "data" directory (see User  
#          Guide for more information):
#           - "historical propane - k1 - #2 price tables_charts1.xlsx"
#           - "psw18vwall.xls"

######################################################################
#  SETUP
######################################################################

# Packages required for this markdown file
list.of.packages <- c("readxl", "dplyr", "tidyr", "lubridate")

# Check which packages from the list have not been installed on local machine,
# and install those which have not.
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# Set working directory to folder containing this .R file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Supress warning messages
warn = getOption("warn")
options(warn=-1)

######################################################################
#  DEFINE FUNCTIONS
######################################################################

# FUNCTION TO INSERT MISSING MONTHS INTO MONTYLY DATA WITH NA PRICE
missing_months <- function(df) {
  
  # Find the column named "month" in the dataframe
  month_column <- df[ , which(colnames(df)=="month")]
  
  # Create new dataframe with full range of months from first to last month
  first_month <- min(month_column)
  last_month <- max(month_column)
  month_range <- data.frame(seq(first_month, last_month, by="month"))
  colnames(month_range) <- "month"
  
  # Merge month_range with original df
  df <- merge(month_range, df, by="month", all.x = TRUE)
  
  return(df)
}


######################################################################
#  LOAD HEATING OIL PRICING DATA
######################################################################

# Define file name/location and import
file <- file.path("data", "historical propane - k1 - #2 price tables_charts1.xlsx")
heatingoil <- as.data.frame(
  read_excel(file, sheet = "#2 chart & monthly averages",
             range = cell_cols("A:B"),
             col_names = c("month", "heatingoil_per_gal"),
             col_types = c("date", "numeric")
  ))
heatingoil <- heatingoil[-c(1,2), ]  # remove 1st two rows (contain NA only)

# Convert month to Date format
heatingoil$month <- as.Date(heatingoil$month)

# Fill in missing months
heatingoil <- missing_months(heatingoil)

# Find earliest month in heating oil price data to use as basis for other datasets
startmonth <- as.Date(min(heatingoil$month)) 
endmonth <- as.Date(max(heatingoil$month)) 

######################################################################
#  LOAD NEW ENGLAND DIESEL PRICING DATA
######################################################################

# Define file name/location and import
file <- file.path("data", "psw18vwall.xls")
diesel <- as.data.frame(
  read_excel(file, 
             sheet = "Data 2",
             range = cell_cols("A:D"),
             col_names = c("month", "delete", "delete2", "diesel_per_gal"),
             col_types = c("date", "numeric", "numeric", "numeric")
  ))
diesel <- diesel[-c(1,3), ]  # remove 1st three rows (contain NA only)

# Delete middle two columns (data for other regions)
diesel <- dplyr::select(diesel, c(-delete, -delete2))

# Dates for diesel prices are currently for the 15th of the month
# Revise to be the 1st of the month to match other datasets
diesel$month <- as.Date(floor_date(as.Date(diesel$month), unit="months"))

# Remove dates with no pricing data (NA) at the beginning 
diesel <- diesel[!is.na(diesel$diesel_per_gal),]

# Filter diesel data to start/end at the same point as heating oil data
diesel <- diesel %>% 
  filter(month >= startmonth) %>% 
  filter(month <= endmonth)

######################################################################
#  IMPUTE MISSING HEATING OIL PRICING DATA
######################################################################

# Merge diesel prices with heating oil prices
heatingoil <- merge(heatingoil, diesel, by="month")
heatingoil$original <- heatingoil$heatingoil_per_gal

# Define linear regession model for relationship between heating oil and diesel prices
linear_mod <- lm(heatingoil_per_gal ~ diesel_per_gal, data=heatingoil)
#summary(linear_mod)

# Calculate imputed heating oil price value using linear model
heatingoil$imputed_per_gal <- linear_mod$coefficients[1] + 
  (heatingoil$diesel_per_gal * linear_mod$coefficients[2])

# Replace NA's with imputed values 
heatingoil$heatingoil_per_gal <- ifelse(is.na(heatingoil$heatingoil_per_gal),
       heatingoil$imputed_per_gal, heatingoil$heatingoil_per_gal)

# Compare
#head(heatingoil)
#plot(heatingoil$month, heatingoil$original, type="h", col="blue")
#lines(heatingoil$month, heatingoil$heatingoil_per_gal, col="red", lwd=3)

# Reduce dataframe to only contain month and heating oil price (incl imputed vals)
heatingoil <- dplyr::select(heatingoil, c(month, heatingoil_per_gal))


# Reallow warning messages
options(warn=warn)

# Remove extraneous objects
rm(list = c('linear_mod', 'file', 'list.of.packages', 
            'new.packages', 'warn', 'missing_months'))



