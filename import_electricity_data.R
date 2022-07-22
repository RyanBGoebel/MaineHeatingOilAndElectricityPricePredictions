###-------------------------------------------------------------------
### Roux Institute
### ALY6980 Spring 2022
### Capstone Project
###-------------------------------------------------------------------

# NOTE:  Prior to running this R file, the following data file(s)  
#        should already be located in the "data" directory (see User  
#        Guide for more information):
#        - "sales_revenue.xlsx"

######################################################################
#  SETUP
######################################################################

# Packages required for this markdown file
list.of.packages <- c("readxl", "dplyr")

# Check which packages from the list have not been installed on local machine,
# and install those which have not.
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(readxl)
library(dplyr)

# Set working directory to folder containing this .R file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Supress warning messages
warn = getOption("warn")
options(warn=-1)

######################################################################
#  LOAD DATA
######################################################################

# Define file name/location and import
file <- file.path("data", "sales_revenue.xlsx")
electricity <- as.data.frame(
  read_excel(file,
             sheet = "Monthly-States",
             range = cell_cols("A:H"),
             col_names = c("year", "month", "state", "data_status","revenue",
                           "sales", "customers", "electricity_per_kwh"),
             col_types = c("numeric", "numeric", "text", "text", "numeric",
                           "numeric", "numeric", "numeric")
             )
  )
electricity <- electricity[-c(1:3), ]  # remove 1st three rows
electricity <- electricity %>%
  filter(state == "ME") %>%            # keep only Maine data
  dplyr::select(-c(data_status, customers))   # remove unnecessary columns

# Convert cents per kwh to dollars per kwh
electricity$electricity_per_kwh <- electricity$electricity_per_kwh/100

# Remove all columns except year, month, and price
electricity <- electricity %>% dplyr::select(c(year, month, electricity_per_kwh))

# Combine year and month and dummy "01" day into date
electricity$combined_date = as.Date(paste0(electricity$year,"-",
                                           electricity$month,"-01"), tz="UTC")

# To match format of heating oil data, rename columns and drop unnecessary ones
electricity <- electricity[ , c("combined_date", "electricity_per_kwh")]
colnames(electricity) <- c("month", "electricity_per_kwh")

# Sort from least recent to most recent
electricity <- electricity %>% arrange(month)


# Reallow warning messages
options(warn=warn)

# Remove extraneous objects
rm(list = c('file', 'list.of.packages', 'new.packages','warn'))
