###-------------------------------------------------------------------
### Roux Institute
### ALY6980 Spring 2022
### Capstone Project
###-------------------------------------------------------------------

# NOTE:  Prior to running this R file, the following data file(s)  
#        should already be located in the "data" directory (see User  
#        Guide for more information):
#        - "17-tavg-all-12-2004-2022.csv" (or similar)
#        - Multiple files named"Download Data - INDEX_US_S&P US_SPX*****.csv"
#        - "RWTCm.xls"
#        - "Global_Policy_Uncertainty_Data.xlsx"
#        - "data_gpr_export.xls"

######################################################################
#  SETUP
######################################################################

# Packages required for this markdown file
list.of.packages <- c("readxl", "dplyr", "lubridate")

# Check which packages from the list have not been installed on local machine,
# and install those which have not.
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(readxl)
library(dplyr)
library(lubridate)

# Set working directory to folder containing this .R file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Supress warning messages
warn = getOption("warn")
options(warn=-1)

######################################################################
#  LOAD WEATHER DATA
######################################################################

# Define file name/location and import
file <- file.path(dir("data", full.names=T)[grep("tavg-all", dir("data"))])
weather <- read.csv(file, header=TRUE)
weather <- weather[-c(1:3), ]  # remove 1st three rows 
colnames(weather) <- c("month", "avgtemp") #rename columns

# Convert months to dates
weather$month <- ymd(paste0(weather$month, "01"))

# Convert avgtemp to numeric
weather$avgtemp <- as.numeric(weather$avgtemp)

######################################################################
#  LOAD S&P500 DATA
######################################################################

# Define file names/location, import, and combine
sp500 <- list.files(path="data", 
                 full.names = TRUE)[grep("Download Data - INDEX_US_S&P",
                                         dir("data"))] %>%
  lapply(read.csv) %>%
  bind_rows

# Format columns
sp500$Date <- as.Date(sp500$Date, format = "%m/%d/%Y")
sp500$Open <- as.numeric(gsub(",","", sp500$Open))
sp500$High <- as.numeric(gsub(",","", sp500$High))
sp500$Low <- as.numeric(gsub(",","", sp500$Low))
sp500$Close <- as.numeric(gsub(",","", sp500$Close))     

# Sort order
sp500 <- sp500 %>% arrange(Date)

# Consolidate by month using average Close value
sp500 <- sp500 %>% group_by(month=floor_date(Date, "month")) %>%
  summarize(avg_close = mean(Close))

######################################################################
#  LOAD CRUDE OIL (WTI) PRICING DATA
######################################################################

# Define file name/location and import
file <- file.path("data", "RWTCm.xls") 
crude <- as.data.frame(
  read_excel(file, 
             sheet = "Data 1",
             range = cell_cols("A:B"),
             col_names = c("month", "WTI_per_bbl"),
             col_types = c("date", "numeric")
  )
)
crude <- crude[-c(1:3), ]  # remove 1st 3 rows 
crude$month <- floor_date(crude$month, "month")  # use 1st of the month for all months
crude$month <- as.Date(crude$month) # convert month to date format


######################################################################
#  LOAD GLOBAL ECONOMIC POLICY UNCERTAINTY INDEX (GEPU) DATA
######################################################################

# Define file name/location and import
file <- file.path("data", "Global_Policy_Uncertainty_Data.xlsx")
gepu <- as.data.frame(read_excel(file))

# Delete rows with NA data
gepu <- gepu[!is.na(gepu$GEPU_ppp),]

# combine year and month into date
gepu$month <- as.Date(paste0(gepu$Year, "-", gepu$Month, "-01"))

# Delete columns for Year, Month, and GEPU_current
gepu <- gepu %>% dplyr::select(-Year, -Month, -GEPU_current)

# Rename columns
colnames(gepu) <- c("GEPU", "month")

# Reorder columns
col_order <- c("month", "GEPU")
gepu <- gepu[ ,col_order]


######################################################################
# IMPORT GEOPOLITICAL RISK INDEX (GPR) DATA 
######################################################################

# Define file name/location and import
file <- file.path("data", "data_gpr_export.xls")
gpr <- as.data.frame(read_excel(file, range = cell_cols("A:B"),
                                col_types = c("date", "numeric")
))
gpr$month <- as.Date(gpr$month) # Convert month to Date format

# Reallow warning messages
options(warn=warn)

# Remove extraneous objects
rm(list = c('file', 'list.of.packages', 'new.packages', 'col_order', 'warn'))
