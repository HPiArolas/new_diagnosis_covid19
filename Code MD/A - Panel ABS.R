#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  Monthly panel ABS
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#

# Description:

# Creates a monthly panel at the ABS level for summary statistics of visits and supplementary information.
# Note that the supplementary information it is constant over time (based on 2019 data).


# Notes:
# Dates are already in as.Date format.

#-------------------------------------------------------------------------------------------------#
##
##  LIBRARIES
##  
##
#-------------------------------------------------------------------------------------------------#

library(readxl)
library(here)
library(stringr)
library(data.table)

#-------------------------------------------------------------------------------------------------#
##
##  DATA READING
##  
##
#-------------------------------------------------------------------------------------------------#

## Visits data
#-------------------------------------------------------------------------------------------------#
# Set directories of work
here::here()
# Open dataset
data_visits <- read.csv(here("Data","Data_results","DT_1.csv"))


data_2019 <- data_visits[which(year(data_visits$visi_data_visita)==2019),]
data_2020 <- data_visits[which(year(data_visits$visi_data_visita)==2020),]
max(data_2019$visi_data_visita)

#-------------------------------------------------------------------------------------------------#
##
##  PANEL FRAMEWORK
##  
##
#-------------------------------------------------------------------------------------------------#

## Dimensions
#-------------------------------------------------------------------------------------------------#
## ABS in visits
# Creates a list of unique ABS included in the data
list_abs_visits<-unique(data_visits$upd)
## Months and years included
list_months_visits<-unique(format(as.Date(data_visits$visi_data_visita), "%m"))
list_years_visits<-unique(format(as.Date(data_visits$visi_data_visita), "%Y"))

## Panel structure
#-------------------------------------------------------------------------------------------------#
# Rows: ABS x months x years
# Columns: ID for ABS, name of ABS, month, year
panel_abs_visits<-data.frame(abs_id=matrix(NA, length(list_abs_visits)*length(list_months_visits)*length(list_years_visits), 1))
# Creating a numeric ID to abs 
setDT(data_visits)[, abs_id:= .GRP, by = data_visits$upd]
# Adding it to the panel
panel_abs_visits$abs_id<-sort(rep(unique(data_visits$abs_id),length(list_months_visits)*length(list_years_visits)),
                              decreasing=FALSE)
# Keeping the ABS name
panel_abs_visits$abs_name<-sort(rep(unique(data_visits$upd),length(list_months_visits)*length(list_years_visits)),
                              decreasing=FALSE)
# Month and year variables -- as many reps of the first part as years of data.
panel_abs_visits$month<-rep(c(rep(1:length(list_months_visits)),rep(1:length(list_months_visits))),length(list_abs_visits))
# Years - need to input manually the years considered
panel_abs_visits$year<-rep(c(rep(2019,length(list_months_visits)),rep(2020,length(list_months_visits))),length(list_abs_visits))

## Storing panel structure
#-------------------------------------------------------------------------------------------------#
saveRDS(panel_abs_visits,"Data/Data_results/panel_abs_placeholder.RDS")



