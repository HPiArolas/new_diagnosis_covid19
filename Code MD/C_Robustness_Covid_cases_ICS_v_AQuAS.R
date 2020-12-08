#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  Covid-19 cases in ICS and AQUAS
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#

# Description:

# Creates a monthly panel at the ABS level for summary statistics of visits [for COVID cases]
# and supplementary information.
# [Note that the supplementary information it is constant over time (based on 2019 data).]


# Notes:
# Dates are already in as.Date format.

#-------------------------------------------------------------------------------------------------#
##
##  LIBRARIES
##  
##
#-------------------------------------------------------------------------------------------------#
#install.packages("icdcoder")
library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(stringr)
library(data.table)
library(stringr)
library(forcats)
library(car)

#-------------------------------------------------------------------------------------------------#
##
##  DATA READING
##  
##
#-------------------------------------------------------------------------------------------------#

# Set directories of work
here::here()
#Aseguranos casos reportados en ése momento y no los acumulados hasta ése momento del tiempo.

rm(list = ls())


## ABS yearly extra data
#-------------------------------------------------------------------------------------------------#
data_from_AQUAS_raw <- read.csv(here("Data", "Data_covid_19", "Registre_de_casos_de_COVID-19_realitzats_a_Catalunya._Segregaci__per_sexe_i__rea_b_sica_de_salut__ABS_.csv"),
                                fileEncoding="UTF-8-BOM", header = T, sep = ",")

dcabs <- data_from_AQUAS_raw #changing dataset name
names(dcabs)

#=========================================================================================#
# Dataset treatment 
#=========================================================================================#
# Preparing date variables
# Generate a visit date variable based on the dataframe
dcabs$visit_date <- dcabs$TipusCasData #changing variable name

# Split the date variable into year and month
dcabs$month <- month(as.POSIXlt(dcabs$visit_date, format="%d/%m/%Y"))
dcabs$year <- year(as.POSIXlt(dcabs$visit_date, format="%d/%m/%Y"))

#Verifying distribution
table(dcabs$month) #From 2 to 11 (February to November)
round(prop.table(table(dcabs$month)),3) #From 2 to 11 (February to November)
table(dcabs$year) #All obs in year 2020
# Note: Nov seems not to be complete.

# Using upped case characters check locale 
dcabs$abs_name <- str_to_upper(dcabs$ABSDescripcio)

#=========================================================================================#
# Panel Structure 
#=========================================================================================#
# Dimensions
## ABS in cases
# Creates a list of unique ABS included in the data
abs_aquas <- unique(dcabs$abs_name)
## Months and years included
list_months <- as.double(unique(dcabs$month))
list_years <- as.double(unique(dcabs$year))

# Rows: ABS x months x years
# Columns: ID for ABS, name of ABS, month, year
abs.panel_str <- data.frame(abs_id=matrix(NA, length(abs_aquas)*length(list_months)*length(list_years), 1))
# Creating a numeric ID to abs 
setDT(dcabs)[, abs_id:= .GRP, by = dcabs$abs_name]
# Adding it to the panel
abs.panel_str$abs_id <- sort(rep(unique(dcabs$abs_id),length(list_months)*length(list_years)),
                             decreasing=FALSE)
# Keeping the ABS name
abs.panel_str$abs_name <- sort(rep(unique(dcabs$abs_name),length(list_months)*length(list_years)),
                               decreasing=FALSE)
# Month and year variables -- as many reps of the first part as years of data.
abs.panel_str$month <- rep((1:length(list_months)+1),length(abs_aquas))
# Years - need to input manually the years considered
abs.panel_str$year <- rep(2020,length(list_months),length(abs_aquas))

#=========================================================================================#
# Generate the panel of covid causes
#=========================================================================================#
# Collapse the dataset in terms of ABS, year and month 
abs.panel_aquas <- dcabs %>% 
  group_by(abs_name,year,month) %>% 
  summarise(cases_aquas = sum(NumCasos, na.rm = TRUE))

# Merge the abs.panel_aquas with the abs.panel_str
abs.panel_aquas <- merge(x = abs.panel_str, y = abs.panel_aquas, by=c('abs_name','year','month'), all=TRUE)

# Compare the abs_names with  data_visits abs_names
abs.panel_visits <- readRDS(here("Data", "Data_results","panel_detections_abs_covid19.RDS"))
abs.panel_visits <- abs.panel_visits[-c(1)]
abs_visit <- unique(abs.panel_visits$abs_name)
setdiff(abs_visit,abs_aquas)

# Change ABS names of aquas considering the Visits ABS names
abs.panel_aquas$abs_name <- str_replace_all(abs.panel_aquas$abs_name, 
                                            c("BERGA" = "BERGA CENTRE", 
                                              "MANRESA 2" = "MANRESA 2 PLAÇA CATALUNYA", 
                                              "MANRESA 4" = "MANRESA 4 SAGRADA FAMÍLIA", 
                                              "NAVARCLES- SANT FRUITÓS DE BAGES" = "NAVARCLES - SANT FRUITÓS DE BAGES",
                                              "NAVÀS/BALSARENY" = "NAVÀS - BALSARENY", 
                                              "IGUALADA1" = "IGUALADA URBÀ",
                                              "SANTA EUGÈNIA DE BERGA CENTRE" = "SANTA EUGÈNIA DE BERGA",
                                              "VALL DEL GES" = "LA VALL DEL GES - TORELLÓ"))

# Double check
abs_aquas <- unique(abs.panel_aquas$abs_name)
setdiff(abs_visit,abs_aquas)

# Merge the abs.panel_aquas and abs.panel_visits 
abs.panel_covid <- merge(abs.panel_visits, abs.panel_aquas, by=c('abs_name', 'year', 'month'), all.x = TRUE)

# Clean the abs.panel_covid
abs.panel_covid <- abs.panel_covid[-c(9)]
colnames(abs.panel_covid)[8] <- "cases_visits"

# Correlation
plot(abs.panel_covid$cases_visits ~ abs.panel_covid$cases_aquas)
regLine(lm(abs.panel_covid$cases_visits ~ abs.panel_covid$cases_aquas))

# Comparison monthly incidence for 2020
covid19_monthly <- abs.panel_covid[which(abs.panel_covid$year==2020),] %>%
  group_by(month) %>%
  summarize_at(c("cases_visits",'cases_aquas'), funs(sum = sum(., na.rm = T)))
# Only consider 2020 cases, since the general code for covid B342 is also used for other conditions in 2019.


# Storage ABS.panel_covid
#saveRDS(abs.panel_covid,here("Data","Data_results","panel_detections_abs_covid19.RDS"))
#abs.panel_covid <- readRDS("Data/Data_results/panel_detections_abs_covid19.RDS")
#======================================= END ===========================================================#
