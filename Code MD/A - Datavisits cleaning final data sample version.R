#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  Data cleaning - Visits Dataframe
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#

## Elaboration date: 28/11/2020
## Last modification date: 28/11/2020

#-------------------------------------------------------------------------------------------------#
# Description:
# This script load the multiple datasets related to patient's visits at the catalan primary attention
# health facilities. Then, it cleans the data in order to use for further analysis.
# We only keep data from January 2019 to November 20th 2019 since that is the period we have for 2020.
# IMPORTANT: This version of the code loads the sample data for revision purposes.

# Notes:
# The dataset generated will be saved as .csv format.
#

#-------------------------------------------------------------------------------------------------#
##
##  LIBRARIES
##  
##
#-------------------------------------------------------------------------------------------------#
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("here")
library(here)
library(readxl)
library(dplyr)
library(data.table)
library(stringr)

rm(list = ls())

# Set directories of work
here::here()

#-------------------------------------------------------------------------------------------------#
##
##  Data reading
##  
##
#-------------------------------------------------------------------------------------------------#

data<-read.csv("Data/Data_visites_final/sample_data.csv", header=TRUE)

# 
# ## Face to face visits
# #-------------------------------------------------------------------------------------------------#
# data_1 <- read_excel(here("Data","Data_visites_final","Anoia 1.xlsx"))
# data_2 <- read_excel(here("Data","Data_visites_final","Anoia 2.xlsx"))
# data_3 <- read_excel(here("Data","Data_visites_final","Bages tot 1.xlsx"))
# data_4 <- read_excel(here("Data","Data_visites_final","Bages tot 2.xlsx"))
# # Bages tot 3_old  is spread out between excel pages
# data_5_1 <- read_excel(here("Data","Data_visites_final","Bages tot 3_old.xlsx"), sheet = "dades")
# data_5_2 <- read_excel(here("Data","Data_visites_final","Bages tot 3_old.xlsx"), sheet = "dades 2")
# data_5_3 <- read_excel(here("Data","Data_visites_final","Bages tot 3_old.xlsx"), sheet = "dades 3")
# data_5_4 <- read_excel(here("Data","Data_visites_final","Bages tot 3_old.xlsx"), sheet = "dades 4")
# data_5_5 <- read_excel(here("Data","Data_visites_final","Bages tot 3_old.xlsx"), sheet = "dades 5")
# # Append all data of Bages tot 3
# data_5 <- rbind(data_5_1, data_5_2, data_5_3, data_5_4, data_5_5)
# rm(list = c(paste("data_5_",1:5,sep="")))
# 
# data_6 <- read_excel(here("Data","Data_visites_final","OSona 1.xlsx"))
# data_7 <- read_excel(here("Data","Data_visites_final","OSona 2.xlsx"))
# data_8 <- read_excel(here("Data","Data_visites_final","OSona 3.xlsx"))
# 
# ## Telemedicine visits
# #-------------------------------------------------------------------------------------------------#
# data_9 <- read_excel(here("Data","Data_visites_final","Anoia 1 tel.xlsx"))
# data_10 <- read_excel(here("Data","Data_visites_final","Anoia 2 tel.xlsx"))
# data_11 <- read_excel(here("Data","Data_visites_final","Bages 1 tel.xlsx"))
# data_12 <- read_excel(here("Data","Data_visites_final","Bages 2 tel.xlsx"))
# data_13 <- read_excel(here("Data","Data_visites_final","Bages 3 tel.xlsx"))
# data_14 <- read_excel(here("Data","Data_visites_final","OSona 1 tel.xlsx"))
# data_15 <- read_excel(here("Data","Data_visites_final","OSona 2 tel.xlsx"))
# data_16 <- read_excel(here("Data","Data_visites_final","OSona 3 tel.xlsx"))
# 
# 
# # Append datasets (simple raw bind)
# data_raw <- rbind(data_1, data_2, data_3, data_4, data_5, data_6, data_7, data_8,
#                   data_9, data_10, data_11, data_12, data_13, data_14, data_15, data_16)
# 
# rm(list = c(paste("data_",1:16,sep="")))# I drop the partial datasets
# # Keeping only the relevant variables for the short detection article
# data<-subset(data_raw, select=c("usua_cip","sap","upd","centred","any_naix","usua_sexe","visi_data_visita","visi_tipus_visita",
#                                 "diagnostics_actius","diagnostics_visita"))
# rm(data_raw) # I drop the partial datasets


#-------------------------------------------------------------------------------------------------#
##
##  Data management
##  
##
#-------------------------------------------------------------------------------------------------#

## Dates
#-------------------------------------------------------------------------------------------------#
# We only keep data for 2019 up to November 2020.
# Declaring dates
data$visi_data_visita<-as.Date(data$visi_data_visita,format="%Y-%m-%d")
# Split data by years
data_2020 <- data[which(year(data$visi_data_visita)==2020),]
data_2019 <- data[which(year(data$visi_data_visita)==2019),]
# Data for 2020 is up to 20/11/2020 and full year for 2019
#max(data_2020$visi_data_visita)
#max(data_2019$visi_data_visita)
# Select data in 2019 up to 20/11/2020
data_2019_restricted <- data_2019[which(as.Date(data_2019$visi_data_visita) <= as.Date('2019-11-20',tz = "UTC",format="%Y-%m-%d")),]
#max(data_2019_restricted$visi_data_visita) #checking it worked
data<-rbind(data_2019_restricted,data_2020)
rm(data_2020,data_2019,data_2019_restricted)

## Generate an id per user and visit
#-------------------------------------------------------------------------------------------------#
data$tot <- 1
data2 <- data %>%
  group_by(usua_cip) %>%
  summarize(sum_visits = sum(tot))
data2$tot <- 1
data2$id <- rowid(data2$tot)
data2 <- merge(data, data2, by.x="usua_cip", by.y="usua_cip")
# Sort the data considering users and date of visit
data2 <- data2[order(data2$usua_cip, data2$visi_data_visita),] # Sorting the data with respect to the patient id and visit date
# Generate an id per visits for each user
data2$id2 <- rowidv(data2, cols=c("id"))

## Clean the variables of active and visit diagnostic
#-------------------------------------------------------------------------------------------------#
# Removing other symbols within the ICD10 codes.
data2$diagnostics_actius<-str_replace_all(data2$diagnostics_actius, pattern = c('C01-'="", "[.]"=""))
data2$diagnostics_visita<-str_replace_all(data2$diagnostics_visita, pattern = c('C01-'="","[.]"=""))

## Cleaning ABS names
#-------------------------------------------------------------------------------------------------#
# Removing "EAP" from ABS names
data2$upd<-str_replace_all(data2$upd, pattern = c('EAP'=""))
# Removing the blank space in front of it
data2$upd<-trimws(data2$upd, "l")
# Removing variables generated in the process
data2 <- data2[c(-11:-13)]
# Ordering the variables in the dataframe
data2 <- data2[c(11,12,1:10)]

#-------------------------------------------------------------------------------------------------#
##
##  Data validation
##  
##
#-------------------------------------------------------------------------------------------------#

# Are we missing anything?
# Mirant que hi siguin tots els tipus de visites en tots els equips en tots els mesos, per exemple

#================================================================================================#
# Data storage
#================================================================================================#
# Save the data set in .csv format
write.csv(data2,here("Data","Data_results","DT_1.csv"), row.names = FALSE, na="")


#========================================== END ==================================================#