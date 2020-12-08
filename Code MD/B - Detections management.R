#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  Identification of new diagnostics
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##

## Elaboration date: 09/11/2020
## Last modification date: 28/11/2020
## 
###################################################################################################
# Description:

# Identifies new diagnostics at different levels of aggregation.


# Notes:
# 
#
#-------------------------------------------------------------------------------------------------#
##
##  LIBRARIES
##  
##
#-------------------------------------------------------------------------------------------------#
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("icd")
#install.packages("icd.data")
#install.packages("data.table")
#install.packages("tictoc")
#install.packages("icd.coder")
#install.packages("tictoc")
#install.packages("ggplot2")

library(here)
library(readxl)
library(dplyr)
library(data.table)
library(icd)
library(stringr)
library(icd.data)
library(devtools) 
library(icdcoder)
library(data.table)
library(tictoc)

rm(list = ls())
#-------------------------------------------------------------------------------------------------#
##
##  READING DATA
##  
##
#-------------------------------------------------------------------------------------------------#
# Set directories of work
here::here()

# Open dataset
data <- read.csv(here("Data","Data_results","DT_1.csv"))



#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTICS ANALYSIS
##  Detecting diagnostics
##
#-------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# First method of detection: detection by visit and medical record
#-----------------------------------------------------------------------------#
# Keep the dataset with the detected 
tmp1 <- data

# Generate the lists of active and visit diagnostic 
active <- strsplit(tmp1$diagnostics_actius, "#")
visit <- strsplit(tmp1$diagnostics_visita, "#")

# Generate a logical list of new causes detected
# Important: causes misscoded (i.e. case 60)
# Important: there are repited causes in a diagnostic(i.e. case 150)
# This returns the list of causes that are different between the visit and active record of the patient.
causes_v1 <- lapply(1:nrow(tmp1), function(x) setdiff(unlist(visit[x]),unlist(active[x])))

#-----------------------------------------------------------------------------#
# Second method of detection: detection through medical record only
#-----------------------------------------------------------------------------#
# Generate my lag variable to compare one by one 
tmp1$lag_active <- sapply(1:nrow(tmp1), function(x) tmp1$diagnostics_actius[x-1])
tmp1$lag_active <- ifelse(tmp1$id2==1, "", tmp1$lag_active)
# Generate my lists of the variable of medical record and its lag 
tmp1$active_1 <- ifelse(tmp1$id2==1, "", tmp1$diagnostics_actius)
active_1 <- strsplit(as.character(tmp1$active_1), "#")
active_lag <- strsplit(as.character(tmp1$lag_active), "#")
# This returns the list of causes that are different between visits.
causes_a2 <- lapply(1:nrow(tmp1), function(x) setdiff(unlist(active_1[x]),unlist(active_lag[x])))
causes_a1 <- sapply(1:length(causes_a2), function(x) causes_a2[x+1])

#-----------------------------------------------------------------------------#
# Put both methods togther
#-----------------------------------------------------------------------------#
# Merge both lists, diagnostics identified by visit and by medical records
causes_tot1 <- lapply(1:length(causes_v1), function(x) do.call(c, list(causes_v1[[x]], causes_a1[[x]])))
# Drop duplicates diagnostics
causes_tot <- lapply(1:length(causes_tot1), function(x) unique(unlist(causes_tot1[x])))

rm(active_1, active_lag, causes_a2, causes_tot1)

#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTICS ANALYSIS
##  Aggregating causes
##
#-------------------------------------------------------------------------------------------------#

# This part reads the new diagnostics codes and transforms them to chapter level codes.
# Reading our ICD codebook
icd10_codebook<-read.csv("Data/Data_results/icd10_codebook.csv")

#=========================================================================#
# ALternarnative? kind of inefficient (time: 668.46 sec elapsed) 
# Note: There are some codes that are not in the icd10_codebook -- nursing codes.
#==========================================================================#
# Medical Records
tic("time")
causes_agg <- causes_tot
causes_agg <- lapply(1:length(causes_agg), function(x) icd10_codebook$chp_id[match(unlist(causes_agg[x]), icd10_codebook$code)])
toc()
#=====================================================#
# Introduce the lists of causes and chapters into the dataframe
data2 <- tmp1
active_dd <- lapply(1:length(causes_a1), function(x) paste(causes_a1[[x]],collapse="#"))
visit_dd <- lapply(1:length(causes_v1), function(x) paste(causes_v1[[x]],collapse="#"))
causes_dd <- lapply(1:length(causes_tot), function(x) paste(causes_tot[[x]],collapse="#"))
chapter_dd <- lapply(1:length(causes_agg), function(x) paste(causes_agg[[x]],collapse="#")) 

h1 <- as.data.frame.character(active_dd)
h2 <- as.data.frame.character(visit_dd)
h3 <- as.data.frame.character(causes_dd)
h4 <- as.data.frame.character(chapter_dd)

data2 <- cbind.data.frame(data2, h1, h2, h3, h4)

# Clean the dataframe from temporary columns
names(data2)
data2 <- data2[c(-13,-14)]
# Count new causes detected 
data2$new_causes <- ifelse(data2$causes_dd!="",1,0)
data2$count_causes <- lapply(1:length(causes_tot), function(x) length(causes_tot[[x]]))
table(data2$new_causes)
prop.table(table(data2$new_causes))

names(data2)

# Save dataframe
data2 <- apply(data2,2,as.character)

write.csv(data2,here("Data","Data_results","DT_2.csv"), row.names = FALSE, na="")

#======================================== END ======================================================#
