#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  Final data for plots
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
## Elaboration date: 29/11/2020
## Last modification date: 29/11/2020

#-------------------------------------------------------------------------------------------------#

# Description:
# This script compiles the necessary data to create the plots for the short article
# It includes:
# i) Incidence Covid-19 cases by ABS and yearly
# ii)

# Notes:



#-------------------------------------------------------------------------------------------------#
##
##  LIBRARIES
##  
##
#-------------------------------------------------------------------------------------------------#
#install.packages("stringdist")
#install.packages("tidyverse")
library(here)
library(readxl)
library(dplyr)
library(data.table)
library(tidyverse)
library(stringdist)
library(rlang)
library(RColorBrewer)
library(rgdal)
library(ggplot2)
library(aplot)

#.................................................................................................
rm(list = ls())

#-------------------------------------------------------------------------------------------------#
##
##  FUNCTIONS
##  Filtering funciton for chapters of causes
##
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
##
##  READING DATA
##  
##
#-------------------------------------------------------------------------------------------------#

# ABS panel without covid cases
abs.panel_chap_no_cov<-readRDS("Data/Data_results/panel_detections_abs_chap_nocovid.RDS")
# ABS panel covid cases 
abs.panel_covid<-readRDS("Data/Data_results/panel_detections_abs_covid19.RDS")
# Renaming cases_aquas as covid, since this is the definitive version we use (column 9)
colnames(abs.panel_covid)[[8]]<-'covid'
# Consider only cases starting only in march
abs.panel_covid$covid<-ifelse((abs.panel_covid$year==2020&abs.panel_covid$month<3)|abs.panel_covid$year==2019,0,abs.panel_covid$covid)

#-------------------------------------------------------------------------------------------------#
##
##  DATA MANAGEMENT
##  
##
#-------------------------------------------------------------------------------------------------#

# Done in a prior set for this version

#-------------------------------------------------------------------------------------------------#
##
##  PART 2: PANEL OF DETECTIONS & COVID-19 BY ABS; YEARLY SUMMARIES BY ABS
##  
##
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
## COVID-19 detections panel by ABS by 100k 
#-------------------------------------------------------------------------------------------------#

## COVID-19 detections panel by ABS
#-------------------------------------------------------------------------------------------------#
covid19_yearly <- abs.panel_covid %>%
  group_by(abs_name,year) %>%
  summarize_at(c("covid"), funs(sum = sum(., na.rm = T)))
names(covid19_yearly)[names(covid19_yearly) == "sum"] <- 'covid19'
# Only consider 2020 cases, since the general code for covid B342 is also used for other conditions in 2019.
covid19_yearly<-covid19_yearly[which(covid19_yearly$year==2020),]

## COVID-19 detections as 100k pop
#-------------------------------------------------------------------------------------------------#
# Now adding population data
data_abs_marc <-readRDS("Data/Data_results/data_abs_extra.RDS")
data_marc <- data_abs_marc
# Capitalizing ABS names to match
data_marc$abs_name <- toupper(data_marc$abs_name)
# Keep my interest variables
data_marc <- subset(data_marc, select=c('abs_name','poblacio'))
# Check differences in names
setdiff(unique(covid19_yearly$abs_name),data_marc$abs_name)
# Change names to match our dataset
#data_marc$abs_name[data_marc$abs_name=="ARTÈS"] <- "ARTÉS"
data_marc$abs_name[data_marc$abs_name=="MANRESA 2"] <- "MANRESA 2 PLAÇA CATALUNYA"
data_marc$abs_name[data_marc$abs_name=="MANRESA 4"] <- "MANRESA 4 SAGRADA FAMÍLIA"
data_marc$abs_name[data_marc$abs_name=="BERGA"] <- "BERGA CENTRE"
data_marc$abs_name[data_marc$abs_name=="VALL DEL GES"] <- "LA VALL DEL GES - TORELLÓ"
data_marc$abs_name[data_marc$abs_name=="VIC-1 NORD"] <- "VIC 1 NORD"
data_marc$abs_name[data_marc$abs_name=="IGUALADA-1"] <- "IGUALADA URBÀ"
data_marc$abs_name[data_marc$abs_name=="NAVÀS/BALSARENY"] <- "NAVÀS - BALSARENY"
# Merge with my data 
yearly_covid_abs_pop <-  Reduce(function(x, y) merge(x, y, by=c('abs_name')), list(covid19_yearly, data_marc))
# Generate covid/pop
yearly_covid_abs_pop$covid19_100k <- (yearly_covid_abs_pop$covid19/yearly_covid_abs_pop$poblacio)*100000 

## COVID-19 detections as 100k pop summing over ABS for yearly total
#-------------------------------------------------------------------------------------------------#
# Data frame to store
totals_covid19_100k<-data.frame(year='2020')
# Summing covid19 incidence and population over ABS
totals_covid19_100k$covid19<-sum(yearly_covid_abs_pop$covid19)
totals_covid19_100k$population<-sum(yearly_covid_abs_pop$poblacio)
# Generate covid/pop
totals_covid19_100k$covid19_100k <- (totals_covid19_100k$covid19/totals_covid19_100k$population)*100000 


## COVID-19 detections as 100k pop monthly incidence
#-------------------------------------------------------------------------------------------------#
# Simply group by abs and year
covid19_monthly <- abs.panel_covid %>%
  group_by(month) %>%
  summarize_at(c("covid"), funs(sum = sum(., na.rm = T)))
names(covid19_monthly)[names(covid19_monthly) == "sum"] <- 'covid19'
# Creating incidence
covid19_monthly$covid19_100k <- (covid19_monthly$covid19/totals_covid19_100k$population)*100000 

#-------------------------------------------------------------------------------------------------#
## Other detections panel by ABS yearly
#-------------------------------------------------------------------------------------------------#

## Other detections panel by ABS
#-------------------------------------------------------------------------------------------------#

# Summing over all chapters
abs.panel_chap_no_cov$others<-rowSums(abs.panel_chap_no_cov[,4:ncol(abs.panel_chap_no_cov)])

other_detec <- abs.panel_chap_no_cov %>%
  group_by(abs_name, year) %>%
  summarize_at(c("others"), funs(sum = sum(., na.rm = T)))
colnames(other_detec) <- c("abs_name","year","others")

#-------------------------------------------------------------------------------------------------#
##
##  PART 3: PROPORTIONAL CHANGE IN DETECTIONS AND COVID BY ABS AND YEAR
##  
##
#-------------------------------------------------------------------------------------------------#

## Proportional change in other diagnostics
#-------------------------------------------------------------------------------------------------#
# Total detections yearly by ABS -- 2019 and 2020
other_detec_2019<-other_detec[which(other_detec$year==2019),]
# 2020
other_detec_2020<-other_detec[which(other_detec$year==2020),]
# Others proportional change
other_detect_change<-data.frame(abs_name=unique(other_detec_2020$abs_name))
# Change in diagnostics
other_detect_change$overall_dd_change<-other_detec_2020[,3]/other_detec_2019[,3]

## Adding covid-19 incidence
#-------------------------------------------------------------------------------------------------#
other_detect_change$covid19_100k<-yearly_covid_abs_pop$covid_100k

## Storing
#-------------------------------------------------------------------------------------------------#
saveRDS(other_detect_change,"Data/Data_results/abs_overall_change_dd_covid19")

#-------------------------------------------------------------------------------------------------#
##
##  PART 4: PANEL OF DETECTIONS MONTHLY GLOBAL BY CHAPTER
##  
##
#-------------------------------------------------------------------------------------------------#

## Aggregating across ABS by chapter (full year)
#-------------------------------------------------------------------------------------------------#
# List of chapters
list_ch <- c(paste("chp",seq(1:21),sep=""))
# Summing up across ABS by month / year and chapter.
bla <- abs.panel_chap_no_cov %>%
  group_by(year, month) %>%
  summarize_at(list_ch, funs(sum = sum(., na.rm = T)))
# Yearly detections by chapter 
det_abs_chp_2019<-bla[which(bla$year==2019),]
det_abs_chp_2020<-bla[which(bla$year==2020),]
# Proportional cases by chapter
det_abs_chp_change<-bla[which(bla$year==2020),3:23]/bla[which(bla$year==2019),3:23]

## Aggregating chapter changes over the year for all ABS
#-------------------------------------------------------------------------------------------------#
# Sum over months by chapter
# 2019
chapter_yearly_2019 <- abs.panel_chap_no_cov[which(abs.panel_chap_no_cov$year==2019),] %>%
  group_by(year) %>%
  summarize_at(list_ch, funs(sum = sum(., na.rm = T)))
# 2020
chapter_yearly_2020 <- abs.panel_chap_no_cov[which(abs.panel_chap_no_cov$year==2020),] %>%
  group_by(year) %>%
  summarize_at(list_ch, funs(sum = sum(., na.rm = T)))
# Proportional cases by chapter yearly
det_abs_chp_change_yearly<-chapter_yearly_2020[,2:ncol(chapter_yearly_2020)]/chapter_yearly_2019[,2:ncol(chapter_yearly_2019)]

## Aggregating chapter changes over the year by ABS
#-------------------------------------------------------------------------------------------------#
# 2019
chapter_yearly_2019_ABS <- abs.panel_chap_no_cov[which(abs.panel_chap_no_cov$year==2019),] %>%
  group_by(year,abs_name) %>%
  summarize_at(list_ch, funs(sum = sum(., na.rm = T)))
# 2020
chapter_yearly_2020_ABS <- abs.panel_chap_no_cov[which(abs.panel_chap_no_cov$year==2020),] %>%
  group_by(year,abs_name) %>%
  summarize_at(list_ch, funs(sum = sum(., na.rm = T)))
# Proportional cases by chapter yearly and ABS
# Picking ABS names
det_abs_chp_change_yearly_abs_1<-chapter_yearly_2020_ABS[,2]
# Picking proportional change
det_abs_chp_change_yearly_abs_2<-chapter_yearly_2020_ABS[,3:ncol(chapter_yearly_2020_ABS)]/
                               chapter_yearly_2019_ABS[,3:ncol(chapter_yearly_2019_ABS)]

# Column binding
det_abs_chp_change_yearly_abs<-cbind(det_abs_chp_change_yearly_abs_1,det_abs_chp_change_yearly_abs_2)

## Overall (all chapters) monthly new diagnostics (aggregate, not by ABS)
#-------------------------------------------------------------------------------------------------#
# 2019
chapter_monthly_2019 <- abs.panel_chap_no_cov[which(abs.panel_chap_no_cov$year==2019),] %>%
  group_by(month) %>%
  summarize_at(list_ch, funs(sum = sum(., na.rm = T)))
# 2020
chapter_monthly_2020 <- abs.panel_chap_no_cov[which(abs.panel_chap_no_cov$year==2020),] %>%
  group_by(month) %>%
  summarize_at(list_ch, funs(sum = sum(., na.rm = T)))

# Now summing across chapters - 2019
over_monthly_others_2019<-data.frame(month=chapter_monthly_2019$month)
over_monthly_others_2019$others_counts<-rowSums(chapter_monthly_2019[,2:ncol(chapter_monthly_2019)])
# 2020
over_monthly_others_2020<-data.frame(month=chapter_monthly_2020$month)
over_monthly_others_2020$others_counts<-rowSums(chapter_monthly_2020[,2:ncol(chapter_monthly_2020)])
# Proportional change by month
# Picking month
det_others_change_monthly_1<-over_monthly_others_2019[,1]
# Picking proportional change
det_others_change_monthly_2<-over_monthly_others_2020[,2:ncol(over_monthly_others_2020)]/
  over_monthly_others_2019[,2:ncol(over_monthly_others_2019)]
# Column binding
det_others_change_monthly<-data.frame(month=det_others_change_monthly_1,others_change=det_others_change_monthly_2)

#-------------------------------------------------------------------------------------------------#
##
##  PART 5: GEOMAPS
##  
##
#-------------------------------------------------------------------------------------------------#

# Altering the shape file to match our names
# Open the shape files
cat_abs = readOGR(dsn = here("Shapes"), layer = "ABS_2018")
# Capitalizing ABS names to match
cat_abs$NOMABS<-toupper(cat_abs$NOMABS)
# Check differences in names
setdiff(unique(abs.panel_chap_no_cov$abs_name),cat_abs$NOMABS)
# Change names to match our dataset
#names_abs_geomap<-data.frame(unique(cat_abs$NOMABS))
cat_abs$NOMABS[cat_abs$NOMABS=="ARTÈS"] <- "ARTÉS"
cat_abs$NOMABS[cat_abs$NOMABS=="MANRESA - 2"] <- "MANRESA 2 PLAÇA CATALUNYA"
cat_abs$NOMABS[cat_abs$NOMABS=="MANRESA - 4"] <- "MANRESA 4 SAGRADA FAMÍLIA"
cat_abs$NOMABS[cat_abs$NOMABS=="BERGA"] <- "BERGA CENTRE"
cat_abs$NOMABS[cat_abs$NOMABS=="LA VALL DEL GES"] <- "LA VALL DEL GES - TORELLÓ"
cat_abs$NOMABS[cat_abs$NOMABS=="VIC - 1 NORD"] <- "VIC 1 NORD"
cat_abs$NOMABS[cat_abs$NOMABS=="IGUALADA - 1"] <- "IGUALADA URBÀ"
cat_abs$NOMABS[cat_abs$NOMABS=="SANT HIPÓLIT DE VOLTREGÀ"] <- "SANT HIPÒLIT DE VOLTREGÀ"

#-------------------------------------------------------------------------------------------------#
##
##  PART 6: STORING DATASETS
##  
##
#-------------------------------------------------------------------------------------------------#

## Covid-19 incidence by ABS and overall
#-------------------------------------------------------------------------------------------------#
# Overall yearly
saveRDS(totals_covid19_100k,"Data/Data_results/covid19_incidence_overall.RDS")
# Overall monthly
saveRDS(covid19_monthly,"Data/Data_results/covid19_incidence_overall_monthly.RDS")
# By ABS
saveRDS(yearly_covid_abs_pop,"Data/Data_results/covid19_incidence_abs.RDS")
# CSV for validation with ICS data
write.csv(covid19_monthly,"Data/Data_results/covid19_incidence_overall_monthly.csv")
write.csv(yearly_covid_abs_pop,"Data/Data_results/covid19_incidence_abs.csv")

## Change in other diagnostics  by ABS and overall
#-------------------------------------------------------------------------------------------------#
# ABS change in other new diagnostics and covid_19 incidence per 100k
saveRDS(other_detect_change,"Data/Data_results/abs_overall_change_dd_covid19")
# Saving the raw cases for yearly detections by chapter summed accross ABS
saveRDS(det_abs_chp_2019,"Data/Data_results/others_new_d_2019.RDS")
saveRDS(det_abs_chp_2020,"Data/Data_results/others_new_d_2020.RDS")
# Proportional cases by chapter monthly
saveRDS(det_abs_chp_change,"Data/Data_results/others_change_2020_2019_monthly.RDS")
# Proportional cases by chapter yearly
saveRDS(det_abs_chp_change_yearly,"Data/Data_results/others_change_2020_2019_yearly.RDS")
# Proportional changes yearly by ABS and chapter
saveRDS(det_abs_chp_change_yearly_abs,"Data/Data_results/others_change_2020_2019_yearly_abs.RDS")

# Clean: changes in diagnostic by chapter and yearly
#-------------------------------------------------------------------------------------------------#
new_diagnostics_comparison<-data.frame(t(det_abs_chp_change))
new_diagnostics_comparison$Overall<-t(det_abs_chp_change_yearly)
colnames(new_diagnostics_comparison)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Overall")
rownames(new_diagnostics_comparison)<-c('Chp.1 Infectious',
                                        'Chp.2 Neoplasms',
                                        'Chp.3 Blood & immune',
                                        'Chp.4 Endoc.& nutrit.& metabol.',
                                        'Chp.5 Mental & behav.',
                                        'Chp.6 Nervous',
                                        'Chp.7 Eye & adnexa',
                                        'Chp.8 Ear & mastoid',
                                        'Chp.9 Circulatory',
                                        'Chp.10 Respiratory',
                                        'Chp.11 Digestive',
                                        'Chp.12 Skin',
                                        'Chp.13 Musculoskeletal & connective',
                                        'Chp.14 Genitourinary',
                                        'Chp.15 Pregnancy & chilbirth & puerperium',
                                        'Chp.16 Perinatal',
                                        'Chp.17 Congenital malf.',
                                        'Chp.18 Not elsewhere clas.',
                                        'Chp.19 Infury & poison',
                                        'Chp.20 External',
                                        'Chp.21 Risk factors')
  

write.csv(new_diagnostics_comparison,"Data/Data_results/new_diagnostics_comparison.csv")
saveRDS(new_diagnostics_comparison,"Data/Data_results/new_diagnostics_comparison.RDS")

## Change in summed other diagnostics by month
#-------------------------------------------------------------------------------------------------#
# New diagnostic counts by month 2019 and 2020
saveRDS(over_monthly_others_2020,"Data/Data_results/overall_monthly_others_2020.RDS")
saveRDS(over_monthly_others_2019,"Data/Data_results/overall_monthly_others_2019.RDS")
# Proportional change
saveRDS(det_others_change_monthly,"Data/Data_results/overall_monthly_others_change.RDS")

## Chapter totals excluding covid-19
#-------------------------------------------------------------------------------------------------#
# 2019 chapter totals
saveRDS(chapter_yearly_2019,"Data/Data_results/chapter_diagnostics_2019.RDS")
# 2020 chapter totals
saveRDS(chapter_yearly_2020,"Data/Data_results/chapter_diagnostics_2020.RDS")

## Shape files
#-------------------------------------------------------------------------------------------------#
# Attempting to save as RDS
saveRDS(cat_abs,"Data/Data_results/cat_abs_shape.RDS")

