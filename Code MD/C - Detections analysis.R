#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  new detections analysis
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
## Elaboration date: 2/12/2020
## Last modification date: 17/11/2020
## 
###################################################################################################
# Description:
# This script generates a panel of new diagnostics by chapter accounting for subchapter composition.


# Notes:

#
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
library(tictoc)

#.................................................................................................

rm(list = ls())
#-------------------------------------------------------------------------------------------------#
##
##  READING DATA
##  
##
#-------------------------------------------------------------------------------------------------#
# Set directories of work
here::here()
# Reading data on visits
data <- read.csv(here("Data","Data_results","DT_2.csv"))
# Reading the icd10 coodebook, modified by us
icd10_codebook <- readRDS(here("Data","Data_results","icd10_codebook.RDS"))
# Panel structure
panel_abs_structure<-readRDS("Data/Data_results/panel_abs_placeholder.RDS")

#-------------------------------------------------------------------------------------------------#
##
##  FUNCTIONS
##  Filtering funciton for chapters of causes
##
#-------------------------------------------------------------------------------------------------#
# This function allows to select a character (cause or chapter) and filter the rows that contain that character.
d_filter <- function(data, var, cod){
  data %>% 
    filter(
      str_detect(!!sym(var),
                 # I consider the cause or chapter that has the character "#" after and before.
                 paste0('(?<=,','#',')',cod,'|',cod,'(?=','#',')')) | 
        # I consider here the cause or chapter that has no "#" after, before or both.
        !!sym(var)==cod)
}
# Function that makes the collpase of chapters/sub_chapters by ABS, year and month
d_chap <- function(data,var){
  data %>% group_by(abs_name, month, year) %>% summarize_at(c(var), funs(sum = sum(., na.rm = T)))
}
# I define the arguments of the function, in the case of statement, I put "&" by default
md_filter <- function(data,
                      var,
                      cod,
                      statement='&'){
  # I create a character object that is the code I want to filter by multiple detections  
  text_eval <-
    paste0(
      # I define the data and put it in quotes just for the context of the function
      quote(data),
      ' %>% filter(',
      # I say the function to filter de character that is inbetween the character "#" for a particular variable
      paste0(
        '(str_detect(',
        var,
        ",paste0('#','",
        cod,
        "','#')))",
        # I include the argument to select the different causes by the estatement "&" or "|"
        collapse = paste0(' ', statement, ' ')
      ),
      ')'
    )
  # Using "parse_expr", I convert the text into code, and then I evaluate it (ctr+enter)
  eval(parse_expr(text_eval))
  
}

#-------------------------------------------------------------------------------------------------#
##
##  DATA MANAGEMENT
##  
##
#-------------------------------------------------------------------------------------------------#

# Keep the chapter to analyze
bla <- data
# Incorporate the character "#" in the detections diagnstics in order to use the function d_filter properly
bla$chapter_dd <- paste0("#", bla$chapter_dd)
bla$chapter_dd[-length(bla$chapter_dd)] <- paste0(bla$chapter_dd[-length(bla$chapter_dd)], '#')

# Generate abs_name
bla$abs_name <- bla$upd

# Split the date of visit
AA <- as.Date(bla$visi_data_visita)
BB <- data.frame(date = AA, year = as.numeric(format(AA, format = "%Y")), month = as.numeric(format(AA, format = "%m")))
bla <- cbind(bla,BB)
rm(AA, BB)


#-------------------------------------------------------------------------------------------------#
##
##  ANALYSIS: SUBCHAPTER AND CHAPTER SPECIFIC DETECTIONS
##  
##
#-------------------------------------------------------------------------------------------------#

# This piece generates detection counts for chapters and subchapters by abs/month/year (panel)

#===================================================================================================#
# Chapter specific objects at a subchapter level (time_chp: 110.2 sec elapsed)
# 
#===================================================================================================#

#--------------------------------------------------------------------------------------------#
# Number of chapter under analysis
# List of chapters
n_c <- unique(icd10_codebook$chp_id)
nc <- as.double(str_replace_all(n_c, pattern = c('chp_'="")))

tic('time_chp')
for(k in nc){
  #
  #--------------------------------------------------------------------------------------------#
  # Filter my data set and codebook by the chapter I want to analyze 
  chap <- d_filter(data = bla, var = 'chapter_dd', cod = paste0("chp_",k))
  icd10cb <- filter(icd10_codebook, icd10_codebook$chp_id == paste0("chp_",k))
  
  # Use a detection list to match with sub_chapter 
  det_sch <- strsplit(chap$causes_dd, "#")
  
  # Matching sub_chapters 
  sub_chap <- lapply(1:length(det_sch), function(x) icd10cb$schp_id[match(unlist(det_sch[x]), icd10cb$code)])
  
  # Drop the NAs of the list
  sub_chap <- lapply(1:length(sub_chap), function(x) na.omit(sub_chap[[x]]))
  
  # Count the number of sub_chapters detected per visit 
  sub_chap_count <- lapply(1:length(sub_chap), function(x) length(sub_chap[[x]]))
  
  # Incorporate the sub_chapters and sub_chapters_count in the data set
  sub_chapp <- lapply(1:length(sub_chap), function(x) paste(sub_chap[[x]],collapse="#"))
  sub_chapp <- as.data.frame.character(sub_chapp)
  sub_chapp_count <- as.data.frame.character(sub_chap_count)
  chap <- cbind.data.frame(chap, sub_chapp, sub_chapp_count)
  chap$sub_chap_count <- as.double(chap$sub_chap_count)
  
  # Incorporate the character "#" in order to use the function d_filter properly
  chap$sub_chapp <- paste0("#", chap$sub_chapp)
  chap$sub_chapp[-length(chap$sub_chapp)] <- paste0(chap$sub_chapp[-length(chap$sub_chapp)], '#')
  
  #===================================================================================================#
  # Panel ABS by sub_chapter
  #===================================================================================================#
  # I will generate my dataframes collapsed and filtered by chapter using the function "d_filter" and "d_chap"
  # I generate a list of number of sub_chapters of the chapter
  a <- unique(as.list(unlist(strsplit(as.character(chap$sub_chapp), "#"))))
  nchap <- as.double(str_replace_all(a, pattern = c('schp_'="")))
  nchap <- sort(nchap[!is.na(nchap)])
  
  # I construct the panel
  for(i in nchap){
    assign(paste0("schapter",i),d_filter(data = chap, var = 'sub_chapp', cod = paste0("schp_",i)))
    assign(paste0("schapter",i),d_chap(data = get(paste0("schapter",i)),var = 'sub_chap_count'))
    # Here I change the name of the column of new_causes to chp_i
    assign(paste0("schapter",i), setNames(get(paste0("schapter",i)), c("abs_name","month","year",paste0("s",i))))
  }
  # Join all panels: I use "Reduce" and "merge" to join multiple dataframes
  list_sch <- lapply(nchap, function(x) list(get(paste0("schapter",x))))
  abs.panel_chap <- Reduce(function(x, y) merge(x, y, by=c('abs_name','year','month'), all=TRUE), list_sch)
  rm(list = c(paste("schapter",nchap,sep="")))
  rm(a, sub_chap, sub_chapp)
  
  # Sum rows of sub_chapter
  abs.panel_chap[paste0('chp',k)] <- rowSums(abs.panel_chap[,4:ncol(abs.panel_chap)], na.rm = TRUE)
  
  # Keep the chapter only
  assign(paste0("abs_ch",k), abs.panel_chap[-c(5:ncol(abs.panel_chap)-1)])
  
}
toc()

# Merge the all chapters
list_absch <- lapply(nc, function(x) list(get(paste0("abs_ch",x))))
abs.panel_chap <- Reduce(function(x, y) merge(x, y, by=c('abs_name','year','month'), all=TRUE), list_absch)
rm(list = c(paste("abs_ch",nc,sep="")))

# Storage the year-month panel for each chapter with sub_chapters
write.csv(abs.panel_chap,here("Data","Data_results","Chapters","abs.panel_chp.csv"), row.names = FALSE, na="")

#===================================================================================================#
# ABS Panel - Causes related to COVID-19
# Causes considered: "B342", "U071", "B342 & J1289", "U071 & J1289"
#===================================================================================================#

# Incorporate the character "#" in the causes detected in order to use the function d_filter properly
bla$causes_dd <- paste0("#", bla$causes_dd)
bla$causes_dd[-length(bla$causes_dd)] <- paste0(bla$causes_dd[-length(bla$causes_dd)], '#')

# Define the cause 
cause = c("B342", "U071")

# Filter my data set and codebook by the cause I want to analyze 
for(i in cause){
  # Filter my data set and codebook by the cause I want to analyze 
  cov1 <- d_filter(data = bla, var = 'causes_dd', cod = i)
  # Converting in panel 
  cov2 <- d_chap(data = cov1, var = 'new_causes')
  # Changing the variable name
  colnames(cov2) <- c('abs_name','month','year', i)
  # Storage
  assign(paste0("cov_",i),cov2)
}
rm(cov1, cov2)

# Define the group of cause
cov1 <- md_filter(data = bla, var = 'causes_dd', cod = c('B342','J1289'), statement = '&')
cov2 <- md_filter(data = bla, var = 'causes_dd', cod = c('U071','J1289'), statement = '&')

# Converting in panel 
cov_B342_J1289 <- d_chap(data = cov1, var = 'new_causes')
cov_U071_J1289 <- d_chap(data = cov2, var = 'new_causes')

# Changing the variable name
colnames(cov_B342_J1289) <- c('abs_name','month','year', 'B342_J1289')
colnames(cov_U071_J1289) <- c('abs_name','month','year', 'U071_J1289')

# Removing intermediate datasets
rm(cov1, cov2)

# Merging the covid causes detections
# Merge the all chapters
list_cov <- list(cov_B342, cov_B342_J1289, cov_U071, cov_U071_J1289)
abs.panel_covid_1 <- Reduce(function(x, y) merge(x, y, by=c('abs_name','month','year'), all=TRUE), list_cov)

# Sum the causes "B342" and "U071" because the group of causes already contain the main covid causes
abs.panel_covid_1$covid <- rowSums(abs.panel_covid_1[, c("B342","U071")], na.rm = TRUE)
# Certain months do not have covid-19 cases, merging it with the panel structure to then annotate them as NA
abs.panel_covid_2<-merge(panel_abs_structure,abs.panel_covid_1,by=c('abs_name','month','year'),all=TRUE)
# Removing abs_id
abs.panel_covid<-subset(abs.panel_covid_2, select=-c(abs_id))
# Replacing NAs by zeros
abs.panel_covid[is.na(abs.panel_covid)]<-0

#=====================================================================================================#
# Merge the abs.panel_chap and abs.panel_covid
#=====================================================================================================#
abs.panel_tot <- Reduce(function(x, y) merge(x, y, by=c('abs_name','year','month'), all=TRUE), list(abs.panel_chap, abs.panel_covid))

# Sort the data considering ABS, year, months
abs.panel_tot <- abs.panel_tot[order(abs.panel_tot$abs_name, abs.panel_tot$year, abs.panel_tot$month),] 

# Substracting chapter_1 from "B342"
abs.panel_tot2 <- abs.panel_tot
abs.panel_tot2$B342 <- abs.panel_tot2$B342*-1
abs.panel_tot2$chp1 <-  rowSums(abs.panel_tot2[, c("chp1","B342")], na.rm = TRUE)

# Substracting chapter_10 from "B342_J1289" and "U071_J1289"
abs.panel_tot2$B342_J1289 <- abs.panel_tot2$B342_J1289*-1
abs.panel_tot2$U071_J1289 <- abs.panel_tot2$U071_J1289*-1
abs.panel_tot2$chp10 <-  rowSums(abs.panel_tot2[, c("chp10","B342_J1289","U071_J1289")], na.rm = TRUE)

# Keep my abs.panel_chp without covid cases
abs.panel_chap_no_cov <- subset(abs.panel_tot2, select = -c(B342,B342_J1289,U071,U071_J1289,covid))
# Replacing NAs by zeros
abs.panel_chap_no_cov[is.na(abs.panel_chap_no_cov)]<-0


#-------------------------------------------------#
# Outcomes:
#-------------------------------------------------#
# 1. ABS panel including covid cases = abs.panel_chap
saveRDS(abs.panel_chap,"Data/Data_results/panel_detections_abs_chap_all.RDS")
# 2. ABS panel without covid cases = abs.panel_chap_no_cov
saveRDS(abs.panel_chap_no_cov,"Data/Data_results/panel_detections_abs_chap_nocovid.RDS")
# 3. ABS panel covid cases = abs.panel_covid
saveRDS(abs.panel_covid,"Data/Data_results/panel_detections_abs_covid19.RDS")
# 4. Year-month panel for each chapter with sub_chapters
write.csv(abs.panel_chap,here("Data","Data_results","Chapters","abs.panel_chp.csv"), row.names = FALSE, na="")
#================================================================================================#
# Notes: 
# 1. The cause U071 is not classified in any chapter, therefore I do not take the difference
# 2. There could be the case in which the covid cause was detected in previous visits, thus the pneumonia can be detected alone but it can be caused by covid
#================================================================================================#

