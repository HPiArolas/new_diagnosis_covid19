#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  CIM / ICD diagnostic
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#

# Description:

# Quick aggregations of diagnostics to higher up groups and codebooks for ICD-10 classifications.
# Currently the example is set to bring diagnostics to chapter level.


# Notes:
# 

#-------------------------------------------------------------------------------------------------#
##
##  LIBRARIES
##  
##
#-------------------------------------------------------------------------------------------------#

#cinstall.packages("devtools")
#devtools::install_github("jackwasey/icd")
#devtools::install_github("wtcooper/icdcoder")
#install.packages("tictoc")
library(icd)
library(here)
library(readxl)
library(dplyr)
library(stringr)
library(icd.data)
library(devtools) 
library(icdcoder)
library(data.table)
library(tictoc)

#-------------------------------------------------------------------------------------------------#
##
##  DATA READING
##  
##
#-------------------------------------------------------------------------------------------------#

# Set directories of work
here::here()
# Open dataset
data <- read.csv(here("Data","Data_results","DT_2.csv"))

#-------------------------------------------------------------------------------------------------#
##
##  CREATING A DIAGNOSTIC CODEBOOK
##  
##
#-------------------------------------------------------------------------------------------------#
## Creating an icd10 codebook: chapters
#-------------------------------------------------------------------------------------------------#

# Original codebook minus unused columns
icd10_codebook<-icd10cm2019[c(-2)]
# For the sake of ordering later (since ICD codes aren't alphanumeric), adding a row_number to keep the original order.
icd10_codebook$row_number<-seq(1:nrow(icd10_codebook))
# Adding costum short names for the chapters
chapter_list<-data.frame(chapter=as.character(unique(icd10_codebook$chapter))) # List of chapter names
# Adding a shortened version
for (i in 1:nrow(chapter_list)){
tmp<-c(seq(1:nrow(chapter_list)))
chapter_list$chp_id[i]<-paste("chp",'_',tmp[i],sep='')
}
# Adding the shortened name to the codebook
icd10_codebook<-merge(icd10_codebook,chapter_list,by=c('chapter'))
# Reordering back
icd10_codebook <- icd10_codebook[order(icd10_codebook$row_number),]

## Creating an icd10 codebook: suchapters
#-------------------------------------------------------------------------------------------------#
# Generating the sub_chapter identification
icd10_codebook2 <- icd10_codebook %>%
  group_by(chapter, sub_chapter) %>%
  summarize(x = 1)
icd10_codebook2$schp_id <- paste0("schp_",rowidv(icd10_codebook2, cols=c("chapter")))
icd10_codebook2 <- icd10_codebook2[c(2,4)]
icd10_codebook2 <- merge(icd10_codebook, icd10_codebook2, by.x="sub_chapter", by.y="sub_chapter")
icd10_codebook2 <- icd10_codebook2[order(icd10_codebook2$row_number),]


# Saving
write.csv(icd10_codebook2,"Data/Data_results/icd10_codebook.csv")

## Replacing diagnostic codes with chapters example
#-------------------------------------------------------------------------------------------------#
# https://stackoverflow.com/questions/35636315/replace-values-in-a-dataframe-based-on-lookup-table
# Example using simply the codebook list of highest disaggregation of diagnostic
diagnostic_fine<-data.frame(code=icd10_codebook$code)
diagnostic_less_fine<-diagnostic_fine
diagnostic_less_fine[]<-icd10_codebook$chp_id[match(unlist(diagnostic_fine), icd10_codebook$code)]

#-------------------------------------------------------------------------------------------------#
##
##  DATA MANAGEMENT: diagnostic names to chapter level example
##  
##
#-------------------------------------------------------------------------------------------------#

## Cleaning diagnostic names
#-------------------------------------------------------------------------------------------------#
#  Removing "." and "C01-" from both active diagnostics and visit diagnostics
data2<-data
# Removing "." and "C01-" from "diagnostics_actius"
data2$diagnostics_actius<-str_replace_all(data$diagnostics_actius, pattern = c('C01-'="", "[.]"=""))
# Removing "." and "C01-" from "diagnostics_actius"
data2$diagnostics_visita<-str_replace_all(data$diagnostics_visita, pattern = c('C01-'="","[.]"=""))

## Creating lists for replacement
#-------------------------------------------------------------------------------------------------#
# Here we attempt to use the above code on lists
# Storing in a list
active_list <- strsplit(data2$diagnostics_actius,"#")
# Replacing elements of the list
active_list_fine<-active_list
# Testing
active_list_less_fine<-active_list_fine

# IMPORTANT!!! @JOSE:Need to apply to ALL elements within each visit, that's the part that is missing.

active_list_less_fine[]<-icd10_codebook$chp_id[match(unlist(active_list_less_fine),icd10_codebook$code)]


#=========================================================================#
# ALternarnative? kind of inefficient (it took 1502.78 sec elapsed in my laptop) 
# Note: There are some codes that are not in the icd10_codebook
#==========================================================================#
tic("time")
active_list2 <- strsplit(data2$diagnostics_actius,"#")
active_list_fine2<-active_list2
active_list_less_fine2 <- lapply(1:length(active_list_fine2), function(x) icd10_codebook$chp_id[match(unlist(active_list_fine2[x]), icd10_codebook$code)])
toc()
#=====================================================#

tic("time")
visit_list2 <- strsplit(data2$diagnostics_visita,"#")
visit_list_fine2<-visit_list2
visit_list_less_fine2 <- lapply(1:length(visit_list_fine2), function(x) icd10_codebook$chp_id[match(unlist(visit_list_fine2[x]), icd10_codebook$code)])
toc()

#-------------------------------------------------------------------------------------------------#
##
##  STORING
##  
##
#-------------------------------------------------------------------------------------------------#

# Saving the codebook
saveRDS(icd10_codebook2,"Data/Data_results/icd10_codebook.RDS")
# Saving the "converted" lists
saveRDS(active_list_less_fine2,"Data/Data_results/active_list_chapter.RDS")
saveRDS(visit_list_less_fine2,"Data/Data_results/visit_list_chapter.RDS")



