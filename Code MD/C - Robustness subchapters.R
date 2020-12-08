###################################################################################################
##
##  DIAGNOSTIC DETECTION
##  Data analysis
##
###################################################################################################
##
## Elaboration date: 02/12/2020
## Last modification date: 02/12/2020
## 
###################################################################################################
# Description:

# 


# Notes:
# 
#
###################################################################################################
##
##  LIBRARIES
##  
##
###################################################################################################
#install.packages("stringdist")
#install.packages("tidyverse")
#install.packages('reshape2')
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

#.................................................................................................

rm(list = ls())

# Set directories of work
here::here()


#-------------------------------------------------------------------------------------------------#
##
##  READING DATA
##  
##
#-------------------------------------------------------------------------------------------------#

# Reading ICD codebook
icd10_codebook<-readRDS('Data/Data_results/icd10_codebook.RDS')

# Panels of chapters split by date have been stored in separate csvs-
# List to be read later, in character form
list_sub_ch <- c(paste("chapter",'abs',seq(1:21),sep="_"))
# To store te results, this is the basic data object
chapters_sub_ch<-vector('list',length=length(list_sub_ch))
# Loop to store data in a list 
for (i in 1:length(list_sub_ch)){
  # Concatenated file name to be read
  tmp<-paste('Data/Data_results/Chapters/',list_sub_ch[i],'.csv', sep='')
  # Storing data frames in the list of pieces
  chapters_sub_ch[[i]]<-read.csv(tmp,sep=',',header=T)
}
# Substituing NAs with 0s
for (i in 1:length(list_sub_ch)){
  chapters_sub_ch[[i]][is.na(chapters_sub_ch[[i]])]<-0
}


# split by years
test<-chapters_sub_ch[[1]]

#-------------------------------------------------------------------------------------------------#
##
##  ANALYSIS
##  
##
#-------------------------------------------------------------------------------------------------#

# Order them within chapter by absolute value, then do the proportional change (ratio 2020/2019)
# Aggregate to have them at year level

#===================================================================================================#
# Yearly chapters ordered by subchapter counts
# 
#===================================================================================================#

# We order them by incidence in 2019
# New list to store the results
chapters_sub_ch_yearly_2019<-vector('list',length=length(list_sub_ch))
chapters_sub_ch_yearly_2020<-vector('list',length=length(list_sub_ch))
# Loop to sum over by year
for (i in 1:length(list_sub_ch)){
  #print(i)
  # Subchapter list: as many subchapters are we find in the data
  list_schp<-colnames(chapters_sub_ch[[i]][1,4:ncol(chapters_sub_ch[[i]])])
  # 2019
  chapters_sub_ch_yearly_2019[[i]] <- chapters_sub_ch[[i]][which(chapters_sub_ch[[i]]$year==2019),] %>%
  group_by(year) %>%
  summarize_at(list_schp, funs(sum = sum(., na.rm = T)))
  # Order by column value
  chapters_sub_ch_yearly_2019[[i]]<-chapters_sub_ch_yearly_2019[[i]][,order(-chapters_sub_ch_yearly_2019[[i]][1,])]
  # 2020
  chapters_sub_ch_yearly_2020[[i]] <- chapters_sub_ch[[i]][which(chapters_sub_ch[[i]]$year==2020),] %>%
    group_by(year) %>%
    summarize_at(list_schp, funs(sum = sum(., na.rm = T)))
  # Order by column value following 2019 orders
  chapters_sub_ch_yearly_2020[[i]]<-chapters_sub_ch_yearly_2020[[i]][colnames(chapters_sub_ch_yearly_2019[[i]])]
}

#===================================================================================================#
# 2020 and 2019 rankings of subchapters by chapter
# 
#===================================================================================================#

# For the time being, simply visually compare them

# ch21, s7 much larger in 2020
# ch4, s8 much smaller in 2020
# ch1, s15 much larger in 2020

#===================================================================================================#
# 2020/2019 ratios by subchapters
# 
#===================================================================================================#

# New list to store the results
chapters_sub_ch_yearly_ratios<-vector('list',length=length(list_sub_ch))
# Looping over chapters
for (i in 1:length(list_sub_ch)){
  #print(i)
  # excluding the year column
  chapters_sub_ch_yearly_ratios[[i]]<-subset(chapters_sub_ch_yearly_2020[[i]],select=-c(year))/
                                      subset(chapters_sub_ch_yearly_2019[[i]],select=-c(year))
}

# Storing the top 4 subchapters across chapters separately
# Frame to store: simply the ratios, then, to identify the chapters they belong to, another object of names
top4_yearly_subchapter_ratios<-data.frame(matrix(NA,nrow=4,ncol=length(list_sub_ch)))
colnames(top4_yearly_subchapter_ratios)<-paste('chp',seq(1:21),sep='')
for (i in 1:length(list_sub_ch)){
  print(i)
  # Ratios
  top4_yearly_subchapter_ratios[,i]<-t(chapters_sub_ch_yearly_ratios[[i]][1:4])
}
# Storing the names of the top 4
top4_yearly_subchapter_names<-data.frame(matrix(NA,nrow=4,ncol=length(list_sub_ch)))
colnames(top4_yearly_subchapter_names)<-paste('chp',seq(1:21),sep='')
for (i in 1:length(list_sub_ch)){
  for (j in 1:4){
    #print(i)
    # Ratios
    top4_yearly_subchapter_names[j,i]<-colnames(chapters_sub_ch_yearly_ratios[[i]][j])
}
}
# Merging names and values
top4_yearly_subchapter_full<-data.frame(matrix(NA,nrow=4,ncol=length(list_sub_ch)))
colnames(top4_yearly_subchapter_full)<-paste("chp",seq(1:21),sep='_')
for (i in 1:length(list_sub_ch)){
  for (j in 1:4){
    #print(i)
    # Ratios
    top4_yearly_subchapter_full[j,i]<-paste(round(top4_yearly_subchapter_ratios[j,i],3),top4_yearly_subchapter_names[j,i],sep=',')
  }
}

#===================================================================================================#
# 2020/2019 ratios visual inspection
# 
#===================================================================================================#

# Flagging for report cases +-20 over the rest
# Keep in mind chapter 1  Other Viral Diseases s15 and chapter 10  Influenza And Pneumonia schp_2 are covid19 related, so do not flag.
# Keep an eye out for chapter 21, where s8 changed massively.
# Remarks from before: 
# ch21, s7 much larger in 2020
# ch4, s8 much smaller in 2020
# ch1, s15 much larger in 2020



## Chapter 1:Infectious
#-------------------------------------------------------------------------------------------------#

# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_1')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_1'&icd10_codebook$schp_id=='schp_16')])
#unique(icd10_codebook$code[which(icd10_codebook$chp_id=='chp_1'&icd10_codebook$schp_id=='schp_15')])

# Raw data to observe
top4_yearly_subchapter_full[1]
# Observations
# High drops:
## (0.501) for S1: Intestinal Infectious Diseases
## B342 unusual due to including covid19, this is purged in the final data.
# Low drops:
## (0.637) for S16: Mycoses
## (0.538) for S11: Viral Infections Characterized By Skin And Mucous Membrane Lesions

# Conclusion:
# Nothing to comment.

## Chapter 2: Neoplasm
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_2')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_2'&icd10_codebook$schp_id=='schp_6')])

# Raw data to observe
top4_yearly_subchapter_full[2]
chapters_sub_ch_yearly_2019[[2]]
chapters_sub_ch_yearly_2020[[2]]
# Observations
# Uneven drop across neoplasm types
# High drops:
## (0.47) for S6: Malignant Neoplasms Of Mesothelial And Soft Tissue
## (0.44) for S18: Benign Neoplasms, Except Benign Neuroendocrine Tumors
# Low drops:
## (0.68) for S2: Malignant Neoplasms Of Digestive Organs
## (0.796) for S7: Malignant Neoplasms Of Breast

# Conclusion:
# High heterogeneity in the cross-year variation for neoplasm types.

## Chapter 3: Blood & immune
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_3')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_3'&icd10_codebook$schp_id=='schp_4')])

# Raw data to observe
top4_yearly_subchapter_full[3]
chapters_sub_ch_yearly_2019[[3]]
chapters_sub_ch_yearly_2020[[3]]
# Observations
# Similar ranking across years in count importance.
# High drops:
## (0.52) for S5: Other Disorders Of Blood And Blood-Forming Organs
## (0.562) for S1: Nutritional Anemias
# Low drops:
## (0.655) for S3: Aplastic And Other Anemias And Other Bone Marrow Failure Syndromes
## (0.58) for S4: Coagulation Defects, Purpura And Other Hemorrhagic Conditions

# Conclusion:
# Low variation across subchapters.


## Chapter 4:  Endocrine, nutritional and metabolic diseases
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_4')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_4'&icd10_codebook$schp_id=='schp_9')])

# Raw data to observe
top4_yearly_subchapter_full[4]
chapters_sub_ch_yearly_2019[[4]]
chapters_sub_ch_yearly_2020[[4]]
# Observations
# Changing ranking across years in count importance (s8 drops a lot more).
# High drops:
## (0.319) for S8: Overweight, Obesity And Other Hyperalimentation
## (0.607) for S1: Disorders Of Thyroid Gland
# Low drops:
## (0.807) for S7: Other Nutritional Deficiencies
## (0.625) for S9: Metabolic Disorders

# Conclusion:
# High variation across subchapters.

## Chapter 5:  Mental, Behavioral and Neurodevelopmental disorders
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_5')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_5'&icd10_codebook$schp_id=='schp_6')])

# Raw data to observe
top4_yearly_subchapter_full[5]
chapters_sub_ch_yearly_2019[[5]]
chapters_sub_ch_yearly_2020[[5]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.675) for S2: Mental And Behavioral Disorders Due To Psychoactive Substance Use
## (0.694) for S4: Mood [Affective] Disorders
# Low drops:
## (0.817) for S5: Anxiety, Dissociative, Stress-Related, Somatoform And Other Nonpsychotic Mental Disorders
## (0.696) for S6: Behavioral Syndromes Associated With Physiological Disturbances And Physical Factors

# Conclusion:
# Anxiety doesn't drop much, likely increased incidence at play.

## Chapter 6:  Diseases of the nervous system
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_6')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_6'&icd10_codebook$schp_id=='schp_6')])

# Raw data to observe
top4_yearly_subchapter_full[6]
chapters_sub_ch_yearly_2019[[6]]
chapters_sub_ch_yearly_2020[[6]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.524) for S7: Nerve, Nerve Root And Plexus Disorders
## (0.554) for S3: Extrapyramidal And Movement Disorders
# Low drops:
## (0.602) for S4: Other Degenerative Diseases Of The Nervous System
## (0.596) for S6: Episodic And Paroxysmal Disorders

# Conclusion:
# Low variation across subchapters.

## Chapter 7:  Diseases of the eye and adnexa
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_7')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_7'&icd10_codebook$schp_id=='schp_2')])

# Raw data to observe
top4_yearly_subchapter_full[7]
chapters_sub_ch_yearly_2019[[7]]
chapters_sub_ch_yearly_2020[[7]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.479) for S10: Visual Disturbances And Blindness
## (0.423) for S11: Other Disorders Of Eye And Adnexa
# Low drops:
## (0.625) for S1: Disorders Of Eyelid, Lacrimal System And Orbit
## (0.503) for S2: Disorders Of Conjunctiva

# Conclusion:
# Low,mid variation across subchapters.

## Chapter 8:  Diseases of the ear and mastoid process
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_8')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_8'&icd10_codebook$schp_id=='schp_3')])

# Raw data to observe
top4_yearly_subchapter_full[8]
chapters_sub_ch_yearly_2019[[8]]
chapters_sub_ch_yearly_2020[[8]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.666) for S1: Diseases Of External Ear
## (0.532) for S2: Diseases Of Middle Ear And Mastoid
# Low drops:
## (0.746) for S4: Other Disorders Of Ear
## (0.759) for S3: Diseases Of Inner Ear

# Conclusion:
# Low,mid variation across subchapters, abnormal for Diseases Of Middle Ear And Mastoid which is a very high incidence code in 2019.



## Chapter 9:  Diseases of the circulatory system
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_9')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_9'&icd10_codebook$schp_id=='schp_7')])

# Raw data to observe
top4_yearly_subchapter_full[9]
chapters_sub_ch_yearly_2019[[9]]
chapters_sub_ch_yearly_2020[[9]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.546) for S6: Other Forms Of Heart Disease
## (0.542) for S3: Hypertensive Diseases
# Low drops:
## (0.668) for S9: Diseases Of Veins, Lymphatic Vessels And Lymph Nodes, Not Elsewhere Classified
## (0.644) for S7: Cerebrovascular Diseases

# Conclusion:
# Low,mid variation across subchapters.


## Chapter 10:  Diseases of the respiratory system
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_10')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_10'&icd10_codebook$schp_id=='schp_2')])

# Raw data to observe
top4_yearly_subchapter_full[10]
chapters_sub_ch_yearly_2019[[10]]
chapters_sub_ch_yearly_2020[[10]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.468) for S3: Other Acute Lower Respiratory Infections
## (0.465) for S5: Chronic Lower Respiratory Diseases
# Low drops:
## (0.507) for S1: Acute Upper Respiratory Infections
## (0.872) for S2: Influenza And Pneumonia

# Conclusion:
# Exceptional for S2:Influenza And Pneumonia, this is due to covid19 classifications and is purged in the final data.

## Chapter 11:  Diseases of the digestive system
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_11')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_11'&icd10_codebook$schp_id=='schp_1')])

# Raw data to observe
top4_yearly_subchapter_full[11]
chapters_sub_ch_yearly_2019[[11]]
chapters_sub_ch_yearly_2020[[11]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.498) for S5: Noninfective Enteritis And Colitis
## (0.56) for S2: Diseases Of Esophagus, Stomach And Duodenum
# Low drops:
## (0.683) for S6: Other Diseases Of Intestines
## (0.579) for S1: Diseases Of Oral Cavity And Salivary Glands

# Conclusion:
# Low,mid variation across subchapters.

## Chapter 12:  Diseases of the skin and subcutaneous tissue
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_12')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_12'&icd10_codebook$schp_id=='schp_7')])

# Raw data to observe
top4_yearly_subchapter_full[12]
chapters_sub_ch_yearly_2019[[12]]
chapters_sub_ch_yearly_2020[[12]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.585) for S3: Dermatitis And Eczema
## (0.596) for S9: Other Disorders Of The Skin And Subcutaneous Tissue
# Low drops:
## (0.801) for S1: Infections Of The Skin And Subcutaneous Tissue
## (0.597) for S7: Disorders Of Skin Appendages

# Conclusion:
# Low variation in general, but exception for S1: Infections Of The Skin And Subcutaneous Tissue.

## Chapter 13: Diseases of the musculoskeletal system and connective tissue
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_13')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_13'&icd10_codebook$schp_id=='schp_5')])

# Raw data to observe
top4_yearly_subchapter_full[13]
chapters_sub_ch_yearly_2019[[13]]
chapters_sub_ch_yearly_2020[[13]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.476) for S13: Other Soft Tissue Disorders
## (0.486) for S3: Inflammatory Polyarthropathies
# Low drops:
## (0.571) for S10: Other Dorsopathies
## (0.55) for S5: Other Joint Disorders

# Conclusion:
# Low variation in general.

## Chapter 14: Diseases of the genitourinary system
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_14')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_14'&icd10_codebook$schp_id=='schp_6')])

# Raw data to observe
top4_yearly_subchapter_full[14]
chapters_sub_ch_yearly_2019[[14]]
chapters_sub_ch_yearly_2020[[14]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.525) for S10: Noninflammatory Disorders Of Female Genital Tract
## (0.59) for S7: Diseases Of Male Genital Organs
# Low drops:
## (0.724) for S4: Urolithiasis
## (0.864) for S6: Other Diseases Of The Urinary System

# Conclusion:
# High variation between drops in S10 and S7 and the rest.

## Chapter 15: Pregnancy, childbirth and the puerperium
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_15')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_15'&icd10_codebook$schp_id=='schp_4')])

# Raw data to observe
top4_yearly_subchapter_full[15]
chapters_sub_ch_yearly_2019[[15]]
chapters_sub_ch_yearly_2020[[15]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.646) for S2: Supervision Of High Risk Pregnancy
## (0.53) for S1: Pregnancy With Abortive Outcome
# Low drops:
## (0.867) for S5: Maternal Care Related To The Fetus And Amniotic Cavity And Possible Delivery Problems
## (0.767) for S4: Other Maternal Disorders Predominantly Related To Pregnancy

# Conclusion:
# Low counts make interpreting this variation tricky, recommend don't focus. Abortive outcomes drop a lot.

## Chapter 16: Certain conditions originating in the perinatal period
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_16')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_16'&icd10_codebook$schp_id=='schp_2')])

# Raw data to observe
top4_yearly_subchapter_full[16]
chapters_sub_ch_yearly_2019[[16]]
chapters_sub_ch_yearly_2020[[16]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.172) for S12: Other Disorders Originating In The Perinatal Period
## (0.6) for S7: Hemorrhagic And Hematological Disorders Of Newborn
# Low drops:
## (0.685) for S6: Infections Specific To The Perinatal Period
## (0.718) for S2: Disorders Of Newborn Related To Length Of Gestation And Fetal Growth

# Conclusion:
# Very low counts, care ful with interpretation; that said, massive drop in S12: Other Disorders Originating In The Perinatal Period

## Chapter 17: Congenital malformations, deformations and chromosomal abnormalities
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_17')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_17'&icd10_codebook$schp_id=='schp_2')])

# Raw data to observe
top4_yearly_subchapter_full[17]
chapters_sub_ch_yearly_2019[[17]]
chapters_sub_ch_yearly_2020[[17]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.508) for S6: Other Congenital Malformations Of The Digestive System
## (0.561) for S9: Congenital Malformations And Deformations Of The Musculoskeletal System
# Low drops:
## (0.691) for S7: Congenital Malformations Of Genital Organs
## (0.575) for S2: Congenital Malformations Of Eye, Ear, Face And Neck

# Conclusion:
# Low variation in general.

## Chapter 18: Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_18')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_18'&icd10_codebook$schp_id=='schp_2')])

# Raw data to observe
top4_yearly_subchapter_full[18]
chapters_sub_ch_yearly_2019[[18]]
chapters_sub_ch_yearly_2020[[18]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.719) for S1: Symptoms And Signs Involving The Circulatory And Respiratory Systems
## (0.743) for S6: Symptoms And Signs Involving Cognition, Perception, Emotional State And Behavior
# Low drops:
## (0.864) for S8: General Symptoms And Signs
## (0.81) for S2: Symptoms And Signs Involving The Digestive System And Abdomen

# Conclusion:
# Low variation in general; difficult to interpret. 

## Chapter 19: Injury, poisoning and certain other consequences of external causes
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_19')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_19'&icd10_codebook$schp_id=='schp_20')])

# Raw data to observe
top4_yearly_subchapter_full[19]
chapters_sub_ch_yearly_2019[[19]]
chapters_sub_ch_yearly_2020[[19]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.461) for S12: Injury Of Unspecified Body Region
## (0.569) for S10: Injuries To The Ankle And Foot
# Low drops:
## (0.634) for S7: Injuries To The Wrist, Hand And Fingers
## (0.633) for S20: Other And Unspecified Effects Of External Causes

# Conclusion:
# Particularly high drop in some subchapters like S12: Injury Of Unspecified Body Region. Confinement, telework, worth commenting?

## Chapter 20: External causes of morbidity
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_20')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_20'&icd10_codebook$schp_id=='schp_13')])

# Raw data to observe
top4_yearly_subchapter_full[20]
chapters_sub_ch_yearly_2019[[20]]
chapters_sub_ch_yearly_2020[[20]]
# Observations
# No variation ranking across years in count importance.
# High drops:
## (0.716) for S24: Assault
## (0.776) for S12: Other And Unspecified Transport Accidents
# Low drops:
## (0.798) for S15: Exposure To Animate Mechanical Forces
## (0.866) for S13: Slipping, Tripping, Stumbling And Falls

# Conclusion:
# Interesting that they do not drop that much. S15 and S13 seem related to work; I guess differential prevalence of telework.
# Factory workers do not telework!


## Chapter 21: Factors influencing health status and contact with health services
#-------------------------------------------------------------------------------------------------#
# To look up a specific subchapter name
unique(icd10_codebook$chapter[which(icd10_codebook$chp_id=='chp_21')])
unique(icd10_codebook$sub_chapter[which(icd10_codebook$chp_id=='chp_21'&icd10_codebook$schp_id=='schp_14')])

# Raw data to observe
top4_yearly_subchapter_full[21]
chapters_sub_ch_yearly_2019[[21]]
chapters_sub_ch_yearly_2020[[21]]
# Observations
# Big change in S7 due to it capturing contact with covid-19 infected person.
# High drops:
## (0.528) for S10: Persons With Potential Health Hazards Related To Socioeconomic And Psychosocial Circumstances
## (0.55) for S15: Persons With Potential Health Hazards Related To Family And Personal History And Certain Conditions Influencing Health Status
# Low drops:
## (0.746) for S1: Persons Encountering Health Services For Examinations
## (0.895) for S14: Persons Encountering Health Services In Other Circumstances
# Big increase:
## (10.19676) for S7: Persons With Potential Health Hazards Related To Communicable Diseases

# Conclusion:
# Note the drops in social services S10 and S15! Those are not minor in number. Big variations here, probably worth mentioning.
