#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  Hypothesis testing
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#


## Writen date: 12/12/2020
## Last modification date: 12/12/2020

#-------------------------------------------------------------------------------------------------#

# Description:
# This script creates the vizualization for the short article on new diagnostics.
# Assume every year is a distribution of new diagnostics (at either the chapter or overall level)

# Notes:

# Hypothesis testing a la Birkmeyer et al. (2020) for the yearly diagnoses ratios.

#-------------------------------------------------------------------------------------------------#
##
##  LIBRARIES
##  
##
#-------------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------------#
##
##  READING DATA
##  
##
#-------------------------------------------------------------------------------------------------#

# New diagnostic counts by month 2019 and 2020 -- summed across chapters
monthly_counts_others_2020<-readRDS("Data/Data_results/overall_monthly_others_2020.RDS")
monthly_counts_others_2019<-readRDS("Data/Data_results/overall_monthly_others_2019.RDS")
# Saving the raw cases for yearly detections by chapter summed accross ABS
monthly_others_incidence_2019<-readRDS("Data/Data_results/others_new_d_2019.RDS")
monthly_others_incidence_2020<-readRDS("Data/Data_results/others_new_d_2020.RDS")
# Subchapter data at the ABS level
counts_subchp_21<-read.csv('Data/Data_results/Chapters/chapter_abs_21.csv',sep=',',header=T)
counts_subchp_2<-read.csv('Data/Data_results/Chapters/chapter_abs_2.csv',sep=',',header=T)


#-------------------------------------------------------------------------------------------------#
##
##  DATA MANAGEMENT
##  
##
#-------------------------------------------------------------------------------------------------#


#=====================================================================================================#
# Chapter 21
#=====================================================================================================#

# Monthly subchapter counts aggregated across ABS
# List of subchapters within counts_subchp_21
list_sub_ch21<-colnames(subset(counts_subchp_21,select=-c(abs_name,year,month)))
# Summing over ABS for monthly counts
# 2019
monthly_counts_subchp_21_2019<-counts_subchp_21[which(counts_subchp_21$year==2019),]%>%
                          group_by(month) %>%
                          summarize_at(list_sub_ch21, funs(sum = sum(., na.rm = T)))
# 2020
monthly_counts_subchp_21_2020<-counts_subchp_21[which(counts_subchp_21$year==2020),]%>%
                          group_by(month) %>%
                          summarize_at(list_sub_ch21, funs(sum = sum(., na.rm = T)))
# Selecting the relevant subchapters: 
# S10: Persons With Potential Health Hazards Related To Socioeconomic And Psychosocial Circumstances
# S15: Persons With Potential Health Hazards Related To Family And Personal History And Certain Conditions Influencing Health Status
s_monthly_counts_subchp_21_2019<-subset(monthly_counts_subchp_21_2019,select=c(s10_sum,s15_sum))
s_monthly_counts_subchp_21_2020<-subset(monthly_counts_subchp_21_2020,select=c(s10_sum,s15_sum))
  


#=====================================================================================================#
# Chapter 2
#=====================================================================================================#  
  
# Monthly subchapter counts aggregated across ABS
# List of subchapters within counts_subchp_21
list_sub_ch2<-colnames(subset(counts_subchp_2,select=-c(abs_name,year,month)))
# Summing over ABS for monthly counts
# 2019
monthly_counts_subchp_2_2019<-counts_subchp_2[which(counts_subchp_2$year==2019),]%>%
  group_by(month) %>%
  summarize_at(list_sub_ch2, funs(sum = sum(., na.rm = T)))
# 2020
monthly_counts_subchp_2_2020<-counts_subchp_2[which(counts_subchp_2$year==2020),]%>%
  group_by(month) %>%
  summarize_at(list_sub_ch2, funs(sum = sum(., na.rm = T)))
# Selecting the relevant subchapters: 
# S2: Malignant Neoplasms Of Digestive Organs
# S7: Malignant Neoplasms Of Breast
# S6: Malignant Neoplasms Of Mesothelial And Soft Tissue
s_monthly_counts_subchp_2_2019<-subset(monthly_counts_subchp_2_2019,select=c(s2_sum,s6_sum,s7_sum))
s_monthly_counts_subchp_2_2020<-subset(monthly_counts_subchp_2_2020,select=c(s2_sum,s6_sum,s7_sum))

  
#-------------------------------------------------------------------------------------------------#
##
##  ANALYSIS
##  
##
#-------------------------------------------------------------------------------------------------#


#=====================================================================================================#
# Hypothesis testing overall
#=====================================================================================================#

# We want to know whether the mean of the monthly ratios of 2020/2019 is sign. different from 1.
# The ratio
overall_ratio<-data.frame(overall_ratio=monthly_counts_others_2020$others_counts/monthly_counts_others_2019$others_counts)
# Then we compute a t-test
overall_test<-t.test(overall_ratio,mu=1)

## Pandemic months only
overall_ratio_pandemic<-data.frame(overall_ratio=monthly_counts_others_2020$others_counts[which(monthly_counts_others_2020$month>=3)]/
                            monthly_counts_others_2019$others_counts[which(monthly_counts_others_2019$month>=3)])
# Then we compute a t-test
overall_pandemic_test<-t.test(overall_ratio_pandemic,mu=1)



#=====================================================================================================#
# Hypothesis testing chapter by chapter
#=====================================================================================================#

# Constructing monthly chapter ratios
chp_ratios<-subset(monthly_others_incidence_2020,select=-c(year,month))/
                subset(monthly_others_incidence_2019,select=-c(year,month))
# Adding colnames
colnames(chp_ratios)<-paste('chp_',seq(1:21),sep='')
# List to store the t-test results
list_chp_ttest<-vector('list',length=ncol(chp_ratios))
# Performing the t-tests against alternative of mu=1.
list_chp_ttest<-lapply(chp_ratios,t.test)
# Now storing the results in a matrix: p-value, upper and lower confidence interval
chp_ratios_tests<-data.frame(matrix(NA,nrow=ncol(chp_ratios),ncol=3))
rownames(chp_ratios_tests)<-paste('chp_',seq(1:21),sep='')
colnames(chp_ratios_tests)<-c('p_value','l_ci','u_ci')
# Extracting the results from the list
for (i in 1:ncol(chp_ratios)){
  # p-value
  chp_ratios_tests$p_value[i]<-round(list_chp_ttest[[i]]$p.value,5)
  # l ci
  chp_ratios_tests$l_ci[i]<-list_chp_ttest[[i]]$conf.int[1]
  # u ci
  chp_ratios_tests$u_ci[i]<-list_chp_ttest[[i]]$conf.int[2]
}


#=====================================================================================================#
# Hypothesis testing chapter by subchapter - Neoplasms
#=====================================================================================================#

# Subchapter ratios
chp2_ratios<-s_monthly_counts_subchp_2_2020/s_monthly_counts_subchp_2_2019
# List to store the t-test results
list_chp2_ttest<-vector('list',length=ncol(chp2_ratios))
# Performing the t-tests against alternative of mu=1.
list_chp2_ttest<-lapply(chp2_ratios,t.test,mu=1)
# Now storing the results in a matrix: p-value, upper and lower confidence interval
chp2_ratios_tests<-data.frame(matrix(NA,nrow=ncol(chp2_ratios),ncol=3))
# S2: Malignant Neoplasms Of Digestive Organs
# S7: Malignant Neoplasms Of Breast
# S6: Malignant Neoplasms Of Mesothelial And Soft Tissue
rownames(chp2_ratios_tests)<-c('bowel','sarcomas','breast')
colnames(chp2_ratios_tests)<-c('p_value','l_ci','u_ci')
# Extracting the results from the list
for (i in 1:ncol(chp2_ratios)){
  # p-value
  chp2_ratios_tests$p_value[i]<-round(list_chp2_ttest[[i]]$p.value,5)
  # l ci
  chp2_ratios_tests$l_ci[i]<-list_chp2_ttest[[i]]$conf.int[1]
  # u ci
  chp2_ratios_tests$u_ci[i]<-list_chp2_ttest[[i]]$conf.int[2]
}

#=====================================================================================================#
# Hypothesis testing chapter by subchapter - health services
#=====================================================================================================#

# Subchapter ratios
chp21_ratios<-s_monthly_counts_subchp_21_2020/s_monthly_counts_subchp_21_2019
# List to store the t-test results
list_chp21_ttest<-vector('list',length=ncol(chp21_ratios))
# Performing the t-tests against alternative of mu=1.
list_chp21_ttest<-lapply(chp21_ratios,t.test,mu=1)
# Now storing the results in a matrix: p-value, upper and lower confidence interval
chp21_ratios_tests<-data.frame(matrix(NA,nrow=ncol(chp21_ratios),ncol=3))
# S10: Persons With Potential Health Hazards Related To Socioeconomic And Psychosocial Circumstances
# S15: Persons With Potential Health Hazards Related To Family And Personal History And Certain Conditions Influencing Health Status
rownames(chp21_ratios_tests)<-c('sociecon','family')
colnames(chp21_ratios_tests)<-c('p_value','l_ci','u_ci')
# Extracting the results from the list
for (i in 1:ncol(chp21_ratios)){
  # p-value
  chp21_ratios_tests$p_value[i]<-round(list_chp21_ttest[[i]]$p.value,5)
  # l ci
  chp21_ratios_tests$l_ci[i]<-list_chp21_ttest[[i]]$conf.int[1]
  # u ci
  chp21_ratios_tests$u_ci[i]<-list_chp21_ttest[[i]]$conf.int[2]
}


save.image(file="Data/Data_results/test_data.RData")


