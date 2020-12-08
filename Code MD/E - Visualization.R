#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
##
##  DIAGNOSTIC DETECTION & TELEMEDICINE
##  Visualization
##
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#

##
## Writen date: 29/11/2020
## Last modification date: 29/11/2020

#-------------------------------------------------------------------------------------------------#

# Description:
# This script creates the vizualization for the short article on new diagnostics.

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
library(dplyr)
library(data.table)
library(stringdist)
library(rlang)
library(RColorBrewer)
library(rgdal)
library(ggplot2)
library(aplot)
library(viridis)
library(tidyr)
library(cowplot)
#.................................................................................................
rm(list = ls())


#-------------------------------------------------------------------------------------------------#
##
##  READING DATA
##  
##
#-------------------------------------------------------------------------------------------------#

## Covid-19 incidence by ABS and overall
#-------------------------------------------------------------------------------------------------#
# Overall yearly
yearly_covid19_incidence<-readRDS("Data/Data_results/covid19_incidence_overall.RDS")
# Overall monthly
monthly_covid19_incidence<-readRDS("Data/Data_results/covid19_incidence_overall_monthly.RDS")
# By ABS
yearly_covid19_incidence_abs<-readRDS("Data/Data_results/covid19_incidence_abs.RDS")

## Change in other diagnostics  by ABS and overall
#-------------------------------------------------------------------------------------------------#
# ABS change in other new diagnostics and covid_19 incidence per 100k
yearly_others_incidence_abs<-readRDS("Data/Data_results/abs_overall_change_dd_covid19")
# Saving the raw cases for yearly detections by chapter summed accross ABS
monthly_others_incidence_2019<-readRDS("Data/Data_results/others_new_d_2019.RDS")
monthly_others_incidence_2020<-readRDS("Data/Data_results/others_new_d_2020.RDS")
# Proportional cases by chapter monthly
others_change_2020_2019_monthly<-readRDS("Data/Data_results/others_change_2020_2019_monthly.RDS")
# Proportional cases by chapter yearly
others_change_2020_2019_yearly<-readRDS("Data/Data_results/others_change_2020_2019_yearly.RDS")
# Proportional changes yearly by ABS and chapter
others_change_2020_2019_yearly_abs<-readRDS("Data/Data_results/others_change_2020_2019_yearly_abs.RDS")

## Chapter totals excluding covid-19
#-------------------------------------------------------------------------------------------------#
# 2019 chapter totals
chapter_yearly_2019<-readRDS("Data/Data_results/chapter_diagnostics_2019.RDS")
# 2020 chapter totals
chapter_yearly_2020<-readRDS("Data/Data_results/chapter_diagnostics_2020.RDS")

## Change in summed other diagnostics by month
#-------------------------------------------------------------------------------------------------#
# New diagnostic counts by month 2019 and 2020
monthly_counts_others_2020<-readRDS("Data/Data_results/overall_monthly_others_2020.RDS")
monthly_counts_others_2019<-readRDS("Data/Data_results/overall_monthly_others_2019.RDS")
# Proportional change
others_change_2020_2019_overall_monthly<-readRDS("Data/Data_results/overall_monthly_others_change.RDS")

## Shape files
#-------------------------------------------------------------------------------------------------#
# Attempting to save as RDS
cat_abs_shape<-readRDS("Data/Data_results/cat_abs_shape.RDS")

## Raw visits data, cleaned
#-------------------------------------------------------------------------------------------------#
data_visits<-read.csv("Data/Data_results/DT_1.csv",header=TRUE)


#-------------------------------------------------------------------------------------------------#
##
##  SUMMARY STATISTICS FOR THE RESULTS
##  
##
#-------------------------------------------------------------------------------------------------#

#=====================================================================================================#
# Overall reduction in new diagnosis
#=====================================================================================================#

# From the chapter total files, we do overall minus the year column
total_diagnosis_2019<-sum(subset(chapter_yearly_2019,select=-c(year)))
total_diagnosis_2020<-sum(subset(chapter_yearly_2020,select=-c(year)))
# Then the ratio
overall_ratio_2020_2019<-total_diagnosis_2020/total_diagnosis_2019

# Restricting it to the months of the pandemic
pandemic_months_diagnosis_2020<-sum(monthly_counts_others_2020$others_counts[which(monthly_counts_others_2020$month>2)])
pandemic_months_diagnosis_2019<-sum(monthly_counts_others_2019$others_counts[which(monthly_counts_others_2019$month>2)])
# Then the ratio
pandemic_months_ratio_2020_2019<-pandemic_months_diagnosis_2020/pandemic_months_diagnosis_2019

# Monthly reductions of diagnostics
others_change_2020_2019_overall_monthly
# April 
april<-1-others_change_2020_2019_overall_monthly[4,2]
# May
may<-1-others_change_2020_2019_overall_monthly[5,2]
# November
november<-1-others_change_2020_2019_overall_monthly[11,2]

#=====================================================================================================#
# Number of visits
#=====================================================================================================#

#table(data_visits$visi_tipus_visita)
#9C visita presencial
#9T visita telefònica
#9R visita RESERVADA. Son visites presencials més llargues (per exemple revisions pediàtriques)
#9E visites Electròniques o no presencials
#9D visites Domicili 
# Coding telemedicine vs ftf visits
data_visits$tm_visit<-ifelse(data_visits$visi_tipus_visita=='9T'|data_visits$visi_tipus_visita=='9E',1,0)
data_visits$ftf_visit<-ifelse(data_visits$visi_tipus_visita=='9C'|data_visits$visi_tipus_visita=='9D',
                              data_visits$visi_tipus_visita=='9R'|1,0)
# Splitting sample by year
data_visits_2019<-data_visits[which(year(data_visits$visi_data_visita)==2019),]
data_visits_2020<-data_visits[which(year(data_visits$visi_data_visita)==2020),]
# Number of visits by year, all included
visits_2019<-nrow(data_visits_2019)
visits_2020<-nrow(data_visits_2020)
# Unique individuals
unique_individuals_2019<-length(unique(data_visits_2019$usua_cip))
unique_individuals_2020<-length(unique(data_visits_2020$usua_cip))
# Proportion telemedicine
prop_tm_2019<-prop.table(table(data_visits_2019$tm_visit))
prop_tm_2020<-prop.table(table(data_visits_2020$tm_visit))

#-------------------------------------------------------------------------------------------------#
##
##  PANEL XX: GEOMAP OF COVID-19 INCIDENCE
##  
##
#-------------------------------------------------------------------------------------------------#
# Merge data with spatial object
cat_abs_shape <- merge(cat_abs_shape, yearly_covid19_incidence_abs, by.x="NOMABS", by.y="abs_name")
# Plot map of variable
spplot(cat_abs_shape,"covid19_100k",col = "grey")

# Grey out nas
#https://stackoverflow.com/questions/30888075/set-color-for-na-value-with-spplot-in-r

#-------------------------------------------------------------------------------------------------#
##
##  PANEL XX: CHAPTER HEATMAP
##  
##
#-------------------------------------------------------------------------------------------------#

# Matrix to use in the heatmap
others_change_monthly<-as.matrix(others_change_2020_2019_monthly)
rownames(others_change_2020_2019_monthly) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov")
# List of chatpers
list_ch <- c(paste("chp_",seq(1:21),sep=""))
colnames(others_change_2020_2019_monthly) <-list_ch
# Transposing for plotting
others_change_2020_2019_monthly<-t(others_change_2020_2019_monthly)
# Ordering by "worse" May month (i.e.fewer detections)
others_change_2020_2019_monthly <- others_change_2020_2019_monthly[order(others_change_2020_2019_monthly[,5]),]
heatmap(others_change_2020_2019_monthly, Colv = NA, Rowv = NA, col = colorRampPalette(brewer.pal(8, "Blues"))(25))


#-------------------------------------------------------------------------------------------------#
##
##  PANEL XX: COVID INCIDENCE HISTOGRAM / LINE?
##  
##
#-------------------------------------------------------------------------------------------------#

# The data is stored in: monthly_covid19_incidence
# Creating month row names
rownames(monthly_covid19_incidence)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov")
# Bar plot time series
plot_covid19_monthly_incidence<-ggplot(monthly_covid19_incidence, aes(x = rownames(monthly_covid19_incidence), 
                                                                      y = monthly_covid19_incidence$covid19_100k)) +
                                geom_bar(stat = "identity")
# Adding labels     
plot_covid19_monthly_incidence +labs(title="COVID-19 incidence through November 21th, 2020",
                       x ="month", y = "COVID-19 incidence")


#-------------------------------------------------------------------------------------------------#
##
##  PANEL XX: ABS LEVEL CUMULATED NEW DIAGNOSTICS ON CUMULATED INCIDENCE
##  
##
#-------------------------------------------------------------------------------------------------#

## Data for model 1: Cumulated ABS overall change in new diagnostics and cumulated ABS incidence
#-------------------------------------------------------------------------------------------------#
# Detection change data
overall_change_abs<-data.frame(abs_name=yearly_others_incidence_abs$abs_name,others=yearly_others_incidence_abs$overall_dd_change)
# Adding covid-19 incidence
overall_change_abs$covid19_incidence_100k<-yearly_covid19_incidence_abs$covid19_100k

## Elasticity - Log log model
#-------------------------------------------------------------------------------------------------#
# Elasticity model
model_elasticity_overall_abs<-lm(log(others) ~ log(covid19_incidence_100k), data=overall_change_abs)
# Checking the prediction
for (i in 1:nrow(overall_change_abs)){
overall_change_abs$predicted[i]<-exp(model_elasticity_overall_abs$coefficients[1]+
                                       model_elasticity_overall_abs$coefficients[2]*log(overall_change_abs$covid19_incidence_100k[i]))
}
summary(model_elasticity_overall_abs)

## Plotting the model
#-------------------------------------------------------------------------------------------------#

plot(log(overall_change_abs$covid19_incidence_100k), y =log(overall_change_abs$others))

plot_elasticity_overall_abs<-ggplot(overall_change_abs, aes(x = log(covid19_incidence_100k), y =log(others))) + 
                            geom_point() +
                            stat_smooth(method = "lm", col = "red")

plot_elasticity_overall_abs +labs(title="New diagnostics elasticity to COVID-19 incidence",
        x ="(log) COVID-19 incidence", y = "New diagnostics growth rate")


plot_overall_abs<-ggplot(overall_change_abs[which(overall_change_abs$abs_name!='ALT BERGUEDÀ'),], aes(x = covid19_incidence_100k, y =others)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

plot_overall_abs +labs(title="New diagnostics elasticity to COVID-19 incidence",
                                 x ="COVID-19 incidence", y = "New diagnostics growth rate")

#-------------------------------------------------------------------------------------------------#
##
##  PANEL XX: MONTHLY OVERALL DETECTIONS AND COVID-INCIDENCE FOR ALL ABS
##  
##
#-------------------------------------------------------------------------------------------------#

## Data for model 2: Monthly overall change in new diagnostics and monthly incidence
#-------------------------------------------------------------------------------------------------#
# Detection change data
overall_change_monthly<-data.frame(month=others_change_2020_2019_overall_monthly$month,
                                   others=others_change_2020_2019_overall_monthly$others_change)
# Adding covid-19 incidence
overall_change_monthly$covid19_incidence_100k<-monthly_covid19_incidence$covid19_100k

plot_overall_monthly<-ggplot(overall_change_monthly, aes(x = covid19_incidence_100k, y =others)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

plot_overall_abs +labs(title="New diagnostics elasticity to COVID-19 incidence",
                       x ="COVID-19 incidence", y = "New diagnostics growth rate")

## Storing the workspace
#-------------------------------------------------------------------------------------------------#
save.image(file="Data/Data_results/viz_data.RData")


new_diagnostics_comparison<-readRDS("Data/Data_results/new_diagnostics_comparison.RDS")

my_chapters<-c('Infectious (Chp.1)', 'Neoplasms (Chp.2)','Blood & immune (Chp.3)',
               'Endoc.& nutrit.& metabol. (Chp.4)',
               'Mental & behav. (Chp.5)',
               'Nervous (Chp.6)',
               'Eye & adnexa (Chp.7)',
               'Ear & mastoid (Chp.8)',
               'Circulatory (Chp.9)',
               'Respiratory (Chp.10)',
               'Digestive (Chp.11)',
               'Skin (Chp.12)',
               'Musculoskeletal & connective (Chp.13)',
               'Genitourinary (Chp.14)',
               'Pregnancy & childbirth & puerperium (Chp.15)',
               'Perinatal (Chp.16)',
               'Congenital malf. (Chp.17)',
               'Not elsewhere clas. (Chp.18)',
               'Injury & poison (Chp.19)',
               'External (Chp.20)',
               'Factors contact with health services (Chp.21)')

rownames(new_diagnostics_comparison)<-my_chapters
my_months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov")
## Heatmap updates
# Matrix to use in the heatmap: new_diagnostics_comparison; order chapters by "Overall" column values
#hm<-as.data.frame(t(new_diagnostics_comparison))
hm<-as.data.frame(t(subset(new_diagnostics_comparison, select=-c(Overall))))
hm$month<-rownames(hm)
hm_melt<-reshape2::melt(hm, id.vars=c("month"))
# Dataframe to use in barplot: monthly_covid19_incidence
monthly_covid19_incidence$month_name<-my_months

my_chapter_ordered<-rownames(new_diagnostics_comparison)[order(-new_diagnostics_comparison$Overall)] #lowest last, highest first
#part 1: heat
heat_part<- hm_melt %>%
  #arrange(value) %>%
  mutate(variable=factor(variable,levels=my_chapter_ordered))%>%
  mutate(month = factor(month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov"))) %>% #set order of the months (minus Dec)
  ggplot(aes(x=month,y=variable,fill=value)) + geom_tile() + 
  scale_x_discrete(position = "top")  + #move x-axis on TOP
  scale_y_discrete(position = "right") + #move y-axis on RHS
  scale_fill_viridis(discrete=FALSE, option="D", begin=1,end=0,alpha=1) + theme_bw() + ylab("") + xlab("") + labs(fill="Ratio of new diagnostics") +
  theme(axis.text.x = element_text(size=12), legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_text(size = 10), legend.key.size = unit(0.75,"cm"),
        legend.text = element_text(size = 7),
        panel.grid.minor=element_blank(),plot.background=element_blank(),plot.margin=margin(r=-1, unit="cm")) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))
# Ratio of new diagnostics (2020/2019)

#removed of legend
heat_part_clean<-heat_part + theme(#axis.title.y = element_blank(), axis.text.y = element_blank(),
  #axis.ticks.y = element_blank(), axis.title.x = element_blank(),
  #axis.text.x = element_blank(), axis.ticks.x = element_blank(),
  legend.position="none")
#part 2: heat legend
l <- ggplot_gtable(ggplot_build(heat_part))
leg <- which(sapply(l$grobs, function(x) x$name) == "guide-box")
heat_legend <- l$grobs[[leg]]

#part 3: barplots
bar_part<- monthly_covid19_incidence %>%
  mutate(month_name = factor(month_name, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov"))) %>% #set order of the months (minus Dec)
  ggplot(aes(x = month_name, y = covid19_100k)) + 
  scale_y_continuous(position = "right", limits = c(0,1150)) + #move y-axis on RHS
  geom_bar(stat = "identity", aes(fill = covid19_100k),
           width = 1) + theme_bw() + xlab("") + ylab("") +#ylim(0,270)+
  annotate("text", x = 3, y = 1150, label = "First wave", color="gray20", size = 2) +
  #annotate("text", x = 7.5, y = 810, label = "Second wave", color="gray20", size = 2) +
  annotate("text", x = 10, y = 1120, label = "Second wave", color="gray20", size = 2) +
  ggtitle("COVID-19 incidence")+
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),axis.text.y = element_text(), axis.title.y = element_text(size=10),
        axis.ticks.y = element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        plot.title = element_text(colour="gray20",size=12),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + scale_fill_gradient(low="grey90",high="black")#scale_fill_grey()
#scale_fill_viridis(discrete=FALSE, option="B", begin=1,end=0.25,alpha=1) 

#part 4: vectors of values next to chapters
chapter_yearly_2019<-as.data.frame(chapter_yearly_2019)
chapter_yearly_2020<-as.data.frame(chapter_yearly_2020)
colnames(chapter_yearly_2019)<-c("year",my_chapters)
colnames(chapter_yearly_2020)<-c("year",my_chapters)
##ratio third vector
chapter_yearly_ratio<-chapter_yearly_2020/chapter_yearly_2019
chapter_yearly_ratio<-round(chapter_yearly_ratio,3) #round off ratio values
chapter_yearly<-rbind(chapter_yearly_2019,chapter_yearly_2020,chapter_yearly_ratio)
chapter_yearly$year<-as.factor(c("2020","2019","   2020/2019"))

tmp<-as.data.frame(gather(chapter_yearly, chapter, sum, `Infectious (Chp.1)`:`Factors contact with health services (Chp.21)`, factor_key=TRUE))
tmp$chapter<-as.factor(tmp$chapter)
tmp$year<-as.factor(tmp$year)
tmp$sum<-as.numeric(tmp$sum)
vector_part <- tmp%>%
  mutate(year = factor(year, levels=c("2020","2019","   2020/2019"))) %>% #set order of the year
  mutate(chapter = factor(chapter, levels=my_chapter_ordered)) %>% #set order of the chpts
  ggplot(aes(x = year, y = chapter)) + #fill=sum
  geom_tile(color="gray20",fill="white") + geom_text(aes(label = paste0(sum), color=sum),size=2.5,fontface="bold") +
  theme_bw() + coord_fixed(ratio=1) +
  scale_x_discrete(position = "top")  + #move x-axis on TOP
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),#element_text(size = 10), 
        axis.title.y = element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),plot.margin=margin(l=-2, unit="cm"),
        legend.position="none") + scale_color_gradient(low="grey50",high="black")
#scale_color_viridis(option="A", begin=0.75,end=0.0,alpha=1)


## put it together:

#grob.title <- textGrob("Main Title", hjust = 0.5, vjust = 0.5, gp = gpar(fontsize = 20))
#grid.arrange(bar_part, bar_part, heat_part, heat_part, nrow = 2, ncol = 2, 
#widths = c(40, 40), heights = c(20, 60), top = grob.title)
a<-plot_grid(bar_part, heat_part_clean, align = "v", nrow = 2, rel_heights = c(1/10, 9/10),rel_widths = c(4/5,1))
b<-plot_grid(heat_legend, vector_part, nrow=2, rel_heights = c((1-0.88), 0.88),rel_widths = c(1,1))
plot_grid(a, b, ncol=2, align = "h", rel_heights=c(1,5/10),rel_widths=c(4/5,1/5))

#export to pdf using device size 10" x 10"
