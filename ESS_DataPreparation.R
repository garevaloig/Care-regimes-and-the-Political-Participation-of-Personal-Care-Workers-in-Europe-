################################################################################
## The Politics of Carework
## Data Preparation ESS (All waves)
################################################################################

####
## 1) LOAD RELEVANT PACKAGES
####

library(foreign)
library(dplyr)
library(plyr)
library(devtools)
library(occupationcross)

################################

####
## 2) IMPORT DATA
###

# Change working directory to data location
setwd("C:/Users/gareval1/OneDrive - UvA/Desktop/UvA/Politics of Carework/Data/European Social Survey/All Waves")

# Import all datasets from .sav files
ESS1<-read.spss("ESS1.sav")
ESS2<-read.spss("ESS2.sav");ESS2IT<-read.spss("ESS2IT.sav") # Italy comes as a separate file in Wave 2
ESS3<-read.spss("ESS3.sav")
#ESS3RO<-read.spss("ESS3RO_noweights.sav");summary(ESS3RO$pspwght) # Analysis weights cannot be computed for Romania in Wave 3
#ESS3LV<-read.spss("ESS3LV_noweights.sav");summary(ESS3LV$pspwght) # Analysis weights cannot be computed for Latvia in Wave 3
ESS4<-read.spss("ESS4.sav");ESS4AT<-read.spss("ESS4AT.sav")
#ESS4LT<-read.spss("ESS4LT_noweights.sav");summary(ESS4LT$pspwght) # Analysis weights cannot be computed for Lithuania in Wave 4
ESS5<-read.spss("ESS5.sav");ESS5AT<-read.spss("ESS5AT.sav")
ESS6<-read.spss("ESS6.sav")
ESS7<-read.spss("ESS7.sav")
ESS8<-read.spss("ESS8.sav")
ESS9<-read.spss("ESS9.sav")
ESS10<-read.spss("ESS10.sav")
ESS11<-read.spss("ESS11.sav")

# Convert all datasets to data frames
ESS1<-data.frame(ESS1)
ESS2<-data.frame(ESS2);ESS2IT<-data.frame(ESS2IT)
ESS3<-data.frame(ESS3)
ESS4<-data.frame(ESS4);ESS4AT<-data.frame(ESS4AT)
ESS5<-data.frame(ESS5);ESS5AT<-data.frame(ESS5AT)
ESS6<-data.frame(ESS6)
ESS7<-data.frame(ESS7)
ESS8<-data.frame(ESS8)
ESS9<-data.frame(ESS9)
ESS10<-data.frame(ESS10)
ESS11<-data.frame(ESS11)

# Add country specific datasets to cross-national datasets
# ESS2 and Italy
mean(colnames(ESS2IT) %in% colnames(ESS2)) # If =1, then both datasets have exactly the same columns and can be combined directly
ESS2<-bind_rows(ESS2,ESS2IT);rm(ESS2IT) # Combine in one dataset and delete Italian file

# ESS4 and Austria
mean(colnames(ESS4AT) %in% colnames(ESS4));ncol(ESS4AT);ncol(ESS4) # The Austria dataset has 1 fewer column
colnames(ESS4)[-which(colnames(ESS4) %in% colnames(ESS4AT))] # The general dataset includes anweights (column 10)
ESS4AT$anweight<-ESS4AT$pspwght*ESS4AT$pweight # Calculate anweights for Austria
ESS4<-bind_rows(ESS4,ESS4AT);rm(ESS4AT) # Combine in one dataset and delete Austrian file

# ESS5 and Austria
mean(colnames(ESS5AT) %in% colnames(ESS5));ncol(ESS5AT);ncol(ESS5) # The Austria dataset has 11 more columns
dlt<-c(1:ncol(ESS5AT))[-which(colnames(ESS5AT) %in% colnames(ESS5))] # The positions of the extra columns
ESS5AT<-ESS5AT[,-dlt] # Delete extra columns
ncol(ESS5AT) # Now the general dataset has 3 more columns. One is anweights (column 10) and the others are Ireland-specific questions with a large number of missings
ESS5AT$anweight<-ESS5AT$pspwght*ESS5AT$pweight # Calculate anweights for Austria
ESS5<-ESS5[,-c(83,112)] # Delete 2 extra columns in general dataset
ESS5<-bind_rows(ESS5,ESS5AT);rm(ESS5AT) # Combine in one dataset and delete Austrian file

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

################################

####
## 3) STANDARDISE KEY VARIABLES ACROSS DATASETS
###

# We are going to create new datasets (CarePolitics#) with the relevant variables for the project in each wave.
# All datasets will have identical column names so that they can be combined in one single CarePolitics pooled dataset.
# When key variables are absent for some waves, the variable will be created and filled wit NAs.

### Country, year and weights
## country (cntry)
CarePolitics1<-data.frame(ESS1$cntry);colnames(CarePolitics1)="cntry";CarePolitics1$cntry<-as.factor(CarePolitics1$cntry)
CarePolitics2<-data.frame(ESS2$cntry);colnames(CarePolitics2)="cntry";CarePolitics2$cntry<-as.factor(CarePolitics2$cntry)
CarePolitics3<-data.frame(ESS3$cntry);colnames(CarePolitics3)="cntry";CarePolitics3$cntry<-as.factor(CarePolitics3$cntry)
CarePolitics4<-data.frame(ESS4$cntry);colnames(CarePolitics4)="cntry";CarePolitics4$cntry<-as.factor(CarePolitics4$cntry)
CarePolitics5<-data.frame(ESS5$cntry);colnames(CarePolitics5)="cntry";CarePolitics5$cntry<-as.factor(CarePolitics5$cntry)
CarePolitics6<-data.frame(ESS6$cntry);colnames(CarePolitics6)="cntry";CarePolitics6$cntry<-as.factor(CarePolitics6$cntry)
CarePolitics7<-data.frame(ESS7$cntry);colnames(CarePolitics7)="cntry";CarePolitics7$cntry<-as.factor(CarePolitics7$cntry)
CarePolitics8<-data.frame(ESS8$cntry);colnames(CarePolitics8)="cntry";CarePolitics8$cntry<-as.factor(CarePolitics8$cntry)
CarePolitics9<-data.frame(ESS9$cntry);colnames(CarePolitics9)="cntry";CarePolitics9$cntry<-as.factor(CarePolitics9$cntry)
CarePolitics10<-data.frame(ESS10$cntry);colnames(CarePolitics10)="cntry";CarePolitics10$cntry<-as.factor(CarePolitics10$cntry)
CarePolitics11<-data.frame(ESS11$cntry);colnames(CarePolitics11)="cntry";CarePolitics11$cntry<-as.factor(CarePolitics11$cntry)

## wave
CarePolitics1$wave<-rep(1,nrow(CarePolitics1))
CarePolitics2$wave<-rep(2,nrow(CarePolitics2))
CarePolitics3$wave<-rep(3,nrow(CarePolitics3))
CarePolitics4$wave<-rep(4,nrow(CarePolitics4))
CarePolitics5$wave<-rep(5,nrow(CarePolitics5))
CarePolitics6$wave<-rep(6,nrow(CarePolitics6))
CarePolitics7$wave<-rep(7,nrow(CarePolitics7))
CarePolitics8$wave<-rep(8,nrow(CarePolitics8))
CarePolitics9$wave<-rep(9,nrow(CarePolitics9))
CarePolitics10$wave<-rep(10,nrow(CarePolitics10))
CarePolitics11$wave<-rep(11,nrow(CarePolitics11))

## year of interview (use start year)
CarePolitics1$year<-as.factor(ESS1$inwyr)
CarePolitics2$year<-as.factor(ESS2$inwyr)
CarePolitics3$year<-as.factor(ESS3$inwyys)
CarePolitics4$year<-as.factor(ESS4$inwyys)
CarePolitics5$year<-as.factor(ESS5$inwyys)
CarePolitics6$year<-as.factor(ESS6$inwyys)
CarePolitics7$year<-as.factor(ESS7$inwyys)
CarePolitics8$year<-as.factor(ESS8$inwyys)
CarePolitics9$year<-as.factor(ESS9$inwyys)
# Wave 10: years not directly available, using each country's main year of fieldwork
CarePolitics10$year[which(CarePolitics10$cntry %in% c("Austria","Bulgaria","Croatia","Czechia","Estonia","Finland","France","Germany",
                                    "Hungary","Iceland","Italy","Latvia","Lithuania","North Macedonia","Norway",
                                    "Portugal","Slovakia","Slovenia","Sweden","Switzerland"))]=2021
CarePolitics10$year[which(CarePolitics10$cntry %in% c("Belgium","Cyprus","Greece","Ireland","Israel","Montenegro","Netherlands",
                                    "Poland","Serbia","Spain","United Kingdom"))]=2022
CarePolitics10$year<-as.factor(CarePolitics10$year)
# Wave 11: years not directly available, using each country's main year of fieldwork
CarePolitics11$year[which(CarePolitics11$cntry %in% c("Austria","Belgium","Cyprus","Croatia","Finland","France","Germany","Hungary",
                                    "Ireland","Lithuania","Netherlands","Norway","Portugal","Slovakia","Slovenia",
                                    "Sweden","Switzerland","United Kingdom"))]=2023
CarePolitics11$year[which(CarePolitics11$cntry %in% c("Bulgaria","Czechia","Greece","Iceland","Israel","Italy","Latvia","Montenegro",
                                    "Poland","Serbia","Spain"))]=2024
CarePolitics11$year<-as.factor(CarePolitics11$year)

## analysis weights (anweight)
CarePolitics1$anweight<-ESS1$anweight
ESS2$anweight<-ESS2$pspwght*ESS2$pweight;CarePolitics2$anweight<-ESS2$anweight
ESS3$anweight<-ESS3$pspwght*ESS3$pweight;CarePolitics3$anweight<-ESS3$anweight
CarePolitics4$anweight<-ESS4$anweight
CarePolitics5$anweight<-ESS5$anweight
CarePolitics6$anweight<-ESS6$anweight
CarePolitics7$anweight<-ESS7$anweight
CarePolitics8$anweight<-ESS8$anweight
CarePolitics9$anweight<-ESS9$anweight
CarePolitics10$anweight<-ESS10$anweight
CarePolitics11$anweight<-ESS11$anweight

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

### Identifying care workers
##Occupation (as isco88 numeric codes: only waves 1-5)
# Recode categorical isco88 variables to numeric codes: use function rcd_isco88 in script "ESS_RecodeIsco.R"
CarePolitics1$isco88<-rcd_isco88(ESS1$iscoco)
CarePolitics2$isco88<-rcd_isco88(ESS2$iscoco)
CarePolitics3$isco88<-rcd_isco88(ESS3$iscoco)
CarePolitics4$isco88<-rcd_isco88(ESS4$iscoco)
CarePolitics5$isco88<-rcd_isco88(ESS5$iscoco)
CarePolitics6$isco88<-rep(NA,nrow(CarePolitics6)) # For waves 6-11 we still create an empty variable to allow posterior matching
CarePolitics7$isco88<-rep(NA,nrow(CarePolitics7))
CarePolitics8$isco88<-rep(NA,nrow(CarePolitics8))
CarePolitics9$isco88<-rep(NA,nrow(CarePolitics9))
CarePolitics10$isco88<-rep(NA,nrow(CarePolitics10))
CarePolitics11$isco88<-rep(NA,nrow(CarePolitics11))

##Occupation (as isco08 numeric codes: use function rcd_isco08 in script "ESS_RecodeIsco.R")
CarePolitics1$isco08<-rep(NA,nrow(CarePolitics1))
CarePolitics2$isco08<-rep(NA,nrow(CarePolitics2))
CarePolitics3$isco08<-rep(NA,nrow(CarePolitics3))
CarePolitics4$isco08<-rep(NA,nrow(CarePolitics4))
CarePolitics5$isco08<-rep(NA,nrow(CarePolitics5))
CarePolitics6$isco08<-rcd_isco08(ESS6$isco08)
CarePolitics7$isco08<-rcd_isco08(ESS7$isco08)
CarePolitics8$isco08<-rcd_isco08(ESS8$isco08)
CarePolitics9$isco08<-rcd_isco08(ESS9$isco08)
CarePolitics10$isco08<-rcd_isco08(ESS10$isco08)
CarePolitics11$isco08<-rcd_isco08(ESS11$isco08)

# ISCO08 codes and skill level for all waves: note that there is not full equivalence and many categories are lost
# when traslating to isco08. The best approach is probably to employ isco88 until wave 5 and find the equivalences
# in the key categories (careworkers)
CarePolitics1<-reclassify_to_isco08(base=CarePolitics1,variable=isco88,classif_origin="ISCO88",
                           add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics2<-reclassify_to_isco08(base=CarePolitics2,variable=isco88,classif_origin="ISCO88",
                           add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics3<-reclassify_to_isco08(base=CarePolitics3,variable=isco88,classif_origin="ISCO88",
                           add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics4<-reclassify_to_isco08(base=CarePolitics4,variable=isco88,classif_origin="ISCO88",
                           add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics5<-reclassify_to_isco08(base=CarePolitics5,variable=isco88,classif_origin="ISCO88",
                           add_major_groups=F,add_skill=T,code_titles=F)

CarePolitics6<-reclassify_to_isco08(base=CarePolitics6,variable=isco08,classif_origin="ISCO08",
                           add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics7<-reclassify_to_isco08(base=CarePolitics7,variable=isco08,classif_origin="ISCO08",
                            add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics8<-reclassify_to_isco08(base=CarePolitics8,variable=isco08,classif_origin="ISCO08",
                            add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics9<-reclassify_to_isco08(base=CarePolitics9,variable=isco08,classif_origin="ISCO08",
                            add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics10<-reclassify_to_isco08(base=CarePolitics10,variable=isco08,classif_origin="ISCO08",
                            add_major_groups=F,add_skill=T,code_titles=F)
CarePolitics11<-reclassify_to_isco08(base=CarePolitics11,variable=isco08,classif_origin="ISCO08",
                                add_major_groups=F,add_skill=T,code_titles=F)

colnames(CarePolitics1)[which(colnames(CarePolitics1)=="ISCO.08")]="isco08_merged" # Note that the merged isco08 variable has lots of missing information (0s)
CarePolitics1<-CarePolitics1[,-which(colnames(CarePolitics1)=="ISCO88")]
colnames(CarePolitics2)[which(colnames(CarePolitics2)=="ISCO.08")]="isco08_merged"
CarePolitics2<-CarePolitics2[,-which(colnames(CarePolitics2)=="ISCO88")]
colnames(CarePolitics3)[which(colnames(CarePolitics3)=="ISCO.08")]="isco08_merged"
CarePolitics3<-CarePolitics3[,-which(colnames(CarePolitics3)=="ISCO88")]
colnames(CarePolitics4)[which(colnames(CarePolitics4)=="ISCO.08")]="isco08_merged"
CarePolitics4<-CarePolitics4[,-which(colnames(CarePolitics4)=="ISCO88")]
colnames(CarePolitics5)[which(colnames(CarePolitics5)=="ISCO.08")]="isco08_merged"
CarePolitics5<-CarePolitics5[,-which(colnames(CarePolitics5)=="ISCO88")]

colnames(CarePolitics6)[which(colnames(CarePolitics6)=="ISCO.08")]="isco08_merged"
colnames(CarePolitics7)[which(colnames(CarePolitics7)=="ISCO.08")]="isco08_merged"
colnames(CarePolitics8)[which(colnames(CarePolitics8)=="ISCO.08")]="isco08_merged"
colnames(CarePolitics9)[which(colnames(CarePolitics9)=="ISCO.08")]="isco08_merged"
colnames(CarePolitics10)[which(colnames(CarePolitics10)=="ISCO.08")]="isco08_merged"
colnames(CarePolitics11)[which(colnames(CarePolitics11)=="ISCO.08")]="isco08_merged"

CarePolitics1$isco08_merged<-as.character(CarePolitics1$isco08_merged) # To allow the posterior merge, these variables have to be characters
CarePolitics2$isco08_merged<-as.character(CarePolitics2$isco08_merged)
CarePolitics3$isco08_merged<-as.character(CarePolitics3$isco08_merged)
CarePolitics4$isco08_merged<-as.character(CarePolitics4$isco08_merged)
CarePolitics5$isco08_merged<-as.character(CarePolitics5$isco08_merged)

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

## Care worker categories
# isco88: 5130 (personal care workers); broader: 2230 (nursing and midwifery professionals), 3230 (nursing and midwifery associate professionals)
# NOTE: THIS VARIABLE HAS A CODING MISTAKE, USE 'pcareworker' INSTEAD
CarePolitics1$care53<-factor(nrow(CarePolitics1),levels=c("No","Yes")) # Care workers
CarePolitics1$care53[which(CarePolitics1$isco88 %in% c("5130","5131","5132","5133"))]="Yes"
CarePolitics1$care53[which(!(CarePolitics1$isco88 %in% c("5130","5131","5132","5133")))]="No"
CarePolitics1$care53[which(is.na(CarePolitics1$isco88))]=NA
CarePolitics1$nurse<-factor(nrow(CarePolitics1),levels=c("No","Yes")) # Nurses
CarePolitics1$nurse[which(CarePolitics1$isco88 %in% c("2230","3230"))]="Yes"
CarePolitics1$nurse[which(!(CarePolitics1$isco88 %in% c("2230","3230")))]="No"
CarePolitics1$nurse[which(is.na(CarePolitics1$isco88))]=NA

CarePolitics2$care53<-factor(nrow(CarePolitics2),levels=c("No","Yes")) # Care workers
CarePolitics2$care53[which(CarePolitics2$isco88 %in% c("5130","5131","5132","5133"))]="Yes"
CarePolitics2$care53[which(!(CarePolitics2$isco88 %in% c("5130","5131","5132","5133")))]="No"
CarePolitics2$care53[which(is.na(CarePolitics2$isco88))]=NA
CarePolitics2$nurse<-factor(nrow(CarePolitics2),levels=c("No","Yes")) # Nurses
CarePolitics2$nurse[which(CarePolitics2$isco88 %in% c("2230","3230"))]="Yes"
CarePolitics2$nurse[which(!(CarePolitics2$isco88 %in% c("2230","3230")))]="No"
CarePolitics2$nurse[which(is.na(CarePolitics2$isco88))]=NA

CarePolitics3$care53<-factor(nrow(CarePolitics3),levels=c("No","Yes")) # Care workers
CarePolitics3$care53[which(CarePolitics3$isco88 %in% c("5130","5131","5132","5133"))]="Yes"
CarePolitics3$care53[which(!(CarePolitics3$isco88 %in% c("5130","5131","5132","5133")))]="No"
CarePolitics3$care53[which(is.na(CarePolitics3$isco88))]=NA
CarePolitics3$nurse<-factor(nrow(CarePolitics3),levels=c("No","Yes")) # Nurses
CarePolitics3$nurse[which(CarePolitics3$isco88 %in% c("2230","3230"))]="Yes"
CarePolitics3$nurse[which(!(CarePolitics3$isco88 %in% c("2230","3230")))]="No"
CarePolitics3$nurse[which(is.na(CarePolitics3$isco88))]=NA

CarePolitics4$care53<-factor(nrow(CarePolitics4),levels=c("No","Yes")) # Care workers
CarePolitics4$care53[which(CarePolitics4$isco88 %in% c("5130","5131","5132","5133"))]="Yes"
CarePolitics4$care53[which(!(CarePolitics4$isco88 %in% c("5130","5131","5132","5133")))]="No"
CarePolitics4$care53[which(is.na(CarePolitics4$isco88))]=NA
CarePolitics4$nurse<-factor(nrow(CarePolitics4),levels=c("No","Yes")) # Nurses
CarePolitics4$nurse[which(CarePolitics4$isco88 %in% c("2230","3230"))]="Yes"
CarePolitics4$nurse[which(!(CarePolitics4$isco88 %in% c("2230","3230")))]="No"
CarePolitics4$nurse[which(is.na(CarePolitics4$isco88))]=NA

CarePolitics5$care53<-factor(nrow(CarePolitics5),levels=c("No","Yes")) # Care workers
CarePolitics5$care53[which(CarePolitics5$isco88 %in% c("5130","5131","5132","5133"))]="Yes"
CarePolitics5$care53[which(!(CarePolitics5$isco88 %in% c("5130","5131","5132","5133")))]="No"
CarePolitics5$care53[which(is.na(CarePolitics5$isco88))]=NA
CarePolitics5$nurse<-factor(nrow(CarePolitics5),levels=c("No","Yes")) # Nurses
CarePolitics5$nurse[which(CarePolitics5$isco88 %in% c("2230","3230"))]="Yes"
CarePolitics5$nurse[which(!(CarePolitics5$isco88 %in% c("2230","3230")))]="No"
CarePolitics5$nurse[which(is.na(CarePolitics5$isco88))]=NA

# isco08: 5300 (personal care workers); broader: 2221 (nursing professionals), 3221 (nursing associate professionals)
CarePolitics6$care53<-factor(nrow(CarePolitics6),levels=c("No","Yes")) # Care workers
CarePolitics6$care53[which(CarePolitics6$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322"))]="Yes"
CarePolitics6$care53[which(!(CarePolitics6$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322")))]="No"
CarePolitics6$care53[which(is.na(CarePolitics6$isco08))]=NA
CarePolitics6$nurse<-factor(nrow(CarePolitics6),levels=c("No","Yes")) # Nurses
CarePolitics6$nurse[which(CarePolitics6$isco08 %in% c("2221","3221"))]="Yes"
CarePolitics6$nurse[which(!(CarePolitics6$isco08 %in% c("2221","3221")))]="No"
CarePolitics6$nurse[which(is.na(CarePolitics6$isco08))]=NA

CarePolitics7$care53<-factor(nrow(CarePolitics7),levels=c("No","Yes"))
CarePolitics7$care53[which(CarePolitics7$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322"))]="Yes"
CarePolitics7$care53[which(!(CarePolitics7$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322")))]="No"
CarePolitics7$care53[which(is.na(CarePolitics7$isco08))]=NA
CarePolitics7$nurse<-factor(nrow(CarePolitics7),levels=c("No","Yes")) # Nurses
CarePolitics7$nurse[which(CarePolitics7$isco08 %in% c("2221","3221"))]="Yes"
CarePolitics7$nurse[which(!(CarePolitics7$isco08 %in% c("2221","3221")))]="No"
CarePolitics7$nurse[which(is.na(CarePolitics7$isco08))]=NA

CarePolitics8$care53<-factor(nrow(CarePolitics8),levels=c("No","Yes"))
CarePolitics8$care53[which(CarePolitics8$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322"))]="Yes"
CarePolitics8$care53[which(!(CarePolitics8$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322")))]="No"
CarePolitics8$care53[which(is.na(CarePolitics8$isco08))]=NA
CarePolitics8$nurse<-factor(nrow(CarePolitics8),levels=c("No","Yes")) # Nurses
CarePolitics8$nurse[which(CarePolitics8$isco08 %in% c("2221","3221"))]="Yes"
CarePolitics8$nurse[which(!(CarePolitics8$isco08 %in% c("2221","3221")))]="No"
CarePolitics8$nurse[which(is.na(CarePolitics8$isco08))]=NA

CarePolitics9$care53<-factor(nrow(CarePolitics9),levels=c("No","Yes"))
CarePolitics9$care53[which(CarePolitics9$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322"))]="Yes"
CarePolitics9$care53[which(!(CarePolitics9$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322")))]="No"
CarePolitics9$care53[which(is.na(CarePolitics9$isco08))]=NA
CarePolitics9$nurse<-factor(nrow(CarePolitics9),levels=c("No","Yes")) # Nurses
CarePolitics9$nurse[which(CarePolitics9$isco08 %in% c("2221","3221"))]="Yes"
CarePolitics9$nurse[which(!(CarePolitics9$isco08 %in% c("2221","3221")))]="No"
CarePolitics9$nurse[which(is.na(CarePolitics9$isco08))]=NA

CarePolitics10$care53<-factor(nrow(CarePolitics10),levels=c("No","Yes"))
CarePolitics10$care53[which(CarePolitics10$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322"))]="Yes"
CarePolitics10$care53[which(!(CarePolitics10$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322")))]="No"
CarePolitics10$care53[which(is.na(CarePolitics10$isco08))]=NA
CarePolitics10$nurse<-factor(nrow(CarePolitics10),levels=c("No","Yes")) # Nurses
CarePolitics10$nurse[which(CarePolitics10$isco08 %in% c("2221","3221"))]="Yes"
CarePolitics10$nurse[which(!(CarePolitics10$isco08 %in% c("2221","3221")))]="No"
CarePolitics10$nurse[which(is.na(CarePolitics10$isco08))]=NA

CarePolitics11$care53<-factor(nrow(CarePolitics11),levels=c("No","Yes"))
CarePolitics11$care53[which(CarePolitics11$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322"))]="Yes"
CarePolitics11$care53[which(!(CarePolitics11$isco08 %in% c("5300","5310","5311","5312","5320","5321","5322")))]="No"
CarePolitics11$care53[which(is.na(CarePolitics11$isco08))]=NA
CarePolitics11$nurse<-factor(nrow(CarePolitics11),levels=c("No","Yes")) # Nurses
CarePolitics11$nurse[which(CarePolitics11$isco08 %in% c("2221","3221"))]="Yes"
CarePolitics11$nurse[which(!(CarePolitics11$isco08 %in% c("2221","3221")))]="No"
CarePolitics11$nurse[which(is.na(CarePolitics11$isco08))]=NA

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

## Political opinion
# Interest in politics (polintr)
CarePolitics1$polintr<-ESS1$polintr;CarePolitics2$polintr<-ESS2$polintr;CarePolitics3$polintr<-ESS3$polintr;CarePolitics4$polintr<-ESS4$polintr
CarePolitics5$polintr<-ESS5$polintr;CarePolitics6$polintr<-ESS6$polintr;CarePolitics7$polintr<-ESS7$polintr;CarePolitics8$polintr<-ESS8$polintr
CarePolitics9$polintr<-ESS9$polintr;CarePolitics10$polintr<-ESS10$polintr;CarePolitics11$polintr<-ESS11$polintr

# Political agency items (not all available in all waves)
CarePolitics1$polcmpl<-ESS1$polcmpl;CarePolitics2$polcmpl<-ESS2$polcmpl;CarePolitics3$polcmpl<-ESS3$polcmpl;CarePolitics4$polcmpl<-ESS4$polcmpl
CarePolitics5$polcmpl<-rep(NA,nrow(CarePolitics5));CarePolitics6$polcmpl<-rep(NA,nrow(CarePolitics6));CarePolitics7$polcmpl<-rep(NA,nrow(CarePolitics7))
CarePolitics8$polcmpl<-rep(NA,nrow(CarePolitics8));CarePolitics9$polcmpl<-rep(NA,nrow(CarePolitics9));CarePolitics10$polcmpl<-rep(NA,nrow(CarePolitics10))
CarePolitics11$polcmpl<-rep(NA,nrow(CarePolitics11))

CarePolitics1$poldcs<-ESS1$poldcs;CarePolitics2$poldcs<-ESS2$poldcs;CarePolitics3$poldcs<-ESS3$poldcs;CarePolitics4$poldcs<-ESS4$poldcs
CarePolitics5$poldcs<-rep(NA,nrow(CarePolitics5));CarePolitics6$poldcs<-rep(NA,nrow(CarePolitics6));CarePolitics7$poldcs<-rep(NA,nrow(CarePolitics7))
CarePolitics8$poldcs<-rep(NA,nrow(CarePolitics8));CarePolitics9$poldcs<-rep(NA,nrow(CarePolitics9));CarePolitics10$poldcs<-rep(NA,nrow(CarePolitics10))
CarePolitics11$poldcs<-rep(NA,nrow(CarePolitics11))

CarePolitics1$polactiv<-ESS1$polactiv;CarePolitics2$polactiv<-rep(NA,nrow(CarePolitics2));CarePolitics3$polactiv<-rep(NA,nrow(CarePolitics3))
CarePolitics4$polactiv<-rep(NA,nrow(CarePolitics4));CarePolitics5$polactiv<-rep(NA,nrow(CarePolitics5));CarePolitics6$polactiv<-rep(NA,nrow(CarePolitics6))
CarePolitics7$polactiv<-rep(NA,nrow(CarePolitics7));CarePolitics8$polactiv<-rep(NA,nrow(CarePolitics8));CarePolitics9$polactiv<-rep(NA,nrow(CarePolitics9))
CarePolitics10$polactiv<-rep(NA,nrow(CarePolitics10));CarePolitics11$polactiv<-rep(NA,nrow(CarePolitics11))

CarePolitics1$pltcare<-ESS1$pltcare;CarePolitics2$pltcare<-rep(NA,nrow(CarePolitics2));CarePolitics3$pltcare<-rep(NA,nrow(CarePolitics3))
CarePolitics4$pltcare<-rep(NA,nrow(CarePolitics4));CarePolitics5$pltcare<-rep(NA,nrow(CarePolitics5));CarePolitics6$pltcare<-rep(NA,nrow(CarePolitics6))
CarePolitics7$pltcare<-rep(NA,nrow(CarePolitics7));CarePolitics8$pltcare<-rep(NA,nrow(CarePolitics8));CarePolitics9$pltcare<-rep(NA,nrow(CarePolitics9))
CarePolitics10$pltcare<-rep(NA,nrow(CarePolitics10));CarePolitics11$pltcare<-rep(NA,nrow(CarePolitics11))

CarePolitics1$pltinvt<-ESS1$pltinvt;CarePolitics2$pltinvt<-rep(NA,nrow(CarePolitics2));CarePolitics3$pltinvt<-rep(NA,nrow(CarePolitics3))
CarePolitics4$pltinvt<-rep(NA,nrow(CarePolitics4));CarePolitics5$pltinvt<-rep(NA,nrow(CarePolitics5));CarePolitics6$pltinvt<-rep(NA,nrow(CarePolitics6))
CarePolitics7$pltinvt<-rep(NA,nrow(CarePolitics7));CarePolitics8$pltinvt<-rep(NA,nrow(CarePolitics8));CarePolitics9$pltinvt<-rep(NA,nrow(CarePolitics9))
CarePolitics10$pltinvt<-rep(NA,nrow(CarePolitics10));CarePolitics11$pltinvt<-rep(NA,nrow(CarePolitics11))

CarePolitics1$ptcpplt<-rep(NA,nrow(CarePolitics1));CarePolitics2$ptcpplt<-rep(NA,nrow(CarePolitics2));CarePolitics3$ptcpplt<-rep(NA,nrow(CarePolitics3))
CarePolitics4$ptcpplt<-rep(NA,nrow(CarePolitics4));CarePolitics5$ptcpplt<-rep(NA,nrow(CarePolitics5));CarePolitics6$ptcpplt<-rep(NA,nrow(CarePolitics6))
CarePolitics7$ptcpplt<-ESS7$ptcpplt;CarePolitics8$ptcpplt<-rep(NA,nrow(CarePolitics8));CarePolitics9$ptcpplt<-rep(NA,nrow(CarePolitics9))
CarePolitics10$ptcpplt<-rep(NA,nrow(CarePolitics10));CarePolitics11$ptcpplt<-rep(NA,nrow(CarePolitics11))

CarePolitics1$actrolg<-rep(NA,nrow(CarePolitics1));CarePolitics2$actrolg<-rep(NA,nrow(CarePolitics2));CarePolitics3$actrolg<-rep(NA,nrow(CarePolitics3))
CarePolitics4$actrolg<-rep(NA,nrow(CarePolitics4));CarePolitics5$actrolg<-rep(NA,nrow(CarePolitics5));CarePolitics6$actrolg<-rep(NA,nrow(CarePolitics6))
CarePolitics7$actrolg<-ESS7$actrolg;CarePolitics8$actrolg<-ESS8$actrolg;CarePolitics9$actrolg<-ESS9$actrolg
CarePolitics10$actrolg<-ESS10$actrolg;CarePolitics11$actrolg<-ESS11$actrolg
CarePolitics7$actrolg<-recode_factor(CarePolitics7$actrolg,"1"="Not at all able","2"="A little able","3"="A little able",
                            "4"="Quite able","5"="Quite able","6"="Quite able",
                            "7"="Very able","8"="Quite able","9"="Completely able") # Armonise levels in W7

CarePolitics1$psppsgv<-rep(NA,nrow(CarePolitics1));CarePolitics2$psppsgv<-rep(NA,nrow(CarePolitics2));CarePolitics3$psppsgv<-rep(NA,nrow(CarePolitics3))
CarePolitics4$psppsgv<-rep(NA,nrow(CarePolitics4));CarePolitics5$psppsgv<-rep(NA,nrow(CarePolitics5));CarePolitics6$psppsgv<-rep(NA,nrow(CarePolitics6))
CarePolitics7$psppsgv<-ESS7$psppsgv;CarePolitics8$psppsgv<-ESS8$psppsgv;CarePolitics9$psppsgv<-ESS9$psppsgv
CarePolitics10$psppsgv<-ESS10$psppsgv;CarePolitics11$psppsgv<-ESS11$psppsgv
CarePolitics7$psppsgv<-recode_factor(CarePolitics7$psppsgv,"1"="Not at all","2"="Very little","3"="Very little",
                            "4"="Some","5"="Some","6"="Some",
                            "7"="A lot","8"="A lot",
                            "9"="A great deal","Completely"="A great deal") # Armonise levels in W7

CarePolitics1$psppipl<-rep(NA,nrow(CarePolitics1));CarePolitics2$psppipl<-rep(NA,nrow(CarePolitics2));CarePolitics3$psppipl<-rep(NA,nrow(CarePolitics3))
CarePolitics4$psppipl<-rep(NA,nrow(CarePolitics4));CarePolitics5$psppipl<-rep(NA,nrow(CarePolitics5));CarePolitics6$psppipl<-rep(NA,nrow(CarePolitics6))
CarePolitics7$psppipl<-ESS7$psppipl;CarePolitics8$psppipl<-ESS8$psppipl;CarePolitics9$psppipl<-ESS9$psppipl
CarePolitics10$psppipl<-ESS10$psppipl;CarePolitics11$psppipl<-ESS11$psppipl
CarePolitics7$psppipl<-recode_factor(CarePolitics7$psppipl,"1"="Not at all","2"="Very little","3"="Very little",
                            "4"="Some","5"="Some","6"="Some",
                            "7"="A lot","8"="A lot",
                            "9"="A great deal","Completely"="A great deal") # Armonise levels in W7

CarePolitics1$cptppol<-rep(NA,nrow(CarePolitics1));CarePolitics2$cptppol<-rep(NA,nrow(CarePolitics2));CarePolitics3$cptppol<-rep(NA,nrow(CarePolitics3))
CarePolitics4$cptppol<-rep(NA,nrow(CarePolitics4));CarePolitics5$cptppol<-rep(NA,nrow(CarePolitics5));CarePolitics6$cptppol<-rep(NA,nrow(CarePolitics6))
CarePolitics7$cptppol<-ESS7$cptppol;CarePolitics8$cptppol<-ESS8$cptppol;CarePolitics9$cptppol<-ESS9$cptppol
CarePolitics10$cptppol<-ESS10$cptppol;CarePolitics11$cptppol<-ESS11$cptppol
CarePolitics7$cptppol<-recode_factor(CarePolitics7$cptppol,"1"="Not at all confident","2"="A little confident","3"="A little confident",
                            "4"="Quite confident","5"="Quite confident","6"="Quite confident",
                            "7"="Very confident","8"="Very confident",
                            "9"="Completely confident") # Armonise levels in W7

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

# Trust items (trstplt, trstep,trstprl,trstprt)
CarePolitics1$trstplt<-ESS1$trstplt;CarePolitics2$trstplt<-ESS2$trstplt;CarePolitics3$trstplt<-ESS3$trstplt;CarePolitics4$trstplt<-ESS4$trstplt
CarePolitics5$trstplt<-ESS5$trstplt;CarePolitics6$trstplt<-ESS6$trstplt;CarePolitics7$trstplt<-ESS7$trstplt;CarePolitics8$trstplt<-ESS8$trstplt
CarePolitics9$trstplt<-ESS9$trstplt;CarePolitics10$trstplt<-ESS10$trstplt;CarePolitics11$trstplt<-ESS11$trstplt

CarePolitics1$trstep<-ESS1$trstep;CarePolitics2$trstep<-ESS2$trstep;CarePolitics3$trstep<-ESS3$trstep;CarePolitics4$trstep<-ESS4$trstep
CarePolitics5$trstep<-ESS5$trstep;CarePolitics6$trstep<-ESS6$trstep;CarePolitics7$trstep<-ESS7$trstep;CarePolitics8$trstep<-ESS8$trstep
CarePolitics9$trstep<-ESS9$trstep;CarePolitics10$trstep<-ESS10$trstep;CarePolitics11$trstep<-ESS11$trstep

CarePolitics1$trstprl<-ESS1$trstprl;CarePolitics2$trstprl<-ESS2$trstprl;CarePolitics3$trstprl<-ESS3$trstprl;CarePolitics4$trstprl<-ESS4$trstprl
CarePolitics5$trstprl<-ESS5$trstprl;CarePolitics6$trstprl<-ESS6$trstprl;CarePolitics7$trstprl<-ESS7$trstprl;CarePolitics8$trstprl<-ESS8$trstprl
CarePolitics9$trstprl<-ESS9$trstprl;CarePolitics10$trstprl<-ESS10$trstprl;CarePolitics11$trstprl<-ESS11$trstprl

CarePolitics1$trstprt<-rep(NA,nrow(CarePolitics1));CarePolitics2$trstprt<-ESS2$trstprt;CarePolitics3$trstprt<-ESS3$trstprt;CarePolitics4$trstprt<-ESS4$trstprt
CarePolitics5$trstprt<-ESS5$trstprt;CarePolitics6$trstprt<-ESS6$trstprt;CarePolitics7$trstprt<-ESS7$trstprt;CarePolitics8$trstprt<-ESS8$trstprt
CarePolitics9$trstprt<-ESS9$trstprt;CarePolitics10$trstprt<-ESS10$trstprt;CarePolitics11$trstprt<-ESS11$trstprt

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

# Feels closer to a particular party (clsprty)
CarePolitics1$clsprty<-ESS1$clsprty;CarePolitics2$clsprty<-ESS2$clsprty;CarePolitics3$clsprty<-ESS3$clsprty;CarePolitics4$clsprty<-ESS4$clsprty
CarePolitics5$clsprty<-ESS5$clsprty;CarePolitics6$clsprty<-ESS6$clsprty;CarePolitics7$clsprty<-ESS7$clsprty;CarePolitics8$clsprty<-ESS8$clsprty
CarePolitics9$clsprty<-ESS9$clsprty;CarePolitics10$clsprty<-ESS10$clsprty;CarePolitics11$clsprty<-ESS11$clsprty

# To which one? (country specific variable, prtclc__)
# Albania
CarePolitics1$prtclal<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclal<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclal<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclal<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtclal<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclal<-ESS6$prtclal
CarePolitics7$prtclal<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclal<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclal<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtclal<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclal<-rep(NA,nrow(CarePolitics11))

# Austria
CarePolitics1$prtclat<-ESS1$prtclat;CarePolitics2$prtclat<-ESS2$prtclat;CarePolitics3$prtclat<-ESS3$prtclaat
CarePolitics4$prtclat<-ESS4$prtclaat;CarePolitics5$prtclat<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclat<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtclat<-ESS7$prtclcat;CarePolitics8$prtclat<-ESS8$prtclcat;CarePolitics9$prtclat<-ESS9$prtcldat
CarePolitics10$prtclat<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclat<-ESS11$prtcleat

# Belgium
CarePolitics1$prtclbe<-ESS1$prtclbe;CarePolitics2$prtclbe<-ESS2$prtclabe;CarePolitics3$prtclbe<-ESS3$prtclabe
CarePolitics4$prtclbe<-ESS4$prtclbbe;CarePolitics5$prtclbe<-ESS5$prtclcbe;CarePolitics6$prtclbe<-ESS6$prtclcbe
CarePolitics7$prtclbe<-ESS7$prtclcbe;CarePolitics8$prtclbe<-ESS8$prtclcbe;CarePolitics9$prtclbe<-ESS9$prtcldbe
CarePolitics10$prtclbe<-ESS10$prtclebe;CarePolitics11$prtclbe<-ESS11$prtclebe

# Bulgaria
CarePolitics1$prtclbg<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclbg<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclbg<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclbg<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtclbg<-ESS5$prtclbbg;CarePolitics6$prtclbg<-ESS6$prtclcbg
CarePolitics7$prtclbg<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclbg<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclbg<-ESS9$prtcldbg
CarePolitics10$prtclbg<-ESS10$prtclebg;CarePolitics11$prtclbg<-ESS11$prtclfbg

# Switzerland
CarePolitics1$prtclch<-ESS1$prtclch;CarePolitics2$prtclch<-ESS2$prtclch;CarePolitics3$prtclch<-ESS3$prtclach
CarePolitics4$prtclch<-ESS4$prtclbch;CarePolitics5$prtclch<-ESS5$prtclcch;CarePolitics6$prtclch<-ESS6$prtcldch
CarePolitics7$prtclch<-ESS7$prtclech;CarePolitics8$prtclch<-ESS8$prtclfch;CarePolitics9$prtclch<-ESS9$prtclgch
CarePolitics10$prtclch<-ESS10$prtclhch;CarePolitics11$prtclch<-ESS11$prtclhch

# Cyprus
CarePolitics1$prtclcy<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclcy<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclcy<-ESS3$prtclcy
CarePolitics4$prtclcy<-ESS4$prtclcy;CarePolitics5$prtclcy<-ESS5$prtclcy;CarePolitics6$prtclcy<-ESS6$prtclacy
CarePolitics7$prtclcy<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclcy<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclcy<-ESS9$prtclbcy
CarePolitics10$prtclcy<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclcy<-ESS11$prtclccy

# Czech Republic
CarePolitics1$prtclcz<-ESS1$prtclcz;CarePolitics2$prtclcz<-ESS2$prtclcz;CarePolitics3$prtclcz<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclcz<-ESS4$prtclacz;CarePolitics5$prtclcz<-ESS5$prtclbcz;CarePolitics6$prtclcz<-ESS6$prtclccz
CarePolitics7$prtclcz<-ESS7$prtcldcz;CarePolitics8$prtclcz<-ESS8$prtcldcz;CarePolitics9$prtclcz<-ESS9$prtclecz
CarePolitics10$prtclcz<-ESS10$prtclecz;CarePolitics11$prtclcz<-rep(NA,nrow(CarePolitics11))

# Germany
CarePolitics1$prtclde<-ESS1$prtclde;CarePolitics2$prtclde<-ESS2$prtclade;CarePolitics3$prtclde<-ESS3$prtclbde
CarePolitics4$prtclde<-ESS4$prtclbde;CarePolitics5$prtclde<-ESS5$prtclcde;CarePolitics6$prtclde<-ESS6$prtcldde
CarePolitics7$prtclde<-ESS7$prtclede;CarePolitics8$prtclde<-ESS8$prtclede;CarePolitics9$prtclde<-ESS9$prtclede
CarePolitics10$prtclde<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclde<-ESS11$prtclgde

# Denmark
CarePolitics1$prtcldk<-ESS1$prtcldk;CarePolitics2$prtcldk<-ESS2$prtcldk;CarePolitics3$prtcldk<-ESS3$prtcladk
CarePolitics4$prtcldk<-ESS4$prtclbdk;CarePolitics5$prtcldk<-ESS5$prtclbdk;CarePolitics6$prtcldk<-ESS6$prtclcdk
CarePolitics7$prtcldk<-ESS7$prtclcdk;CarePolitics8$prtcldk<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtcldk<-ESS9$prtclddk
CarePolitics10$prtcldk<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtcldk<-rep(NA,nrow(CarePolitics11))

# Estonia
CarePolitics1$prtclee<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclee<-ESS2$prtclee;CarePolitics3$prtclee<-ESS3$prtclaee
CarePolitics4$prtclee<-ESS4$prtclbee;CarePolitics5$prtclee<-ESS5$prtclcee;CarePolitics6$prtclee<-ESS6$prtcldee
CarePolitics7$prtclee<-ESS7$prtcleee;CarePolitics8$prtclee<-ESS8$prtclfee;CarePolitics9$prtclee<-ESS9$prtclgee
CarePolitics10$prtclee<-ESS10$prtclhee;CarePolitics11$prtclee<-rep(NA,nrow(CarePolitics11))

# Spain
CarePolitics1$prtcles<-ESS1$prtcles;CarePolitics2$prtcles<-ESS2$prtclaes;CarePolitics3$prtcles<-ESS3$prtclaes
CarePolitics4$prtcles<-ESS4$prtclbes;CarePolitics5$prtcles<-ESS5$prtclbes;CarePolitics6$prtcles<-ESS6$prtclces
CarePolitics7$prtcles<-ESS7$prtcldes;CarePolitics8$prtcles<-ESS8$prtclees;CarePolitics9$prtcles<-ESS9$prtclfes
CarePolitics10$prtcles<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtcles<-ESS11$prtclhes

# Finland
CarePolitics1$prtclfi<-ESS1$prtclfi;CarePolitics2$prtclfi<-ESS2$prtclfi;CarePolitics3$prtclfi<-ESS3$prtclfi
CarePolitics4$prtclfi<-ESS4$prtclafi;CarePolitics5$prtclfi<-ESS5$prtclbfi;CarePolitics6$prtclfi<-ESS6$prtclcfi
CarePolitics7$prtclfi<-ESS7$prtclcfi;CarePolitics8$prtclfi<-ESS8$prtcldfi;CarePolitics9$prtclfi<-ESS9$prtclefi
CarePolitics10$prtclfi<-ESS10$prtclffi;CarePolitics11$prtclfi<-ESS11$prtclgfi

# France
CarePolitics1$prtclfr<-ESS1$prtclfr;CarePolitics2$prtclfr<-ESS2$prtclfr;CarePolitics3$prtclfr<-ESS3$prtclafr
CarePolitics4$prtclfr<-ESS4$prtclbfr;CarePolitics5$prtclfr<-ESS5$prtclcfr;CarePolitics6$prtclfr<-ESS6$prtcldfr
CarePolitics7$prtclfr<-ESS7$prtcldfr;CarePolitics8$prtclfr<-ESS8$prtclefr;CarePolitics9$prtclfr<-ESS9$prtclffr
CarePolitics10$prtclfr<-ESS10$prtclffr;CarePolitics11$prtclfr<-ESS11$prtclgfr

# United Kingdom
CarePolitics1$prtclgb<-ESS1$prtclgb;CarePolitics2$prtclgb<-ESS2$prtclgb;CarePolitics3$prtclgb<-ESS3$prtclagb
CarePolitics4$prtclgb<-ESS4$prtclgb;CarePolitics5$prtclgb<-ESS5$prtclgb;CarePolitics6$prtclgb<-ESS6$prtclgb
CarePolitics7$prtclgb<-ESS7$prtclbgb;CarePolitics8$prtclgb<-ESS8$prtclbgb;CarePolitics9$prtclgb<-ESS9$prtclcgb
CarePolitics10$prtclgb<-ESS10$prtcldgb;CarePolitics11$prtclgb<-ESS11$prtcldgb

# Greece
CarePolitics1$prtclgr<-ESS1$prtclgr;CarePolitics2$prtclgr<-ESS2$prtclagr;CarePolitics3$prtclgr<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclgr<-ESS4$prtclbgr;CarePolitics5$prtclgr<-ESS5$prtclcgr;CarePolitics6$prtclgr<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtclgr<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclgr<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclgr<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtclgr<-ESS10$prtcldgr;CarePolitics11$prtclgr<-ESS11$prtclegr

# Croatia
CarePolitics1$prtclhr<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclhr<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclhr<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclhr<-ESS4$prtclhr;CarePolitics5$prtclhr<-ESS5$prtclhr;CarePolitics6$prtclhr<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtclhr<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclhr<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclhr<-ESS9$prtclahr
CarePolitics10$prtclhr<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclhr<-ESS11$prtclbhr

# Hungary
CarePolitics1$prtclhu<-ESS1$prtclhu;CarePolitics2$prtclhu<-ESS2$prtclhu;CarePolitics3$prtclhu<-ESS3$prtclahu
CarePolitics4$prtclhu<-ESS4$prtclbhu;CarePolitics5$prtclhu<-ESS5$prtclchu;CarePolitics6$prtclhu<-ESS6$prtcldhu
CarePolitics7$prtclhu<-ESS7$prtclehu;CarePolitics8$prtclhu<-ESS8$prtclfhu;CarePolitics9$prtclhu<-ESS9$prtclghu
CarePolitics10$prtclhu<-ESS10$prtclhhu;CarePolitics11$prtclhu<-ESS11$prtclihu

# Ireland
CarePolitics1$prtclie<-ESS1$prtclie;CarePolitics2$prtclie<-ESS2$prtclie;CarePolitics3$prtclie<-ESS3$prtclaie
CarePolitics4$prtclie<-ESS4$prtclbie;CarePolitics5$prtclie<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclie<-ESS6$prtclcie
CarePolitics7$prtclie<-ESS7$prtclcie;CarePolitics8$prtclie<-ESS8$prtcldie;CarePolitics9$prtclie<-ESS9$prtcleie
CarePolitics10$prtclie<-ESS10$prtclfie;CarePolitics11$prtclie<-ESS11$prtclfie

# Iceland
CarePolitics1$prtclis<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclis<-ESS2$prtclis;CarePolitics3$prtclis<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclis<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtclis<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclis<-ESS6$prtclais
CarePolitics7$prtclis<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclis<-ESS8$prtclbis;CarePolitics9$prtclis<-ESS9$prtclcis
CarePolitics10$prtclis<-ESS10$prtcldis;CarePolitics11$prtclis<-ESS11$prtcleis

# Israel
CarePolitics1$prtclil<-ESS1$prtclil;CarePolitics2$prtclil<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclil<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclil<-ESS4$prtclail;CarePolitics5$prtclil<-ESS5$prtclbil;CarePolitics6$prtclil<-ESS6$prtclcil
CarePolitics7$prtclil<-ESS7$prtcldil;CarePolitics8$prtclil<-ESS8$prtcldil;CarePolitics9$prtclil<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtclil<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclil<-ESS11$prtclfil

# Italy
CarePolitics1$prtclit<-ESS1$prtclit;CarePolitics2$prtclit<-ESS2$prtclait;CarePolitics3$prtclit<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclit<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtclit<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclit<-ESS6$prtclbit
CarePolitics7$prtclit<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclit<-ESS8$prtclcit;CarePolitics9$prtclit<-ESS9$prtcldit
CarePolitics10$prtclit<-ESS10$prtcleit;CarePolitics11$prtclit<-ESS11$prtclfit

# Lithuania
CarePolitics1$prtcllt<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtcllt<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtcllt<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtcllt<-ESS4$prtcllt;CarePolitics5$prtcllt<-ESS5$prtcllt;CarePolitics6$prtcllt<-ESS6$prtclalt
CarePolitics7$prtcllt<-ESS7$prtclalt;CarePolitics8$prtcllt<-ESS8$prtclblt;CarePolitics9$prtcllt<-ESS9$prtclblt
CarePolitics10$prtcllt<-ESS10$prtclclt;CarePolitics11$prtcllt<-ESS11$prtclclt

# Latvia
CarePolitics1$prtcllv<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtcllv<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtcllv<-ESS3$prtcllv
CarePolitics4$prtcllv<-ESS4$prtcllv;CarePolitics5$prtcllv<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtcllv<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtcllv<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtcllv<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtcllv<-ESS9$prtclalv
CarePolitics10$prtcllv<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtcllv<-ESS11$prtclblv

# Luxembourg
CarePolitics1$prtcllu<-ESS1$prtcllu;CarePolitics2$prtcllu<-ESS2$prtcllu;CarePolitics3$prtcllu<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtcllu<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtcllu<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtcllu<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtcllu<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtcllu<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtcllu<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtcllu<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtcllu<-rep(NA,nrow(CarePolitics11))

# Montenegro
CarePolitics1$prtclme<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclme<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclme<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclme<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtclme<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclme<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtclme<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclme<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclme<-ESS9$prtclme
CarePolitics10$prtclme<-ESS10$prtclame;CarePolitics11$prtclme<-ESS11$prtclbme

# Netherlands
CarePolitics1$prtclnl<-ESS1$prtclnl;CarePolitics2$prtclnl<-ESS2$prtclanl;CarePolitics3$prtclnl<-ESS3$prtclnl
CarePolitics4$prtclnl<-ESS4$prtclbnl;CarePolitics5$prtclnl<-ESS5$prtclcnl;CarePolitics6$prtclnl<-ESS6$prtcldnl
CarePolitics7$prtclnl<-ESS7$prtclenl;CarePolitics8$prtclnl<-ESS8$prtclenl;CarePolitics9$prtclnl<-ESS9$prtclfnl
CarePolitics10$prtclnl<-ESS10$prtclgnl;CarePolitics11$prtclnl<-ESS11$prtclhnl

# North Macedonia
CarePolitics1$prtclmk<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclmk<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclmk<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclmk<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtclmk<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclmk<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtclmk<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclmk<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclmk<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtclmk<-ESS10$prtclmk;CarePolitics11$prtclmk<-rep(NA,nrow(CarePolitics11))

# Norway
CarePolitics1$prtclno<-ESS1$prtclno;CarePolitics2$prtclno<-ESS2$prtclno;CarePolitics3$prtclno<-ESS3$prtclno
CarePolitics4$prtclno<-ESS4$prtclno;CarePolitics5$prtclno<-ESS5$prtclano;CarePolitics6$prtclno<-ESS6$prtclano
CarePolitics7$prtclno<-ESS7$prtclbno;CarePolitics8$prtclno<-ESS8$prtclbno;CarePolitics9$prtclno<-ESS9$prtclbno
CarePolitics10$prtclno<-ESS10$prtclbno;CarePolitics11$prtclno<-ESS11$prtclcno

# Poland
CarePolitics1$prtclpl<-ESS1$prtclpl;CarePolitics2$prtclpl<-ESS2$prtclapl;CarePolitics3$prtclpl<-ESS3$prtclbpl
CarePolitics4$prtclpl<-ESS4$prtclcpl;CarePolitics5$prtclpl<-ESS5$prtcldpl;CarePolitics6$prtclpl<-ESS6$prtclepl
CarePolitics7$prtclpl<-ESS7$prtclfpl;CarePolitics8$prtclpl<-ESS8$prtclgpl;CarePolitics9$prtclpl<-ESS9$prtclhpl
CarePolitics10$prtclpl<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclpl<-ESS11$prtcljpl

# Portugal
CarePolitics1$prtclpt<-ESS1$prtclpt;CarePolitics2$prtclpt<-ESS2$prtclapt;CarePolitics3$prtclpt<-ESS3$prtclbpt
CarePolitics4$prtclpt<-ESS4$prtclbpt;CarePolitics5$prtclpt<-ESS5$prtclcpt;CarePolitics6$prtclpt<-ESS6$prtclcpt
CarePolitics7$prtclpt<-ESS7$prtcldpt;CarePolitics8$prtclpt<-ESS8$prtclept;CarePolitics9$prtclpt<-ESS9$prtclept
CarePolitics10$prtclpt<-ESS10$prtclfpt;CarePolitics11$prtclpt<-ESS11$prtclgpt

# Romania
CarePolitics1$prtclro<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclro<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclro<-ESS3$prtclro
CarePolitics4$prtclro<-ESS4$prtclaro;CarePolitics5$prtclro<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclro<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtclro<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclro<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclro<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtclro<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclro<-rep(NA,nrow(CarePolitics11))

# Russia
CarePolitics1$prtclru<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclru<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclru<-ESS3$prtclru
CarePolitics4$prtclru<-ESS4$prtclaru;CarePolitics5$prtclru<-ESS5$prtclbru;CarePolitics6$prtclru<-ESS6$prtclcru
CarePolitics7$prtclru<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclru<-ESS8$prtcldru;CarePolitics9$prtclru<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtclru<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclru<-rep(NA,nrow(CarePolitics11))

# Serbia
CarePolitics1$prtclrs<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclrs<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclrs<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclrs<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtclrs<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclrs<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtclrs<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclrs<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclrs<-ESS9$prtclrs
CarePolitics10$prtclrs<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclrs<-ESS11$prtclbrs

# Sweden
CarePolitics1$prtclse<-ESS1$prtclse;CarePolitics2$prtclse<-ESS2$prtclse;CarePolitics3$prtclse<-ESS3$prtclse
CarePolitics4$prtclse<-ESS4$prtclse;CarePolitics5$prtclse<-ESS5$prtclase;CarePolitics6$prtclse<-ESS6$prtclbse
CarePolitics7$prtclse<-ESS7$prtclbse;CarePolitics8$prtclse<-ESS8$prtclbse;CarePolitics9$prtclse<-ESS9$prtclcse
CarePolitics10$prtclse<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclse<-ESS11$prtcldse

# Slovenia
CarePolitics1$prtclsi<-ESS1$prtclsi;CarePolitics2$prtclsi<-ESS2$prtclasi;CarePolitics3$prtclsi<-ESS3$prtclbsi
CarePolitics4$prtclsi<-ESS4$prtclcsi;CarePolitics5$prtclsi<-ESS5$prtclcsi;CarePolitics6$prtclsi<-ESS6$prtcldsi
CarePolitics7$prtclsi<-ESS7$prtclesi;CarePolitics8$prtclsi<-ESS8$prtclesi;CarePolitics9$prtclsi<-ESS9$prtclfsi
CarePolitics10$prtclsi<-ESS10$prtclfsi;CarePolitics11$prtclsi<-ESS11$prtclgsi

# Slovakia
CarePolitics1$prtclsk<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclsk<-ESS2$prtclsk;CarePolitics3$prtclsk<-ESS3$prtclask
CarePolitics4$prtclsk<-ESS4$prtclask;CarePolitics5$prtclsk<-ESS5$prtclbsk;CarePolitics6$prtclsk<-ESS6$prtclcsk
CarePolitics7$prtclsk<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclsk<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclsk<-ESS9$prtcldsk
CarePolitics10$prtclsk<-ESS10$prtclesk;CarePolitics11$prtclsk<-ESS11$prtclesk

# Turkey
CarePolitics1$prtcltr<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtcltr<-ESS2$prtcltr;CarePolitics3$prtcltr<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtcltr<-ESS4$prtclatr;CarePolitics5$prtcltr<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtcltr<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtcltr<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtcltr<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtcltr<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtcltr<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtcltr<-rep(NA,nrow(CarePolitics11))

# Ukraine
CarePolitics1$prtclua<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclua<-ESS2$prtclua;CarePolitics3$prtclua<-ESS3$prtclaua
CarePolitics4$prtclua<-ESS4$prtclbua;CarePolitics5$prtclua<-ESS5$prtclcua;CarePolitics6$prtclua<-ESS6$prtcldua
CarePolitics7$prtclua<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclua<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclua<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtclua<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclua<-rep(NA,nrow(CarePolitics11))

# Kosovo
CarePolitics1$prtclxk<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtclxk<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtclxk<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtclxk<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtclxk<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtclxk<-ESS6$prtclxk
CarePolitics7$prtclxk<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtclxk<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtclxk<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtclxk<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtclxk<-rep(NA,nrow(CarePolitics11))

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

# How close to this party? (prtdgcl)
CarePolitics1$prtdgcl<-ESS1$prtdgcl;CarePolitics2$prtdgcl<-ESS2$prtdgcl;CarePolitics3$prtdgcl<-ESS3$prtdgcl
CarePolitics4$prtdgcl<-ESS4$prtdgcl;CarePolitics5$prtdgcl<-ESS5$prtdgcl;CarePolitics6$prtdgcl<-ESS6$prtdgcl
CarePolitics7$prtdgcl<-ESS7$prtdgcl;CarePolitics8$prtdgcl<-ESS8$prtdgcl;CarePolitics9$prtdgcl<-ESS9$prtdgcl
CarePolitics10$prtdgcl<-ESS10$prtdgcl;CarePolitics11$prtdgcl<-ESS11$prtdgcl

# Placement in left-right scale (lrscale)
CarePolitics1$lrscale<-ESS1$lrscale;CarePolitics2$lrscale<-ESS2$lrscale;CarePolitics3$lrscale<-ESS3$lrscale
CarePolitics4$lrscale<-ESS4$lrscale;CarePolitics5$lrscale<-ESS5$lrscale;CarePolitics6$lrscale<-ESS6$lrscale
CarePolitics7$lrscale<-ESS7$lrscale;CarePolitics8$lrscale<-ESS8$lrscale;CarePolitics9$lrscale<-ESS9$lrscale
CarePolitics10$lrscale<-ESS10$lrscale;CarePolitics11$lrscale<-ESS11$lrscale

# Satisfaction items (stfeco, stfgov, stfdem)
CarePolitics1$stfeco<-ESS1$stfeco;CarePolitics2$stfeco<-ESS2$stfeco;CarePolitics3$stfeco<-ESS3$stfeco
CarePolitics4$stfeco<-ESS4$stfeco;CarePolitics5$stfeco<-ESS5$stfeco;CarePolitics6$stfeco<-ESS6$stfeco
CarePolitics7$stfeco<-ESS7$stfeco;CarePolitics8$stfeco<-ESS8$stfeco;CarePolitics9$stfeco<-ESS9$stfeco
CarePolitics10$stfeco<-ESS10$stfeco;CarePolitics11$stfeco<-ESS11$stfeco

CarePolitics1$stfdem<-ESS1$stfdem;CarePolitics2$stfdem<-ESS2$stfdem;CarePolitics3$stfdem<-ESS3$stfdem
CarePolitics4$stfdem<-ESS4$stfdem;CarePolitics5$stfdem<-ESS5$stfdem;CarePolitics6$stfdem<-ESS6$stfdem
CarePolitics7$stfdem<-ESS7$stfdem;CarePolitics8$stfdem<-ESS8$stfdem;CarePolitics9$stfdem<-ESS9$stfdem
CarePolitics10$stfdem<-ESS10$stfdem;CarePolitics11$stfdem<-ESS11$stfdem

CarePolitics1$stfgov<-ESS1$stfgov;CarePolitics2$stfgov<-ESS2$stfgov;CarePolitics3$stfgov<-ESS3$stfgov
CarePolitics4$stfgov<-ESS4$stfgov;CarePolitics5$stfgov<-ESS5$stfgov;CarePolitics6$stfgov<-ESS6$stfgov
CarePolitics7$stfgov<-ESS7$stfgov;CarePolitics8$stfgov<-ESS8$stfgov;CarePolitics9$stfgov<-ESS9$stfgov
CarePolitics10$stfgov<-ESS10$stfgov;CarePolitics11$stfgov<-ESS11$stfgov

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

# State of health services (stfhlth)
CarePolitics1$stfhlth<-ESS1$stfhlth;CarePolitics2$stfhlth<-ESS2$stfhlth;CarePolitics3$stfhlth<-ESS3$stfhlth
CarePolitics4$stfhlth<-ESS4$stfhlth;CarePolitics5$stfhlth<-ESS5$stfhlth;CarePolitics6$stfhlth<-ESS6$stfhlth
CarePolitics7$stfhlth<-ESS7$stfhlth;CarePolitics8$stfhlth<-ESS8$stfhlth;CarePolitics9$stfhlth<-ESS9$stfhlth
CarePolitics10$stfhlth<-ESS10$stfhlth;CarePolitics11$stfhlth<-ESS11$stfhlth

# Preferences for redistribution (gincdif)
CarePolitics1$gincdif<-ESS1$gincdif;CarePolitics2$gincdif<-ESS2$gincdif;CarePolitics3$gincdif<-ESS3$gincdif
CarePolitics4$gincdif<-ESS4$gincdif;CarePolitics5$gincdif<-ESS5$gincdif;CarePolitics6$gincdif<-ESS6$gincdif
CarePolitics7$gincdif<-ESS7$gincdif;CarePolitics8$gincdif<-ESS8$gincdif;CarePolitics9$gincdif<-ESS9$gincdif
CarePolitics10$gincdif<-ESS10$gincdif;CarePolitics11$gincdif<-ESS11$gincdif

# Migration attitudes items (imbgeco)
CarePolitics1$imbgeco<-ESS1$imbgeco;CarePolitics2$imbgeco<-ESS2$imbgeco;CarePolitics3$imbgeco<-ESS3$imbgeco
CarePolitics4$imbgeco<-ESS4$imbgeco;CarePolitics5$imbgeco<-ESS5$imbgeco;CarePolitics6$imbgeco<-ESS6$imbgeco
CarePolitics7$imbgeco<-ESS7$imbgeco;CarePolitics8$imbgeco<-ESS8$imbgeco;CarePolitics9$imbgeco<-ESS9$imbgeco
CarePolitics10$imbgeco<-ESS10$imbgeco;CarePolitics11$imbgeco<-ESS11$imbgeco

# Gender and work attitudes (mnrgtjb, trwkcnt, eqpaybg, freinsw, fineqpy)
CarePolitics1$mnrgtjb<-rep(NA,nrow(CarePolitics1));CarePolitics2$mnrgtjb<-ESS2$mnrgtjb;CarePolitics3$mnrgtjb<-rep(NA,nrow(CarePolitics3))
CarePolitics4$mnrgtjb<-ESS4$mnrgtjb;CarePolitics5$mnrgtjb<-ESS5$mnrgtjb;CarePolitics6$mnrgtjb<-rep(NA,nrow(CarePolitics6))
CarePolitics7$mnrgtjb<-rep(NA,nrow(CarePolitics7));CarePolitics8$mnrgtjb<-ESS8$mnrgtjb;CarePolitics9$mnrgtjb<-rep(NA,nrow(CarePolitics9))
CarePolitics10$mnrgtjb<-rep(NA,nrow(CarePolitics10));CarePolitics11$mnrgtjb<-rep(NA,nrow(CarePolitics11))

CarePolitics1$trwkcnt<-rep(NA,nrow(CarePolitics1));CarePolitics2$trwkcnt<-rep(NA,nrow(CarePolitics2));CarePolitics3$trwkcnt<-rep(NA,nrow(CarePolitics3))
CarePolitics4$trwkcnt<-rep(NA,nrow(CarePolitics4));CarePolitics5$trwkcnt<-rep(NA,nrow(CarePolitics5));CarePolitics6$trwkcnt<-rep(NA,nrow(CarePolitics6))
CarePolitics7$trwkcnt<-rep(NA,nrow(CarePolitics7));CarePolitics8$trwkcnt<-rep(NA,nrow(CarePolitics8));CarePolitics9$trwkcnt<-rep(NA,nrow(CarePolitics9))
CarePolitics10$trwkcnt<-rep(NA,nrow(CarePolitics10));CarePolitics11$trwkcnt<-ESS11$trwkcnt

CarePolitics1$eqpaybg<-rep(NA,nrow(CarePolitics1));CarePolitics2$eqpaybg<-rep(NA,nrow(CarePolitics2));CarePolitics3$eqpaybg<-rep(NA,nrow(CarePolitics3))
CarePolitics4$eqpaybg<-rep(NA,nrow(CarePolitics4));CarePolitics5$eqpaybg<-rep(NA,nrow(CarePolitics5));CarePolitics6$eqpaybg<-rep(NA,nrow(CarePolitics6))
CarePolitics7$eqpaybg<-rep(NA,nrow(CarePolitics7));CarePolitics8$eqpaybg<-rep(NA,nrow(CarePolitics8));CarePolitics9$eqpaybg<-rep(NA,nrow(CarePolitics9))
CarePolitics10$eqpaybg<-rep(NA,nrow(CarePolitics10));CarePolitics11$eqpaybg<-ESS11$eqpaybg

CarePolitics1$freinsw<-rep(NA,nrow(CarePolitics1));CarePolitics2$freinsw<-rep(NA,nrow(CarePolitics2));CarePolitics3$freinsw<-rep(NA,nrow(CarePolitics3))
CarePolitics4$freinsw<-rep(NA,nrow(CarePolitics4));CarePolitics5$freinsw<-rep(NA,nrow(CarePolitics5));CarePolitics6$freinsw<-rep(NA,nrow(CarePolitics6))
CarePolitics7$freinsw<-rep(NA,nrow(CarePolitics7));CarePolitics8$freinsw<-rep(NA,nrow(CarePolitics8));CarePolitics9$freinsw<-rep(NA,nrow(CarePolitics9))
CarePolitics10$freinsw<-rep(NA,nrow(CarePolitics10));CarePolitics11$freinsw<-ESS11$freinsw

CarePolitics1$fineqpy<-rep(NA,nrow(CarePolitics1));CarePolitics2$fineqpy<-rep(NA,nrow(CarePolitics2));CarePolitics3$fineqpy<-rep(NA,nrow(CarePolitics3))
CarePolitics4$fineqpy<-rep(NA,nrow(CarePolitics4));CarePolitics5$fineqpy<-rep(NA,nrow(CarePolitics5));CarePolitics6$fineqpy<-rep(NA,nrow(CarePolitics6))
CarePolitics7$fineqpy<-rep(NA,nrow(CarePolitics7));CarePolitics8$fineqpy<-rep(NA,nrow(CarePolitics8));CarePolitics9$fineqpy<-rep(NA,nrow(CarePolitics9))
CarePolitics10$fineqpy<-rep(NA,nrow(CarePolitics10));CarePolitics11$fineqpy<-ESS11$fineqpy

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

## Political activity
# Voted in last election (vote)
CarePolitics1$vote<-ESS1$vote;CarePolitics2$vote<-ESS2$vote;CarePolitics3$vote<-ESS3$vote;CarePolitics4$vote<-ESS4$vote
CarePolitics5$vote<-ESS5$vote;CarePolitics6$vote<-ESS6$vote;CarePolitics7$vote<-ESS7$vote;CarePolitics8$vote<-ESS8$vote
CarePolitics9$vote<-ESS9$vote;CarePolitics10$vote<-ESS10$vote;CarePolitics11$vote<-ESS11$vote

# For whom (country specific variable, prtv__)
# Albania
CarePolitics1$prtvtal<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtal<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtal<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtal<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtvtal<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtal<-ESS6$prtvtal
CarePolitics7$prtvtal<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtal<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtal<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtal<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtal<-rep(NA,nrow(CarePolitics11))

# Austria
CarePolitics1$prtvtat<-ESS1$prtvtat;CarePolitics2$prtvtat<-ESS2$prtvtat;CarePolitics3$prtvtat<-ESS3$prtvtaat
CarePolitics4$prtvtat<-ESS4$prtvtaat;CarePolitics5$prtvtat<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtat<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvtat<-ESS7$prtvtbat;CarePolitics8$prtvtat<-ESS8$prtvtbat;CarePolitics9$prtvtat<-ESS9$prtvtcat
CarePolitics10$prtvtat<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtat<-ESS11$prtvtdat

# Belgium
CarePolitics1$prtvtbe<-ESS1$prtvtbe;CarePolitics2$prtvtbe<-ESS2$prtvtabe;CarePolitics3$prtvtbe<-ESS3$prtvtabe
CarePolitics4$prtvtbe<-ESS4$prtvtbbe;CarePolitics5$prtvtbe<-ESS5$prtvtcbe;CarePolitics6$prtvtbe<-ESS6$prtvtcbe
CarePolitics7$prtvtbe<-ESS7$prtvtcbe;CarePolitics8$prtvtbe<-ESS8$prtvtcbe;CarePolitics9$prtvtbe<-ESS9$prtvtdbe
CarePolitics10$prtvtbe<-ESS10$prtvtebe;CarePolitics11$prtvtbe<-ESS11$prtvtebe

# Bulgaria
CarePolitics1$prtvtbg<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtbg<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtbg<-ESS3$prtvtbg
CarePolitics4$prtvtbg<-ESS4$prtvtabg;CarePolitics5$prtvtbg<-ESS5$prtvtbbg;CarePolitics6$prtvtbg<-ESS6$prtvtcbg
CarePolitics7$prtvtbg<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtbg<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtbg<-ESS9$prtvtdbg
CarePolitics10$prtvtbg<-ESS10$prtvtebg;CarePolitics11$prtvtbg<-ESS11$prtvtfbg

# Switzerland
CarePolitics1$prtvtch<-ESS1$prtvtch;CarePolitics2$prtvtch<-ESS2$prtvtch;CarePolitics3$prtvtch<-ESS3$prtvtach
CarePolitics4$prtvtch<-ESS4$prtvtbch;CarePolitics5$prtvtch<-ESS5$prtvtcch;CarePolitics6$prtvtch<-ESS6$prtvtdch
CarePolitics7$prtvtch<-ESS7$prtvtech;CarePolitics8$prtvtch<-ESS8$prtvtfch;CarePolitics9$prtvtch<-ESS9$prtvtgch
CarePolitics10$prtvtch<-ESS10$prtvthch;CarePolitics11$prtvtch<-ESS11$prtvthch

# Cyprus
CarePolitics1$prtvtcy<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtcy<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtcy<-ESS3$prtvtcy
CarePolitics4$prtvtcy<-ESS4$prtvtcy;CarePolitics5$prtvtcy<-ESS5$prtvtcy;CarePolitics6$prtvtcy<-ESS6$prtvtacy
CarePolitics7$prtvtcy<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtcy<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtcy<-ESS9$prtvtbcy
CarePolitics10$prtvtcy<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtcy<-ESS11$prtvtccy

# Czech Republic
CarePolitics1$prtvtcz<-ESS1$prtvtcz;CarePolitics2$prtvtcz<-ESS2$prtvtcz;CarePolitics3$prtvtcz<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtcz<-ESS4$prtvtacz;CarePolitics5$prtvtcz<-ESS5$prtvtbcz;CarePolitics6$prtvtcz<-ESS6$prtvtccz
CarePolitics7$prtvtcz<-ESS7$prtvtdcz;CarePolitics8$prtvtcz<-ESS8$prtvtdcz;CarePolitics9$prtvtcz<-ESS9$prtvtecz
CarePolitics10$prtvtcz<-ESS10$prtvtecz;CarePolitics11$prtvtcz<-rep(NA,nrow(CarePolitics11))

# Germany
CarePolitics1$prtvtde1<-ESS1$prtvde1;CarePolitics2$prtvtde1<-ESS2$prtvade1;CarePolitics3$prtvtde1<-ESS3$prtvbde1
CarePolitics4$prtvtde1<-ESS4$prtvbde1;CarePolitics5$prtvtde1<-ESS5$prtvcde1;CarePolitics6$prtvtde1<-ESS6$prtvdde1
CarePolitics7$prtvtde1<-ESS7$prtvede1;CarePolitics8$prtvtde1<-ESS8$prtvede1;CarePolitics9$prtvtde1<-ESS9$prtvede1
CarePolitics10$prtvtde1<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtde1<-ESS11$prtvgde1

CarePolitics1$prtvtde2<-ESS1$prtvde2;CarePolitics2$prtvtde2<-ESS2$prtvade2;CarePolitics3$prtvtde2<-ESS3$prtvbde2
CarePolitics4$prtvtde2<-ESS4$prtvbde2;CarePolitics5$prtvtde2<-ESS5$prtvcde2;CarePolitics6$prtvtde2<-ESS6$prtvdde2
CarePolitics7$prtvtde2<-ESS7$prtvede2;CarePolitics8$prtvtde2<-ESS8$prtvede2;CarePolitics9$prtvtde2<-ESS9$prtvede2
CarePolitics10$prtvtde2<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtde2<-ESS11$prtvgde2

# Denmark
CarePolitics1$prtvtdk<-ESS1$prtvtdk;CarePolitics2$prtvtdk<-ESS2$prtvtdk;CarePolitics3$prtvtdk<-ESS3$prtvtadk
CarePolitics4$prtvtdk<-ESS4$prtvtbdk;CarePolitics5$prtvtdk<-ESS5$prtvtbdk;CarePolitics6$prtvtdk<-ESS6$prtvtcdk
CarePolitics7$prtvtdk<-ESS7$prtvtcdk;CarePolitics8$prtvtdk<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtdk<-ESS9$prtvtddk
CarePolitics10$prtvtdk<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtdk<-rep(NA,nrow(CarePolitics11))

# Estonia
CarePolitics1$prtvtee<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtee<-ESS2$prtvtee;CarePolitics3$prtvtee<-ESS3$prtvtaee
CarePolitics4$prtvtee<-ESS4$prtvtbee;CarePolitics5$prtvtee<-ESS5$prtvtcee;CarePolitics6$prtvtee<-ESS6$prtvtdee
CarePolitics7$prtvtee<-ESS7$prtvteee;CarePolitics8$prtvtee<-ESS8$prtvtfee;CarePolitics9$prtvtee<-ESS9$prtvtgee
CarePolitics10$prtvtee<-ESS10$prtvthee;CarePolitics11$prtvtee<-rep(NA,nrow(CarePolitics11))

# Spain
CarePolitics1$prtvtes<-ESS1$prtvtes;CarePolitics2$prtvtes<-ESS2$prtvtaes;CarePolitics3$prtvtes<-ESS3$prtvtaes
CarePolitics4$prtvtes<-ESS4$prtvtbes;CarePolitics5$prtvtes<-ESS5$prtvtbes;CarePolitics6$prtvtes<-ESS6$prtvtces
CarePolitics7$prtvtes<-ESS7$prtvtces;CarePolitics8$prtvtes<-ESS8$prtvtdes;CarePolitics9$prtvtes<-ESS9$prtvtees
CarePolitics10$prtvtes<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtes<-ESS11$prtvtges

# Finland
CarePolitics1$prtvtfi<-ESS1$prtvtfi;CarePolitics2$prtvtfi<-ESS2$prtvtfi;CarePolitics3$prtvtfi<-ESS3$prtvtfi
CarePolitics4$prtvtfi<-ESS4$prtvtafi;CarePolitics5$prtvtfi<-ESS5$prtvtbfi;CarePolitics6$prtvtfi<-ESS6$prtvtcfi
CarePolitics7$prtvtfi<-ESS7$prtvtcfi;CarePolitics8$prtvtfi<-ESS8$prtvtdfi;CarePolitics9$prtvtfi<-ESS9$prtvtdfi
CarePolitics10$prtvtfi<-ESS10$prtvtefi;CarePolitics11$prtvtfi<-ESS11$prtvtffi

# France
CarePolitics1$prtvtfr<-ESS1$prtvtfr;CarePolitics2$prtvtfr<-ESS2$prtvtfr;CarePolitics3$prtvtfr<-ESS3$prtvtafr
CarePolitics4$prtvtfr<-ESS4$prtvtbfr;CarePolitics5$prtvtfr<-ESS5$prtvtbfr;CarePolitics6$prtvtfr<-ESS6$prtvtcfr
CarePolitics7$prtvtfr<-ESS7$prtvtcfr;CarePolitics8$prtvtfr<-ESS8$prtvtcfr;CarePolitics9$prtvtfr<-ESS9$prtvtdfr
CarePolitics10$prtvtfr<-ESS10$prtvtefr;CarePolitics11$prtvtfr<-ESS11$prtvtffr

# United Kingdom
CarePolitics1$prtvtgb<-ESS1$prtvtgb;CarePolitics2$prtvtgb<-ESS2$prtvtgb;CarePolitics3$prtvtgb<-ESS3$prtvtagb
CarePolitics4$prtvtgb<-ESS4$prtvtgb;CarePolitics5$prtvtgb<-ESS5$prtvtgb;CarePolitics6$prtvtgb<-ESS6$prtvtgb
CarePolitics7$prtvtgb<-ESS7$prtvtbgb;CarePolitics8$prtvtgb<-ESS8$prtvtbgb;CarePolitics9$prtvtgb<-ESS9$prtvtcgb
CarePolitics10$prtvtgb<-ESS10$prtvtdgb;CarePolitics11$prtvtgb<-ESS11$prtvtdgb

# Greece
CarePolitics1$prtvtgr<-ESS1$prtvtgr;CarePolitics2$prtvtgr<-ESS2$prtvtagr;CarePolitics3$prtvtgr<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtgr<-ESS4$prtvtbgr;CarePolitics5$prtvtgr<-ESS5$prtvtcgr;CarePolitics6$prtvtgr<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvtgr<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtgr<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtgr<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtgr<-ESS10$prtvtdgr;CarePolitics11$prtvtgr<-ESS11$prtvtegr

# Croatia
CarePolitics1$prtvthr<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvthr<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvthr<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvthr<-ESS4$prtvthr;CarePolitics5$prtvthr<-ESS5$prtvthr;CarePolitics6$prtvthr<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvthr<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvthr<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvthr<-ESS9$prtvtahr
CarePolitics10$prtvthr<-ESS10$prtvtbhr;CarePolitics11$prtvthr<-ESS11$prtvtchr

# Hungary
CarePolitics1$prtvthu<-ESS1$prtvthu;CarePolitics2$prtvthu<-ESS2$prtvthu;CarePolitics3$prtvthu<-ESS3$prtvtahu
CarePolitics4$prtvthu<-ESS4$prtvtbhu;CarePolitics5$prtvthu<-ESS5$prtvtchu;CarePolitics6$prtvthu<-ESS6$prtvtdhu
CarePolitics7$prtvthu<-ESS7$prtvtehu;CarePolitics8$prtvthu<-ESS8$prtvtehu;CarePolitics9$prtvthu<-ESS9$prtvtfhu
CarePolitics10$prtvthu<-ESS10$prtvtghu;CarePolitics11$prtvthu<-ESS11$prtvthhu

# Ireland
CarePolitics1$prtvtie<-ESS1$prtvtie;CarePolitics2$prtvtie<-ESS2$prtvtie;CarePolitics3$prtvtie<-ESS3$prtvtie
CarePolitics4$prtvtie<-ESS4$prtvtie;CarePolitics5$prtvtie<-ESS5$prtvtaie;CarePolitics6$prtvtie<-ESS6$prtvtaie
CarePolitics7$prtvtie<-ESS7$prtvtaie;CarePolitics8$prtvtie<-ESS8$prtvtbie;CarePolitics9$prtvtie<-ESS9$prtvtcie
CarePolitics10$prtvtie<-ESS10$prtvtdie;CarePolitics11$prtvtie<-ESS11$prtvteie

# Iceland
CarePolitics1$prtvtis<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtis<-ESS2$prtvtis;CarePolitics3$prtvtis<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtis<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtvtis<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtis<-ESS6$prtvtais
CarePolitics7$prtvtis<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtis<-ESS8$prtvtbis;CarePolitics9$prtvtis<-ESS9$prtvtcis
CarePolitics10$prtvtis<-ESS10$prtvtdis;CarePolitics11$prtvtis<-ESS11$prtvteis

# Israel
CarePolitics1$prtvtil<-ESS1$prtvtil;CarePolitics2$prtvtil<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtil<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtil<-ESS4$prtvtail;CarePolitics5$prtvtil<-ESS5$prtvtbil;CarePolitics6$prtvtil<-ESS6$prtvtbil
CarePolitics7$prtvtil<-ESS7$prtvtcil;CarePolitics8$prtvtil<-ESS8$prtvtcil;CarePolitics9$prtvtil<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtil<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtil<-ESS11$prtvteil

# Italy
CarePolitics1$prtvtit<-ESS1$prtvtit;CarePolitics2$prtvtit<-ESS2$prtvtait;CarePolitics3$prtvtit<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtit<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtvtit<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtit<-ESS6$prtvtbit
CarePolitics7$prtvtit<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtit<-ESS8$prtvtbit;CarePolitics9$prtvtit<-ESS9$prtvtcit
CarePolitics10$prtvtit<-ESS10$prtvtdit;CarePolitics11$prtvtit<-ESS11$prtvteit

# Lithuania
CarePolitics1$prtvtlt1<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtlt1<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtlt1<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtlt1<-ESS4$prtvlt1;CarePolitics5$prtvtlt1<-ESS5$prtvlt1;CarePolitics6$prtvtlt1<-ESS6$prtvalt1
CarePolitics7$prtvtlt1<-ESS7$prtvalt1;CarePolitics8$prtvtlt1<-ESS8$prtvblt1;CarePolitics9$prtvtlt1<-ESS9$prtvblt1
CarePolitics10$prtvtlt1<-ESS10$prtvclt1;CarePolitics11$prtvtlt1<-ESS11$prtvclt1

CarePolitics1$prtvtlt2<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtlt2<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtlt2<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtlt2<-ESS4$prtvlt2;CarePolitics5$prtvtlt2<-ESS5$prtvlt2;CarePolitics6$prtvtlt2<-ESS6$prtvalt2
CarePolitics7$prtvtlt2<-ESS7$prtvalt2;CarePolitics8$prtvtlt2<-ESS8$prtvblt2;CarePolitics9$prtvtlt2<-ESS9$prtvblt2
CarePolitics10$prtvtlt2<-ESS10$prtvclt2;CarePolitics11$prtvtlt2<-ESS11$prtvclt2

CarePolitics1$prtvtlt3<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtlt3<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtlt3<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtlt3<-ESS4$prtvlt3;CarePolitics5$prtvtlt3<-ESS5$prtvlt3;CarePolitics6$prtvtlt3<-ESS6$prtvalt3
CarePolitics7$prtvtlt3<-ESS7$prtvalt3;CarePolitics8$prtvtlt3<-ESS8$prtvblt3;CarePolitics9$prtvtlt3<-ESS9$prtvblt3
CarePolitics10$prtvtlt3<-ESS10$prtvclt3;CarePolitics11$prtvtlt3<-ESS11$prtvclt3

# Latvia
CarePolitics1$prtvtlv<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtlv<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtlv<-ESS3$prtvtlv
CarePolitics4$prtvtlv<-ESS4$prtvtlv;CarePolitics5$prtvtlv<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtlv<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvtlv<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtlv<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtlv<-ESS9$prtvtalv
CarePolitics10$prtvtlv<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtlv<-ESS11$prtvtblv

# Luxembourg
CarePolitics1$prtvtlu<-ESS1$prtvtlu;CarePolitics2$prtvtlu<-ESS2$prtvtlu;CarePolitics3$prtvtlu<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtlu<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtvtlu<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtlu<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvtlu<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtlu<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtlu<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtlu<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtlu<-rep(NA,nrow(CarePolitics11))

# Montenegro
CarePolitics1$prtvtme<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtme<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtme<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtme<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtvtme<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtme<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvtme<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtme<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtme<-ESS9$prtvtme
CarePolitics10$prtvtme<-ESS10$prtvtame;CarePolitics11$prtvtme<-ESS11$prtvtbme

# Netherlands
CarePolitics1$prtvtnl<-ESS1$prtvtnl;CarePolitics2$prtvtnl<-ESS2$prtvtanl;CarePolitics3$prtvtnl<-ESS3$prtvtbnl
CarePolitics4$prtvtnl<-ESS4$prtvtcnl;CarePolitics5$prtvtnl<-ESS5$prtvtdnl;CarePolitics6$prtvtnl<-ESS6$prtvtenl
CarePolitics7$prtvtnl<-ESS7$prtvtfnl;CarePolitics8$prtvtnl<-ESS8$prtvtfnl;CarePolitics9$prtvtnl<-ESS9$prtvtgnl
CarePolitics10$prtvtnl<-ESS10$prtvthnl;CarePolitics11$prtvtnl<-ESS11$prtvtinl

# North Macedonia
CarePolitics1$prtvtmk<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtmk<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtmk<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtmk<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtvtmk<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtmk<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvtmk<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtmk<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtmk<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtmk<-ESS10$prtvtmk;CarePolitics11$prtvtmk<-rep(NA,nrow(CarePolitics11))

# Norway
CarePolitics1$prtvtno<-ESS1$prtvtno;CarePolitics2$prtvtno<-ESS2$prtvtno;CarePolitics3$prtvtno<-ESS3$prtvtno
CarePolitics4$prtvtno<-ESS4$prtvtno;CarePolitics5$prtvtno<-ESS5$prtvtano;CarePolitics6$prtvtno<-ESS6$prtvtano
CarePolitics7$prtvtno<-ESS7$prtvtbno;CarePolitics8$prtvtno<-ESS8$prtvtbno;CarePolitics9$prtvtno<-ESS9$prtvtbno
CarePolitics10$prtvtno<-ESS10$prtvtbno;CarePolitics11$prtvtno<-ESS11$prtvtcno

# Poland
CarePolitics1$prtvtpl<-ESS1$prtvtpl;CarePolitics2$prtvtpl<-ESS2$prtvtpl;CarePolitics3$prtvtpl<-ESS3$prtvtapl
CarePolitics4$prtvtpl<-ESS4$prtvtbpl;CarePolitics5$prtvtpl<-ESS5$prtvtbpl;CarePolitics6$prtvtpl<-ESS6$prtvtcpl
CarePolitics7$prtvtpl<-ESS7$prtvtcpl;CarePolitics8$prtvtpl<-ESS8$prtvtdpl;CarePolitics9$prtvtpl<-ESS9$prtvtdpl
CarePolitics10$prtvtpl<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtpl<-ESS11$prtvtfpl

# Portugal
CarePolitics1$prtvtpt<-ESS1$prtvtpt;CarePolitics2$prtvtpt<-ESS2$prtvtpt;CarePolitics3$prtvtpt<-ESS3$prtvtapt
CarePolitics4$prtvtpt<-ESS4$prtvtapt;CarePolitics5$prtvtpt<-ESS5$prtvtbpt;CarePolitics6$prtvtpt<-ESS6$prtvtbpt
CarePolitics7$prtvtpt<-ESS7$prtvtbpt;CarePolitics8$prtvtpt<-ESS8$prtvtcpt;CarePolitics9$prtvtpt<-ESS9$prtvtcpt
CarePolitics10$prtvtpt<-ESS10$prtvtdpt;CarePolitics11$prtvtpt<-ESS11$prtvtept

# Romania
CarePolitics1$prtvtro<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtro<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtro<-ESS3$prtvtro
CarePolitics4$prtvtro<-ESS4$prtvtaro;CarePolitics5$prtvtro<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtro<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvtro<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtro<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtro<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtro<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtro<-rep(NA,nrow(CarePolitics11))

# Russia
CarePolitics1$prtvtru<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtru<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtru<-ESS3$prtvtru
CarePolitics4$prtvtru<-ESS4$prtvtaru;CarePolitics5$prtvtru<-ESS5$prtvtbru;CarePolitics6$prtvtru<-ESS6$prtvtcru
CarePolitics7$prtvtru<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtru<-ESS8$prtvtdru;CarePolitics9$prtvtru<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtru<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtru<-rep(NA,nrow(CarePolitics11))

# Serbia
CarePolitics1$prtvtrs<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtrs<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtrs<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtrs<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtvtrs<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtrs<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvtrs<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtrs<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtrs<-ESS9$prtvtrs
CarePolitics10$prtvtrs<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtrs<-ESS11$prtvtbrs

# Sweden
CarePolitics1$prtvtse<-ESS1$prtvtse;CarePolitics2$prtvtse<-ESS2$prtvtse;CarePolitics3$prtvtse<-ESS3$prtvtse
CarePolitics4$prtvtse<-ESS4$prtvtse;CarePolitics5$prtvtse<-ESS5$prtvtase;CarePolitics6$prtvtse<-ESS6$prtvtbse
CarePolitics7$prtvtse<-ESS7$prtvtbse;CarePolitics8$prtvtse<-ESS8$prtvtbse;CarePolitics9$prtvtse<-ESS9$prtvtcse
CarePolitics10$prtvtse<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtse<-ESS11$prtvtdse

# Slovenia
CarePolitics1$prtvtsi<-ESS1$prtvtsi;CarePolitics2$prtvtsi<-ESS2$prtvtasi;CarePolitics3$prtvtsi<-ESS3$prtvtbsi
CarePolitics4$prtvtsi<-ESS4$prtvtcsi;CarePolitics5$prtvtsi<-ESS5$prtvtcsi;CarePolitics6$prtvtsi<-ESS6$prtvtdsi
CarePolitics7$prtvtsi<-ESS7$prtvtesi;CarePolitics8$prtvtsi<-ESS8$prtvtesi;CarePolitics9$prtvtsi<-ESS9$prtvtfsi
CarePolitics10$prtvtsi<-ESS10$prtvtfsi;CarePolitics11$prtvtsi<-ESS11$prtvtgsi

# Slovakia
CarePolitics1$prtvtsk<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtsk<-ESS2$prtvtsk;CarePolitics3$prtvtsk<-ESS3$prtvtask
CarePolitics4$prtvtsk<-ESS4$prtvtask;CarePolitics5$prtvtsk<-ESS5$prtvtbsk;CarePolitics6$prtvtsk<-ESS6$prtvtcsk
CarePolitics7$prtvtsk<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtsk<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtsk<-ESS9$prtvtdsk
CarePolitics10$prtvtsk<-ESS10$prtvtesk;CarePolitics11$prtvtsk<-ESS11$prtvtesk

# Turkey
CarePolitics1$prtvttr<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvttr<-ESS2$prtvttr;CarePolitics3$prtvttr<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvttr<-ESS4$prtvtatr;CarePolitics5$prtvttr<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvttr<-rep(NA,nrow(CarePolitics6))
CarePolitics7$prtvttr<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvttr<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvttr<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvttr<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvttr<-rep(NA,nrow(CarePolitics11))

# Ukraine
CarePolitics1$prtvtua<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtua<-ESS2$prtvtua;CarePolitics3$prtvtua<-ESS3$prtvtaua
CarePolitics4$prtvtua<-ESS4$prtvtbua;CarePolitics5$prtvtua<-ESS5$prtvtbua;CarePolitics6$prtvtua<-ESS6$prtvtcua
CarePolitics7$prtvtua<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtua<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtua<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtua<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtua<-rep(NA,nrow(CarePolitics11))

# Kosovo
CarePolitics1$prtvtxk<-rep(NA,nrow(CarePolitics1));CarePolitics2$prtvtxk<-rep(NA,nrow(CarePolitics2));CarePolitics3$prtvtxk<-rep(NA,nrow(CarePolitics3))
CarePolitics4$prtvtxk<-rep(NA,nrow(CarePolitics4));CarePolitics5$prtvtxk<-rep(NA,nrow(CarePolitics5));CarePolitics6$prtvtxk<-ESS6$prtvtxk
CarePolitics7$prtvtxk<-rep(NA,nrow(CarePolitics7));CarePolitics8$prtvtxk<-rep(NA,nrow(CarePolitics8));CarePolitics9$prtvtxk<-rep(NA,nrow(CarePolitics9))
CarePolitics10$prtvtxk<-rep(NA,nrow(CarePolitics10));CarePolitics11$prtvtxk<-rep(NA,nrow(CarePolitics11))

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

# Political mobilisation items (contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, pstplonl)
CarePolitics1$contplt<-ESS1$contplt;CarePolitics2$contplt<-ESS2$contplt;CarePolitics3$contplt<-ESS3$contplt
CarePolitics4$contplt<-ESS4$contplt;CarePolitics5$contplt<-ESS5$contplt;CarePolitics6$contplt<-ESS6$contplt
CarePolitics7$contplt<-ESS7$contplt;CarePolitics8$contplt<-ESS8$contplt;CarePolitics9$contplt<-ESS9$contplt
CarePolitics10$contplt<-ESS10$contplt;CarePolitics11$contplt<-ESS11$contplt

CarePolitics1$wrkprty<-ESS1$wrkprty;CarePolitics2$wrkprty<-ESS2$wrkprty;CarePolitics3$wrkprty<-ESS3$wrkprty
CarePolitics4$wrkprty<-ESS4$wrkprty;CarePolitics5$wrkprty<-ESS5$wrkprty;CarePolitics6$wrkprty<-ESS6$wrkprty
CarePolitics7$wrkprty<-ESS7$wrkprty;CarePolitics8$wrkprty<-ESS8$wrkprty;CarePolitics9$wrkprty<-ESS9$wrkprty
CarePolitics10$wrkprty<-ESS10$donprty;CarePolitics11$wrkprty<-ESS11$donprty

CarePolitics1$wrkorg<-ESS1$wrkorg;CarePolitics2$wrkorg<-ESS2$wrkorg;CarePolitics3$wrkorg<-ESS3$wrkorg
CarePolitics4$wrkorg<-ESS4$wrkorg;CarePolitics5$wrkorg<-ESS5$wrkorg;CarePolitics6$wrkorg<-ESS6$wrkorg
CarePolitics7$wrkorg<-ESS7$wrkorg;CarePolitics8$wrkorg<-ESS8$wrkorg;CarePolitics9$wrkorg<-ESS9$wrkorg
CarePolitics10$wrkorg<-rep(NA,nrow(CarePolitics10));CarePolitics11$wrkorg<-rep(NA,nrow(CarePolitics11))

CarePolitics1$badge<-ESS1$badge;CarePolitics2$badge<-ESS2$badge;CarePolitics3$badge<-ESS3$badge
CarePolitics4$badge<-ESS4$badge;CarePolitics5$badge<-ESS5$badge;CarePolitics6$badge<-ESS6$badge
CarePolitics7$badge<-ESS7$badge;CarePolitics8$badge<-ESS8$badge;CarePolitics9$badge<-ESS9$badge
CarePolitics10$badge<-ESS10$badge;CarePolitics11$badge<-ESS11$badge

CarePolitics1$sgnptit<-ESS1$sgnptit;CarePolitics2$sgnptit<-ESS2$sgnptit;CarePolitics3$sgnptit<-ESS3$sgnptit
CarePolitics4$sgnptit<-ESS4$sgnptit;CarePolitics5$sgnptit<-ESS5$sgnptit;CarePolitics6$sgnptit<-ESS6$sgnptit
CarePolitics7$sgnptit<-ESS7$sgnptit;CarePolitics8$sgnptit<-ESS8$sgnptit;CarePolitics9$sgnptit<-ESS9$sgnptit
CarePolitics10$sgnptit<-ESS10$sgnptit;CarePolitics11$sgnptit<-ESS11$sgnptit

CarePolitics1$pbldmn<-ESS1$pbldmn;CarePolitics2$pbldmn<-ESS2$pbldmn;CarePolitics3$pbldmn<-ESS3$pbldmn
CarePolitics4$pbldmn<-ESS4$pbldmn;CarePolitics5$pbldmn<-ESS5$pbldmn;CarePolitics6$pbldmn<-ESS6$pbldmn
CarePolitics7$pbldmn<-ESS7$pbldmn;CarePolitics8$pbldmn<-ESS8$pbldmn;CarePolitics9$pbldmn<-ESS9$pbldmn
CarePolitics10$pbldmn<-ESS10$pbldmn;CarePolitics11$pbldmn<-ESS11$pbldmn

CarePolitics1$pstplonl<-rep(NA,nrow(CarePolitics1));CarePolitics2$pstplonl<-rep(NA,nrow(CarePolitics2))
CarePolitics3$pstplonl<-rep(NA,nrow(CarePolitics3));CarePolitics4$pstplonl<-rep(NA,nrow(CarePolitics4))
CarePolitics5$pstplonl<-rep(NA,nrow(CarePolitics5));CarePolitics6$pstplonl<-rep(NA,nrow(CarePolitics6))
CarePolitics7$pstplonl<-rep(NA,nrow(CarePolitics7));CarePolitics8$pstplonl<-ESS8$pstplonl
CarePolitics9$pstplonl<-ESS9$pstplonl;CarePolitics10$pstplonl<-ESS10$pstplonl
CarePolitics11$pstplonl<-ESS11$pstplonl

# Trade union membership (mbtru)
CarePolitics1$mbtru<-ESS1$mbtru;CarePolitics2$mbtru<-ESS2$mbtru;CarePolitics3$mbtru<-ESS3$mbtru
CarePolitics4$mbtru<-ESS4$mbtru;CarePolitics5$mbtru<-ESS5$mbtru;CarePolitics6$mbtru<-ESS6$mbtru
CarePolitics7$mbtru<-ESS7$mbtru;CarePolitics8$mbtru<-ESS8$mbtru;CarePolitics9$mbtru<-ESS9$mbtru
CarePolitics10$mbtru<-ESS10$mbtru;CarePolitics11$mbtru<-ESS11$mbtru

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

## Working conditions
# Employment relation (emplrel)
CarePolitics1$emplrel<-ESS1$emplrel;CarePolitics2$emplrel<-ESS2$emplrel;CarePolitics3$emplrel<-ESS3$emplrel
CarePolitics4$emplrel<-ESS4$emplrel;CarePolitics5$emplrel<-ESS5$emplrel;CarePolitics6$emplrel<-ESS6$emplrel
CarePolitics7$emplrel<-ESS7$emplrel;CarePolitics8$emplrel<-ESS8$emplrel;CarePolitics9$emplrel<-ESS9$emplrel
CarePolitics10$emplrel<-ESS10$emplrel;CarePolitics11$emplrel<-ESS11$emplrel

# Type of contract (wrkctra)
CarePolitics1$wrkctra<-ESS1$wrkctr;CarePolitics2$wrkctra<-ESS2$wrkctra;CarePolitics3$wrkctra<-ESS3$wrkctra
CarePolitics4$wrkctra<-ESS4$wrkctra;CarePolitics5$wrkctra<-ESS5$wrkctra;CarePolitics6$wrkctra<-ESS6$wrkctra
CarePolitics7$wrkctra<-ESS7$wrkctra;CarePolitics8$wrkctra<-ESS8$wrkctra;CarePolitics9$wrkctra<-ESS9$wrkctra
CarePolitics10$wrkctra<-ESS10$wrkctra;CarePolitics11$wrkctra<-ESS11$wrkctra

# Total contracted hours (wkhct)
CarePolitics1$wkhct<-ESS1$wkhct;CarePolitics2$wkhct<-ESS2$wkhct;CarePolitics3$wkhct<-ESS3$wkhct
CarePolitics4$wkhct<-ESS4$wkhct;CarePolitics5$wkhct<-ESS5$wkhct;CarePolitics6$wkhct<-ESS6$wkhct
CarePolitics7$wkhct<-ESS7$wkhct;CarePolitics8$wkhct<-ESS8$wkhct;CarePolitics9$wkhct<-ESS9$wkhct
CarePolitics10$wkhct<-ESS10$wkhct;CarePolitics11$wkhct<-ESS11$wkhct

# Have a set basic or contracted number of hours (icwhct)
CarePolitics1$icwhct<-rep(NA,nrow(CarePolitics1));CarePolitics2$icwhct<-rep(NA,nrow(CarePolitics2))
CarePolitics3$icwhct<-rep(NA,nrow(CarePolitics3));CarePolitics4$icwhct<-rep(NA,nrow(CarePolitics4))
CarePolitics5$icwhct<-rep(NA,nrow(CarePolitics5));CarePolitics6$icwhct<-rep(NA,nrow(CarePolitics6))
CarePolitics7$icwhct<-ESS7$icwhct;CarePolitics8$icwhct<-ESS8$icwhct;CarePolitics9$icwhct<-ESS9$icwhct
CarePolitics10$icwhct<-rep(NA,nrow(CarePolitics10));CarePolitics11$icwhct<-rep(NA,nrow(CarePolitics11))

# Total normally worked hours overtime excluded (wkhtot)
CarePolitics1$wkhtot<-ESS1$wkhtot;CarePolitics2$wkhtot<-ESS2$wkhtot;CarePolitics3$wkhtot<-ESS3$wkhtot
CarePolitics4$wkhtot<-ESS4$wkhtot;CarePolitics5$wkhtot<-ESS5$wkhtot;CarePolitics6$wkhtot<-ESS6$wkhtot
CarePolitics7$wkhtot<-ESS7$wkhtot;CarePolitics8$wkhtot<-ESS8$wkhtot;CarePolitics9$wkhtot<-ESS9$wkhtot
CarePolitics10$wkhtot<-ESS10$wkhtot;CarePolitics11$wkhtot<-ESS11$wkhtot

# Employment sector (tporgwk)
CarePolitics1$tporgwk<-rep(NA,nrow(CarePolitics1));CarePolitics2$tporgwk<-rep(NA,nrow(CarePolitics2))
CarePolitics3$tporgwk<-rep(NA,nrow(CarePolitics3))
CarePolitics4$tporgwk<-ESS4$tporgwk;CarePolitics5$tporgwk<-ESS5$tporgwk;CarePolitics6$tporgwk<-ESS6$tporgwk
CarePolitics7$tporgwk<-ESS7$tporgwk;CarePolitics8$tporgwk<-ESS8$tporgwk;CarePolitics9$tporgwk<-ESS9$tporgwk
CarePolitics10$tporgwk<-ESS10$tporgwk;CarePolitics11$tporgwk<-ESS11$tporgwk
# There is a problem here because we are missing data for waves 1-3
# Imputation?

# Establishment size (estsz)
CarePolitics1$estsz<-ESS1$estsz;CarePolitics2$estsz<-ESS2$estsz;CarePolitics3$estsz<-ESS3$estsz
CarePolitics4$estsz<-ESS4$estsz;CarePolitics5$estsz<-ESS5$estsz;CarePolitics6$estsz<-ESS6$estsz
CarePolitics7$estsz<-ESS7$estsz;CarePolitics8$estsz<-ESS8$estsz;CarePolitics9$estsz<-ESS9$estsz
CarePolitics10$estsz<-ESS10$estsz;CarePolitics11$estsz<-ESS11$estsz

# Responsible for supervising other employees (jbspv)
CarePolitics1$jbspv<-ESS1$jbspv;CarePolitics2$jbspv<-ESS2$jbspv;CarePolitics3$jbspv<-ESS3$jbspv
CarePolitics4$jbspv<-ESS4$jbspv;CarePolitics5$jbspv<-ESS5$jbspv;CarePolitics6$jbspv<-ESS6$jbspv
CarePolitics7$jbspv<-ESS7$jbspv;CarePolitics8$jbspv<-ESS8$jbspv;CarePolitics9$jbspv<-ESS9$jbspv
CarePolitics10$jbspv<-ESS10$jbspv;CarePolitics11$jbspv<-ESS11$jbspv

# Number of people responsible for (njbspv)
CarePolitics1$njbspv<-ESS1$njbspv;CarePolitics2$njbspv<-ESS2$njbspv;CarePolitics3$njbspv<-ESS3$njbspv
CarePolitics4$njbspv<-ESS4$njbspv;CarePolitics5$njbspv<-ESS5$njbspv;CarePolitics6$njbspv<-ESS6$njbspv
CarePolitics7$njbspv<-ESS7$njbspv;CarePolitics8$njbspv<-ESS8$njbspv;CarePolitics9$njbspv<-ESS9$njbspv
CarePolitics10$njbspv<-ESS10$njbspv;CarePolitics11$njbspv<-ESS11$njbspv

# Allowed to decide how daily work is organised (wkdcorga)
CarePolitics1$wkdcorga<-ESS1$wkdcorg;CarePolitics2$wkdcorga<-ESS2$wkdcorga;CarePolitics3$wkdcorga<-ESS3$wkdcorga
CarePolitics4$wkdcorga<-ESS4$wkdcorga;CarePolitics5$wkdcorga<-ESS5$wkdcorga;CarePolitics6$wkdcorga<-ESS6$wkdcorga
CarePolitics7$wkdcorga<-ESS7$wkdcorga;CarePolitics8$wkdcorga<-ESS8$wkdcorga;CarePolitics9$wkdcorga<-ESS9$wkdcorga
CarePolitics10$wkdcorga<-ESS10$wkdcorga;CarePolitics11$wkdcorga<-ESS11$wkdcorga

# Allowed to influence policy decisions about activities of organisation (iorgact)
CarePolitics1$iorgact<-rep(NA,nrow(CarePolitics1))
CarePolitics2$iorgact<-ESS2$iorgact;CarePolitics3$iorgact<-ESS3$iorgact
CarePolitics4$iorgact<-ESS4$iorgact;CarePolitics5$iorgact<-ESS5$iorgact;CarePolitics6$iorgact<-ESS6$iorgact
CarePolitics7$iorgact<-ESS7$iorgact;CarePolitics8$iorgact<-ESS8$iorgact;CarePolitics9$iorgact<-ESS9$iorgact
CarePolitics10$iorgact<-ESS10$iorgact;CarePolitics11$iorgact<-ESS11$iorgact

# Likelihood of unemployment in next 12 months (lkuemp)
CarePolitics1$lkuemp<-rep(NA,nrow(CarePolitics1));CarePolitics2$lkuemp<-rep(NA,nrow(CarePolitics2))
CarePolitics3$lkuemp<-rep(NA,nrow(CarePolitics3));CarePolitics4$lkuemp<-ESS4$lkuemp
CarePolitics5$lkuemp<-rep(NA,nrow(CarePolitics5));CarePolitics6$lkuemp<-rep(NA,nrow(CarePolitics6))
CarePolitics7$lkuemp<-rep(NA,nrow(CarePolitics7));CarePolitics8$lkuemp<-ESS8$lkuemp
CarePolitics9$lkuemp<-rep(NA,nrow(CarePolitics9));CarePolitics10$lkuemp<-rep(NA,nrow(CarePolitics10))
CarePolitics11$lkuemp<-rep(NA,nrow(CarePolitics11))

# In any job ever, exposition to tiring or painful positions (jbexpti)
CarePolitics1$jbexpti<-rep(NA,nrow(CarePolitics1));CarePolitics2$jbexpti<-rep(NA,nrow(CarePolitics2))
CarePolitics3$jbexpti<-rep(NA,nrow(CarePolitics3));CarePolitics4$jbexpti<-rep(NA,nrow(CarePolitics4))
CarePolitics5$jbexpti<-rep(NA,nrow(CarePolitics5));CarePolitics6$jbexpti<-rep(NA,nrow(CarePolitics6))
CarePolitics7$jbexpti<-ESS7$jbexpti;CarePolitics8$jbexpti<-rep(NA,nrow(CarePolitics8))
CarePolitics9$jbexpti<-rep(NA,nrow(CarePolitics9));CarePolitics10$jbexpti<-rep(NA,nrow(CarePolitics10))
CarePolitics11$jbexpti<-ESS11$jbexpti

# In any job ever, exposition to manually lifting or moving people (jbexpmc)
CarePolitics1$jbexpmc<-rep(NA,nrow(CarePolitics1));CarePolitics2$jbexpmc<-rep(NA,nrow(CarePolitics2))
CarePolitics3$jbexpmc<-rep(NA,nrow(CarePolitics3));CarePolitics4$jbexpmc<-rep(NA,nrow(CarePolitics4))
CarePolitics5$jbexpmc<-rep(NA,nrow(CarePolitics5));CarePolitics6$jbexpmc<-rep(NA,nrow(CarePolitics6))
CarePolitics7$jbexpmc<-ESS7$jbexpmc;CarePolitics8$jbexpmc<-rep(NA,nrow(CarePolitics8))
CarePolitics9$jbexpmc<-rep(NA,nrow(CarePolitics9));CarePolitics10$jbexpmc<-rep(NA,nrow(CarePolitics10))
CarePolitics11$jbexpmc<-ESS11$jbexpmc

# Unfairly treated in hiring pay or promtion because of gender (trwrkmw)
CarePolitics1$trwrkmw<-rep(NA,nrow(CarePolitics1));CarePolitics2$trwrkmw<-rep(NA,nrow(CarePolitics2))
CarePolitics3$trwrkmw<-rep(NA,nrow(CarePolitics3));CarePolitics4$trwrkmw<-rep(NA,nrow(CarePolitics4))
CarePolitics5$trwrkmw<-rep(NA,nrow(CarePolitics5));CarePolitics6$trwrkmw<-rep(NA,nrow(CarePolitics6))
CarePolitics7$trwrkmw<-rep(NA,nrow(CarePolitics7));CarePolitics8$trwrkmw<-rep(NA,nrow(CarePolitics8))
CarePolitics9$trwrkmw<-rep(NA,nrow(CarePolitics9));CarePolitics10$trwrkmw<-rep(NA,nrow(CarePolitics10))
CarePolitics11$trwrkmw<-ESS11$trwrkmw

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

## Migration background
# Religiosity and religion items (rlgblg, rlgdnm, rlgdgr, rlgatnd)
CarePolitics1$rlgblg<-ESS1$rlgblg;CarePolitics2$rlgblg<-ESS2$rlgblg;CarePolitics3$rlgblg<-ESS3$rlgblg
CarePolitics4$rlgblg<-ESS4$rlgblg;CarePolitics5$rlgblg<-ESS5$rlgblg;CarePolitics6$rlgblg<-ESS6$rlgblg
CarePolitics7$rlgblg<-ESS7$rlgblg;CarePolitics8$rlgblg<-ESS8$rlgblg;CarePolitics9$rlgblg<-ESS9$rlgblg
CarePolitics10$rlgblg<-ESS10$rlgblg;CarePolitics11$rlgblg<-ESS11$rlgblg

CarePolitics1$rlgdnm<-ESS1$rlgdnm;CarePolitics2$rlgdnm<-ESS2$rlgdnm;CarePolitics3$rlgdnm<-ESS3$rlgdnm
CarePolitics4$rlgdnm<-ESS4$rlgdnm;CarePolitics5$rlgdnm<-ESS5$rlgdnm;CarePolitics6$rlgdnm<-ESS6$rlgdnm
CarePolitics7$rlgdnm<-ESS7$rlgdnm;CarePolitics8$rlgdnm<-ESS8$rlgdnm;CarePolitics9$rlgdnm<-ESS9$rlgdnm
CarePolitics10$rlgdnm<-ESS10$rlgdnm;CarePolitics11$rlgdnm<-ESS11$rlgdnm

CarePolitics1$rlgdgr<-ESS1$rlgdgr;CarePolitics2$rlgdgr<-ESS2$rlgdgr;CarePolitics3$rlgdgr<-ESS3$rlgdgr
CarePolitics4$rlgdgr<-ESS4$rlgdgr;CarePolitics5$rlgdgr<-ESS5$rlgdgr;CarePolitics6$rlgdgr<-ESS6$rlgdgr
CarePolitics7$rlgdgr<-ESS7$rlgdgr;CarePolitics8$rlgdgr<-ESS8$rlgdgr;CarePolitics9$rlgdgr<-ESS9$rlgdgr
CarePolitics10$rlgdgr<-ESS10$rlgdgr;CarePolitics11$rlgdgr<-ESS11$rlgdgr

CarePolitics1$rlgatnd<-ESS1$rlgatnd;CarePolitics2$rlgatnd<-ESS2$rlgatnd;CarePolitics3$rlgatnd<-ESS3$rlgatnd
CarePolitics4$rlgatnd<-ESS4$rlgatnd;CarePolitics5$rlgatnd<-ESS5$rlgatnd;CarePolitics6$rlgatnd<-ESS6$rlgatnd
CarePolitics7$rlgatnd<-ESS7$rlgatnd;CarePolitics8$rlgatnd<-ESS8$rlgatnd;CarePolitics9$rlgatnd<-ESS9$rlgatnd
CarePolitics10$rlgatnd<-ESS10$rlgatnd;CarePolitics11$rlgatnd<-ESS11$rlgatnd

# Member of a group discriminated in country (dscrgrp)
CarePolitics1$dscrgrp<-ESS1$dscrgrp;CarePolitics2$dscrgrp<-ESS2$dscrgrp;CarePolitics3$dscrgrp<-ESS3$dscrgrp
CarePolitics4$dscrgrp<-ESS4$dscrgrp;CarePolitics5$dscrgrp<-ESS5$dscrgrp;CarePolitics6$dscrgrp<-ESS6$dscrgrp
CarePolitics7$dscrgrp<-ESS7$dscrgrp;CarePolitics8$dscrgrp<-ESS8$dscrgrp;CarePolitics9$dscrgrp<-ESS9$dscrgrp
CarePolitics10$dscrgrp<-ESS10$dscrgrp;CarePolitics11$dscrgrp<-ESS11$dscrgrp

# Grounds of discrimination items (dscrrce, dscrntn,dscrrlg,dscrlng,dscretn)
CarePolitics1$dscrrce<-ESS1$dscrrce;CarePolitics2$dscrrce<-ESS2$dscrrce;CarePolitics3$dscrrce<-ESS3$dscrrce
CarePolitics4$dscrrce<-ESS4$dscrrce;CarePolitics5$dscrrce<-ESS5$dscrrce;CarePolitics6$dscrrce<-ESS6$dscrrce
CarePolitics7$dscrrce<-ESS7$dscrrce;CarePolitics8$dscrrce<-ESS8$dscrrce;CarePolitics9$dscrrce<-ESS9$dscrrce
CarePolitics10$dscrrce<-ESS10$dscrrce;CarePolitics11$dscrrce<-ESS11$dscrrce

CarePolitics1$dscrntn<-ESS1$dscrntn;CarePolitics2$dscrntn<-ESS2$dscrntn;CarePolitics3$dscrntn<-ESS3$dscrntn
CarePolitics4$dscrntn<-ESS4$dscrntn;CarePolitics5$dscrntn<-ESS5$dscrntn;CarePolitics6$dscrntn<-ESS6$dscrntn
CarePolitics7$dscrntn<-ESS7$dscrntn;CarePolitics8$dscrntn<-ESS8$dscrntn;CarePolitics9$dscrntn<-ESS9$dscrntn
CarePolitics10$dscrntn<-ESS10$dscrntn;CarePolitics11$dscrntn<-ESS11$dscrntn

CarePolitics1$dscrrlg<-ESS1$dscrrlg;CarePolitics2$dscrrlg<-ESS2$dscrrlg;CarePolitics3$dscrrlg<-ESS3$dscrrlg
CarePolitics4$dscrrlg<-ESS4$dscrrlg;CarePolitics5$dscrrlg<-ESS5$dscrrlg;CarePolitics6$dscrrlg<-ESS6$dscrrlg
CarePolitics7$dscrrlg<-ESS7$dscrrlg;CarePolitics8$dscrrlg<-ESS8$dscrrlg;CarePolitics9$dscrrlg<-ESS9$dscrrlg
CarePolitics10$dscrrlg<-ESS10$dscrrlg;CarePolitics11$dscrrlg<-ESS11$dscrrlg

CarePolitics1$dscrlng<-ESS1$dscrlng;CarePolitics2$dscrlng<-ESS2$dscrlng;CarePolitics3$dscrlng<-ESS3$dscrlng
CarePolitics4$dscrlng<-ESS4$dscrlng;CarePolitics5$dscrlng<-ESS5$dscrlng;CarePolitics6$dscrlng<-ESS6$dscrlng
CarePolitics7$dscrlng<-ESS7$dscrlng;CarePolitics8$dscrlng<-ESS8$dscrlng;CarePolitics9$dscrlng<-ESS9$dscrlng
CarePolitics10$dscrlng<-ESS10$dscrlng;CarePolitics11$dscrlng<-ESS11$dscrlng

CarePolitics1$dscretn<-ESS1$dscretn;CarePolitics2$dscretn<-ESS2$dscretn;CarePolitics3$dscretn<-ESS3$dscretn
CarePolitics4$dscretn<-ESS4$dscretn;CarePolitics5$dscretn<-ESS5$dscretn;CarePolitics6$dscretn<-ESS6$dscretn
CarePolitics7$dscretn<-ESS7$dscretn;CarePolitics8$dscretn<-ESS8$dscretn;CarePolitics9$dscretn<-ESS9$dscretn
CarePolitics10$dscretn<-ESS10$dscretn;CarePolitics11$dscretn<-ESS11$dscretn

# Citizen of country (ctzcntr)
CarePolitics1$ctzcntr<-ESS1$ctzcntr;CarePolitics2$ctzcntr<-ESS2$ctzcntr;CarePolitics3$ctzcntr<-ESS3$ctzcntr
CarePolitics4$ctzcntr<-ESS4$ctzcntr;CarePolitics5$ctzcntr<-ESS5$ctzcntr;CarePolitics6$ctzcntr<-ESS6$ctzcntr
CarePolitics7$ctzcntr<-ESS7$ctzcntr;CarePolitics8$ctzcntr<-ESS8$ctzcntr;CarePolitics9$ctzcntr<-ESS9$ctzcntr
CarePolitics10$ctzcntr<-ESS10$ctzcntr;CarePolitics11$ctzcntr<-ESS11$ctzcntr

# Citizenship (ctzship)
CarePolitics1$ctzship<-ESS1$ctzship;CarePolitics2$ctzship<-ESS2$ctzshipa;CarePolitics3$ctzship<-ESS3$ctzshipa
CarePolitics4$ctzship<-ESS4$ctzshipb;CarePolitics5$ctzship<-ESS5$ctzshipb;CarePolitics6$ctzship<-ESS6$ctzshipc
CarePolitics7$ctzship<-ESS7$ctzshipc;CarePolitics8$ctzship<-ESS8$ctzshipc;CarePolitics9$ctzship<-ESS9$ctzshipd
CarePolitics10$ctzship<-rep(NA,nrow(CarePolitics10));CarePolitics11$ctzship<-rep(NA,nrow(CarePolitics11))

# Born in country (brncntr)
CarePolitics1$brncntr<-ESS1$brncntr;CarePolitics2$brncntr<-ESS2$brncntr;CarePolitics3$brncntr<-ESS3$brncntr
CarePolitics4$brncntr<-ESS4$brncntr;CarePolitics5$brncntr<-ESS5$brncntr;CarePolitics6$brncntr<-ESS6$brncntr
CarePolitics7$brncntr<-ESS7$brncntr;CarePolitics8$brncntr<-ESS8$brncntr;CarePolitics9$brncntr<-ESS9$brncntr
CarePolitics10$brncntr<-ESS10$brncntr;CarePolitics11$brncntr<-ESS11$brncntr

# Country of birth (cntbrthc)
CarePolitics1$cntbrth<-ESS1$cntbrth;CarePolitics2$cntbrth<-ESS2$cntbrtha;CarePolitics3$cntbrth<-ESS3$cntbrtha
CarePolitics4$cntbrth<-ESS4$cntbrthb;CarePolitics5$cntbrth<-ESS5$cntbrthb;CarePolitics6$cntbrth<-ESS6$cntbrthc
CarePolitics7$cntbrth<-ESS7$cntbrthc;CarePolitics8$cntbrth<-ESS8$cntbrthc;CarePolitics9$cntbrth<-ESS9$cntbrthd
CarePolitics10$cntbrth<-ESS10$cntbrthd;CarePolitics11$cntbrth<-ESS11$cntbrthd

# Year first come to country (livecnta)
CarePolitics1$livecnta<-rep(NA,nrow(CarePolitics1));CarePolitics2$livecnta<-rep(NA,nrow(CarePolitics2))
CarePolitics3$livecnta<-rep(NA,nrow(CarePolitics3));CarePolitics4$livecnta<-rep(NA,nrow(CarePolitics4))
CarePolitics5$livecnta<-ESS5$livecnta;CarePolitics6$livecnta<-ESS6$livecnta;CarePolitics7$livecnta<-ESS7$livecnta
CarePolitics8$livecnta<-ESS8$livecnta;CarePolitics9$livecnta<-ESS9$livecnta;CarePolitics10$livecnta<-ESS10$livecnta
CarePolitics11$livecnta<-ESS11$livecnta

# How long ago came to country (livecntr)
CarePolitics1$livecntr<-ESS1$livecntr;CarePolitics2$livecntr<-ESS2$livecntr
CarePolitics3$livecntr<-ESS3$livecntr;CarePolitics4$livecntr<-ESS4$livecntr
# We need to calculate based on the year of arrival for waves 5-8
CarePolitics5$livecntr<-factor(nrow(CarePolitics5),levels=levels(CarePolitics1$livecntr))

CarePolitics5$livecntr[which((as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta)))==0)]="Within last year"
CarePolitics5$livecntr[which((as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta)))>=1 &
                               (as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta)))<=5)]="1-5 years ago"
CarePolitics5$livecntr[which((as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta)))>=6 &
                               (as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta)))<=10)]="6-10 years ago"
CarePolitics5$livecntr[which((as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta)))>=11 &
                               (as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta)))<=20)]="11-20 years ago"
CarePolitics5$livecntr[which((as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta)))>20)]="More than 20 years ago"
CarePolitics5$livecntr[which(is.na(as.numeric(paste(CarePolitics5$year))-as.numeric(paste(CarePolitics5$livecnta))))]=NA

CarePolitics5$livecntr[which(CarePolitics5$livecnta=="Not applicable")]="Not applicable"
CarePolitics5$livecntr[which(CarePolitics5$livecnta=="Refusal")]="Refusal"
CarePolitics5$livecntr[which(CarePolitics5$livecnta=="Don't know")]="Don't know"
CarePolitics5$livecntr[which(CarePolitics5$livecnta=="No answer")]="No answer"

CarePolitics5$livecntr<-factor(nrow(CarePolitics5),levels=levels(CarePolitics1$livecntr))

##
CarePolitics6$livecntr<-factor(nrow(CarePolitics6),levels=levels(CarePolitics1$livecntr))

CarePolitics6$livecntr[which((as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta)))==0)]="Within last year"
CarePolitics6$livecntr[which((as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta)))>=1 &
                               (as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta)))<=5)]="1-5 years ago"
CarePolitics6$livecntr[which((as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta)))>=6 &
                               (as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta)))<=10)]="6-10 years ago"
CarePolitics6$livecntr[which((as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta)))>=11 &
                               (as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta)))<=20)]="11-20 years ago"
CarePolitics6$livecntr[which((as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta)))>20)]="More than 20 years ago"
CarePolitics6$livecntr[which(is.na(as.numeric(paste(CarePolitics6$year))-as.numeric(paste(CarePolitics6$livecnta))))]=NA

CarePolitics6$livecntr[which(CarePolitics6$livecnta=="Not applicable")]="Not applicable"
CarePolitics6$livecntr[which(CarePolitics6$livecnta=="Refusal")]="Refusal"
CarePolitics6$livecntr[which(CarePolitics6$livecnta=="Don't know")]="Don't know"
CarePolitics6$livecntr[which(CarePolitics6$livecnta=="No answer")]="No answer"

##
CarePolitics7$livecntr<-factor(nrow(CarePolitics7),levels=levels(CarePolitics1$livecntr))

CarePolitics7$livecntr[which((as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta)))==0)]="Within last year"
CarePolitics7$livecntr[which((as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta)))>=1 &
                               (as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta)))<=5)]="1-5 years ago"
CarePolitics7$livecntr[which((as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta)))>=6 &
                               (as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta)))<=10)]="6-10 years ago"
CarePolitics7$livecntr[which((as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta)))>=11 &
                               (as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta)))<=20)]="11-20 years ago"
CarePolitics7$livecntr[which((as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta)))>20)]="More than 20 years ago"
CarePolitics7$livecntr[which(is.na(as.numeric(paste(CarePolitics7$year))-as.numeric(paste(CarePolitics7$livecnta))))]=NA

CarePolitics7$livecntr[which(CarePolitics7$livecnta=="Not applicable")]="Not applicable"
CarePolitics7$livecntr[which(CarePolitics7$livecnta=="Refusal")]="Refusal"
CarePolitics7$livecntr[which(CarePolitics7$livecnta=="Don't know")]="Don't know"
CarePolitics7$livecntr[which(CarePolitics7$livecnta=="No answer")]="No answer"

##
CarePolitics8$livecntr<-factor(nrow(CarePolitics8),levels=levels(CarePolitics1$livecntr))

CarePolitics8$livecntr[which((as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta)))==0)]="Within last year"
CarePolitics8$livecntr[which((as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta)))>=1 &
                               (as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta)))<=5)]="1-5 years ago"
CarePolitics8$livecntr[which((as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta)))>=6 &
                               (as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta)))<=10)]="6-10 years ago"
CarePolitics8$livecntr[which((as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta)))>=11 &
                               (as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta)))<=20)]="11-20 years ago"
CarePolitics8$livecntr[which((as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta)))>20)]="More than 20 years ago"
CarePolitics8$livecntr[which(is.na(as.numeric(paste(CarePolitics8$year))-as.numeric(paste(CarePolitics8$livecnta))))]=NA

CarePolitics8$livecntr[which(CarePolitics8$livecnta=="Not applicable")]="Not applicable"
CarePolitics8$livecntr[which(CarePolitics8$livecnta=="Refusal")]="Refusal"
CarePolitics8$livecntr[which(CarePolitics8$livecnta=="Don't know")]="Don't know"
CarePolitics8$livecntr[which(CarePolitics8$livecnta=="No answer")]="No answer"

##
CarePolitics9$livecntr<-factor(nrow(CarePolitics9),levels=levels(CarePolitics1$livecntr))

CarePolitics9$livecntr[which((as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta)))==0)]="Within last year"
CarePolitics9$livecntr[which((as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta)))>=1 &
                               (as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta)))<=5)]="1-5 years ago"
CarePolitics9$livecntr[which((as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta)))>=6 &
                               (as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta)))<=10)]="6-10 years ago"
CarePolitics9$livecntr[which((as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta)))>=11 &
                               (as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta)))<=20)]="11-20 years ago"
CarePolitics9$livecntr[which((as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta)))>20)]="More than 20 years ago"
CarePolitics9$livecntr[which(is.na(as.numeric(paste(CarePolitics9$year))-as.numeric(paste(CarePolitics9$livecnta))))]=NA

CarePolitics9$livecntr[which(CarePolitics9$livecnta=="Not applicable")]="Not applicable"
CarePolitics9$livecntr[which(CarePolitics9$livecnta=="Refusal")]="Refusal"
CarePolitics9$livecntr[which(CarePolitics9$livecnta=="Don't know")]="Don't know"
CarePolitics9$livecntr[which(CarePolitics9$livecnta=="No answer")]="No answer"

##
CarePolitics10$livecntr<-factor(nrow(CarePolitics10),levels=levels(CarePolitics1$livecntr))

CarePolitics10$livecntr[which((as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta)))==0)]="Within last year"
CarePolitics10$livecntr[which((as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta)))>=1 &
                               (as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta)))<=5)]="1-5 years ago"
CarePolitics10$livecntr[which((as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta)))>=6 &
                               (as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta)))<=10)]="6-10 years ago"
CarePolitics10$livecntr[which((as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta)))>=11 &
                               (as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta)))<=20)]="11-20 years ago"
CarePolitics10$livecntr[which((as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta)))>20)]="More than 20 years ago"
CarePolitics10$livecntr[which(is.na(as.numeric(paste(CarePolitics10$year))-as.numeric(paste(CarePolitics10$livecnta))))]=NA

CarePolitics10$livecntr[which(CarePolitics10$livecnta=="Not applicable")]="Not applicable"
CarePolitics10$livecntr[which(CarePolitics10$livecnta=="Refusal")]="Refusal"
CarePolitics10$livecntr[which(CarePolitics10$livecnta=="Don't know")]="Don't know"
CarePolitics10$livecntr[which(CarePolitics10$livecnta=="No answer")]="No answer"

##
CarePolitics11$livecntr<-factor(nrow(CarePolitics11),levels=levels(CarePolitics1$livecntr))

CarePolitics11$livecntr[which((as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta)))==0)]="Within last year"
CarePolitics11$livecntr[which((as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta)))>=1 &
                               (as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta)))<=5)]="1-5 years ago"
CarePolitics11$livecntr[which((as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta)))>=6 &
                               (as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta)))<=10)]="6-10 years ago"
CarePolitics11$livecntr[which((as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta)))>=11 &
                               (as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta)))<=20)]="11-20 years ago"
CarePolitics11$livecntr[which((as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta)))>20)]="More than 20 years ago"
CarePolitics11$livecntr[which(is.na(as.numeric(paste(CarePolitics11$year))-as.numeric(paste(CarePolitics11$livecnta))))]=NA

CarePolitics11$livecntr[which(CarePolitics11$livecnta=="Not applicable")]="Not applicable"
CarePolitics11$livecntr[which(CarePolitics11$livecnta=="Refusal")]="Refusal"
CarePolitics11$livecntr[which(CarePolitics11$livecnta=="Don't know")]="Don't know"
CarePolitics11$livecntr[which(CarePolitics11$livecnta=="No answer")]="No answer"

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

# Language(s) spoken more often at home (lnghom1, lnghom2)
CarePolitics1$lnghom1<-ESS1$lnghoma;CarePolitics2$lnghom1<-ESS2$lnghoma
CarePolitics3$lnghom1<-ESS3$lnghoma;CarePolitics4$lnghom1<-ESS4$lnghoma
CarePolitics5$lnghom1<-ESS5$lnghom1;CarePolitics6$lnghom1<-ESS6$lnghom1;CarePolitics7$lnghom1<-ESS7$lnghom1
CarePolitics8$lnghom1<-ESS8$lnghom1;CarePolitics9$lnghom1<-ESS9$lnghom1;CarePolitics10$lnghom1<-ESS10$lnghom1
CarePolitics11$lnghom1<-ESS11$lnghom1

CarePolitics1$lnghom2<-ESS1$lnghomb;CarePolitics2$lnghom2<-ESS2$lnghomb
CarePolitics3$lnghom2<-ESS3$lnghomb;CarePolitics4$lnghom2<-ESS4$lnghomb
CarePolitics5$lnghom2<-ESS5$lnghom2;CarePolitics6$lnghom2<-ESS6$lnghom2;CarePolitics7$lnghom2<-ESS7$lnghom2
CarePolitics8$lnghom2<-ESS8$lnghom2;CarePolitics9$lnghom2<-ESS9$lnghom2;CarePolitics10$lnghom2<-ESS10$lnghom2
CarePolitics11$lnghom2<-ESS11$lnghom2

# Belong to minority ethnic group in country (blgetmg)
CarePolitics1$blgetmg<-ESS1$blgetmg;CarePolitics2$blgetmg<-ESS2$blgetmg;CarePolitics3$blgetmg<-ESS3$blgetmg
CarePolitics4$blgetmg<-ESS4$blgetmg;CarePolitics5$blgetmg<-ESS5$blgetmg;CarePolitics6$blgetmg<-ESS6$blgetmg
CarePolitics7$blgetmg<-ESS7$blgetmg;CarePolitics8$blgetmg<-ESS8$blgetmg;CarePolitics9$blgetmg<-ESS9$blgetmg
CarePolitics10$blgetmg<-rep(NA,nrow(CarePolitics10));CarePolitics11$blgetmg<-rep(NA,nrow(CarePolitics11))

# Father born in country (facntr)
CarePolitics1$facntr<-ESS1$facntr;CarePolitics2$facntr<-ESS2$facntr;CarePolitics3$facntr<-ESS3$facntr
CarePolitics4$facntr<-ESS4$facntr;CarePolitics5$facntr<-ESS5$facntr;CarePolitics6$facntr<-ESS6$facntr
CarePolitics7$facntr<-ESS7$facntr;CarePolitics8$facntr<-ESS8$facntr;CarePolitics9$facntr<-ESS9$facntr
CarePolitics10$facntr<-ESS10$facntr;CarePolitics11$facntr<-ESS11$facntr

# Father's country/continent of birth (fbrncntb)
CarePolitics1$fbrncnt<-ESS1$facntn;CarePolitics2$fbrncnt<-ESS2$fbrncnt;CarePolitics3$fbrncnt<-ESS3$fbrncnt
CarePolitics4$fbrncnt<-ESS4$fbrncnta;CarePolitics5$fbrncnt<-ESS5$fbrncnta;CarePolitics6$fbrncnt<-ESS6$fbrncntb
CarePolitics7$fbrncnt<-ESS7$fbrncntb;CarePolitics8$fbrncnt<-ESS8$fbrncntb;CarePolitics9$fbrncnt<-ESS9$fbrncntc
CarePolitics10$fbrncnt<-ESS10$fbrncntc;CarePolitics11$fbrncnt<-ESS11$fbrncntc

# Mother born in country (mocntr)
CarePolitics1$mocntr<-ESS1$mocntr;CarePolitics2$mocntr<-ESS2$mocntr;CarePolitics3$mocntr<-ESS3$mocntr
CarePolitics4$mocntr<-ESS4$mocntr;CarePolitics5$mocntr<-ESS5$mocntr;CarePolitics6$mocntr<-ESS6$mocntr
CarePolitics7$mocntr<-ESS7$mocntr;CarePolitics8$mocntr<-ESS8$mocntr;CarePolitics9$mocntr<-ESS9$mocntr
CarePolitics10$mocntr<-ESS10$mocntr;CarePolitics11$mocntr<-ESS11$mocntr

# Mother's country of birth (mbrncntb)
CarePolitics1$mbrncnt<-ESS1$mocntn;CarePolitics2$mbrncnt<-ESS2$mbrncnt;CarePolitics3$mbrncnt<-ESS3$mbrncnt
CarePolitics4$mbrncnt<-ESS4$mbrncnta;CarePolitics5$mbrncnt<-ESS5$mbrncnta;CarePolitics6$mbrncnt<-ESS6$mbrncntb
CarePolitics7$mbrncnt<-ESS7$mbrncntb;CarePolitics8$mbrncnt<-ESS8$mbrncntb;CarePolitics9$mbrncnt<-ESS9$mbrncntc
CarePolitics10$mbrncnt<-ESS10$mbrncntc;CarePolitics11$mbrncnt<-ESS11$mbrncntc

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

## Sociodemographics
# Gender (gndr)
CarePolitics1$gndr<-ESS1$gndr;CarePolitics2$gndr<-ESS2$gndr;CarePolitics3$gndr<-ESS3$gndr
CarePolitics4$gndr<-ESS4$gndr;CarePolitics5$gndr<-ESS5$gndr;CarePolitics6$gndr<-ESS6$gndr
CarePolitics7$gndr<-ESS7$gndr;CarePolitics8$gndr<-ESS8$gndr;CarePolitics9$gndr<-ESS9$gndr
CarePolitics10$gndr<-ESS10$gndr;CarePolitics11$gndr<-ESS11$gndr

# Age (agea)
CarePolitics1$agea<-ESS1$agea;CarePolitics2$agea<-ESS2$agea;CarePolitics3$agea<-ESS3$agea
CarePolitics4$agea<-ESS4$agea;CarePolitics5$agea<-ESS5$agea;CarePolitics6$agea<-ESS6$agea
CarePolitics7$agea<-ESS7$agea;CarePolitics8$agea<-ESS8$agea;CarePolitics9$agea<-ESS9$agea
CarePolitics10$agea<-ESS10$agea;CarePolitics11$agea<-ESS11$agea

# Household members (hhmmb)
CarePolitics1$hhmmb<-ESS1$hhmmb;CarePolitics2$hhmmb<-ESS2$hhmmb;CarePolitics3$hhmmb<-ESS3$hhmmb
CarePolitics4$hhmmb<-ESS4$hhmmb;CarePolitics5$hhmmb<-ESS5$hhmmb;CarePolitics6$hhmmb<-ESS6$hhmmb
CarePolitics7$hhmmb<-ESS7$hhmmb;CarePolitics8$hhmmb<-ESS8$hhmmb;CarePolitics9$hhmmb<-ESS9$hhmmb
CarePolitics10$hhmmb<-ESS10$hhmmb;CarePolitics11$hhmmb<-ESS11$hhmmb

# Gender of each hh member (gndr2-12)
CarePolitics1$gndr2<-ESS1$gndr2;CarePolitics2$gndr2<-ESS2$gndr2;CarePolitics3$gndr2<-ESS3$gndr2
CarePolitics4$gndr2<-ESS4$gndr2;CarePolitics5$gndr2<-ESS5$gndr2;CarePolitics6$gndr2<-ESS6$gndr2
CarePolitics7$gndr2<-ESS7$gndr2;CarePolitics8$gndr2<-ESS8$gndr2;CarePolitics9$gndr2<-ESS9$gndr2
CarePolitics10$gndr2<-ESS10$gndr2;CarePolitics11$gndr2<-ESS11$gndr2

CarePolitics1$gndr3<-ESS1$gndr3;CarePolitics2$gndr3<-ESS2$gndr3;CarePolitics3$gndr3<-ESS3$gndr3
CarePolitics4$gndr3<-ESS4$gndr3;CarePolitics5$gndr3<-ESS5$gndr3;CarePolitics6$gndr3<-ESS6$gndr3
CarePolitics7$gndr3<-ESS7$gndr3;CarePolitics8$gndr3<-ESS8$gndr3;CarePolitics9$gndr3<-ESS9$gndr3
CarePolitics10$gndr3<-ESS10$gndr3;CarePolitics11$gndr3<-ESS11$gndr3

CarePolitics1$gndr4<-ESS1$gndr4;CarePolitics2$gndr4<-ESS2$gndr4;CarePolitics3$gndr4<-ESS3$gndr4
CarePolitics4$gndr4<-ESS4$gndr4;CarePolitics5$gndr4<-ESS5$gndr4;CarePolitics6$gndr4<-ESS6$gndr4
CarePolitics7$gndr4<-ESS7$gndr4;CarePolitics8$gndr4<-ESS8$gndr4;CarePolitics9$gndr4<-ESS9$gndr4
CarePolitics10$gndr4<-ESS10$gndr4;CarePolitics11$gndr4<-ESS11$gndr4

CarePolitics1$gndr5<-ESS1$gndr5;CarePolitics2$gndr5<-ESS2$gndr5;CarePolitics3$gndr5<-ESS3$gndr5
CarePolitics4$gndr5<-ESS4$gndr5;CarePolitics5$gndr5<-ESS5$gndr5;CarePolitics6$gndr5<-ESS6$gndr5
CarePolitics7$gndr5<-ESS7$gndr5;CarePolitics8$gndr5<-ESS8$gndr5;CarePolitics9$gndr5<-ESS9$gndr5
CarePolitics10$gndr5<-ESS10$gndr5;CarePolitics11$gndr5<-ESS11$gndr5

CarePolitics1$gndr6<-ESS1$gndr6;CarePolitics2$gndr6<-ESS2$gndr6;CarePolitics3$gndr6<-ESS3$gndr6
CarePolitics4$gndr6<-ESS4$gndr6;CarePolitics5$gndr6<-ESS5$gndr6;CarePolitics6$gndr6<-ESS6$gndr6
CarePolitics7$gndr6<-ESS7$gndr6;CarePolitics8$gndr6<-ESS8$gndr6;CarePolitics9$gndr6<-ESS9$gndr6
CarePolitics10$gndr6<-ESS10$gndr6;CarePolitics11$gndr6<-ESS11$gndr6

CarePolitics1$gndr7<-ESS1$gndr7;CarePolitics2$gndr7<-ESS2$gndr7;CarePolitics3$gndr7<-ESS3$gndr7
CarePolitics4$gndr7<-ESS4$gndr7;CarePolitics5$gndr7<-ESS5$gndr7;CarePolitics6$gndr7<-ESS6$gndr7
CarePolitics7$gndr7<-ESS7$gndr7;CarePolitics8$gndr7<-ESS8$gndr7;CarePolitics9$gndr7<-ESS9$gndr7
CarePolitics10$gndr7<-ESS10$gndr7;CarePolitics11$gndr7<-ESS11$gndr7

CarePolitics1$gndr8<-ESS1$gndr8;CarePolitics2$gndr8<-ESS2$gndr8;CarePolitics3$gndr8<-ESS3$gndr8
CarePolitics4$gndr8<-ESS4$gndr8;CarePolitics5$gndr8<-ESS5$gndr8;CarePolitics6$gndr8<-ESS6$gndr8
CarePolitics7$gndr8<-ESS7$gndr8;CarePolitics8$gndr8<-ESS8$gndr8;CarePolitics9$gndr8<-ESS9$gndr8
CarePolitics10$gndr8<-ESS10$gndr8;CarePolitics11$gndr8<-ESS11$gndr8

CarePolitics1$gndr9<-ESS1$gndr9;CarePolitics2$gndr9<-ESS2$gndr9;CarePolitics3$gndr9<-ESS3$gndr9
CarePolitics4$gndr9<-ESS4$gndr9;CarePolitics5$gndr9<-ESS5$gndr9;CarePolitics6$gndr9<-ESS6$gndr9
CarePolitics7$gndr9<-ESS7$gndr9;CarePolitics8$gndr9<-ESS8$gndr9;CarePolitics9$gndr9<-ESS9$gndr9
CarePolitics10$gndr9<-ESS10$gndr9;CarePolitics11$gndr9<-ESS11$gndr9

CarePolitics1$gndr10<-ESS1$gndr10;CarePolitics2$gndr10<-ESS2$gndr10;CarePolitics3$gndr10<-ESS3$gndr10
CarePolitics4$gndr10<-ESS4$gndr10;CarePolitics5$gndr10<-ESS5$gndr10;CarePolitics6$gndr10<-ESS6$gndr10
CarePolitics7$gndr10<-ESS7$gndr10;CarePolitics8$gndr10<-ESS8$gndr10;CarePolitics9$gndr10<-ESS9$gndr10
CarePolitics10$gndr10<-ESS10$gndr10;CarePolitics11$gndr10<-ESS11$gndr10

CarePolitics1$gndr11<-ESS1$gndr11;CarePolitics2$gndr11<-ESS2$gndr11;CarePolitics3$gndr11<-ESS3$gndr11
CarePolitics4$gndr11<-ESS4$gndr11;CarePolitics5$gndr11<-ESS5$gndr11;CarePolitics6$gndr11<-ESS6$gndr11
CarePolitics7$gndr11<-ESS7$gndr11;CarePolitics8$gndr11<-ESS8$gndr11;CarePolitics9$gndr11<-ESS9$gndr11
CarePolitics10$gndr11<-ESS10$gndr11;CarePolitics11$gndr11<-ESS11$gndr11

CarePolitics1$gndr12<-ESS1$gndr12;CarePolitics2$gndr12<-ESS2$gndr12;CarePolitics3$gndr12<-ESS3$gndr12
CarePolitics4$gndr12<-ESS4$gndr12;CarePolitics5$gndr12<-ESS5$gndr12;CarePolitics6$gndr12<-ESS6$gndr12
CarePolitics7$gndr12<-ESS7$gndr12;CarePolitics8$gndr12<-ESS8$gndr12;CarePolitics9$gndr12<-ESS9$gndr12
CarePolitics10$gndr12<-ESS10$gndr12;CarePolitics11$gndr12<-ESS11$gndr12

# Year born each HH member (yrbrn2-12)
CarePolitics1$yrbrn2<-ESS1$yrbrn2;CarePolitics2$yrbrn2<-ESS2$yrbrn2;CarePolitics3$yrbrn2<-ESS3$yrbrn2
CarePolitics4$yrbrn2<-ESS4$yrbrn2;CarePolitics5$yrbrn2<-ESS5$yrbrn2;CarePolitics6$yrbrn2<-ESS6$yrbrn2
CarePolitics7$yrbrn2<-ESS7$yrbrn2;CarePolitics8$yrbrn2<-ESS8$yrbrn2;CarePolitics9$yrbrn2<-ESS9$yrbrn2
CarePolitics10$yrbrn2<-ESS10$yrbrn2;CarePolitics11$yrbrn2<-ESS11$yrbrn2

CarePolitics1$yrbrn3<-ESS1$yrbrn3;CarePolitics2$yrbrn3<-ESS2$yrbrn3;CarePolitics3$yrbrn3<-ESS3$yrbrn3
CarePolitics4$yrbrn3<-ESS4$yrbrn3;CarePolitics5$yrbrn3<-ESS5$yrbrn3;CarePolitics6$yrbrn3<-ESS6$yrbrn3
CarePolitics7$yrbrn3<-ESS7$yrbrn3;CarePolitics8$yrbrn3<-ESS8$yrbrn3;CarePolitics9$yrbrn3<-ESS9$yrbrn3
CarePolitics10$yrbrn3<-ESS10$yrbrn3;CarePolitics11$yrbrn3<-ESS11$yrbrn3

CarePolitics1$yrbrn4<-ESS1$yrbrn4;CarePolitics2$yrbrn4<-ESS2$yrbrn4;CarePolitics3$yrbrn4<-ESS3$yrbrn4
CarePolitics4$yrbrn4<-ESS4$yrbrn4;CarePolitics5$yrbrn4<-ESS5$yrbrn4;CarePolitics6$yrbrn4<-ESS6$yrbrn4
CarePolitics7$yrbrn4<-ESS7$yrbrn4;CarePolitics8$yrbrn4<-ESS8$yrbrn4;CarePolitics9$yrbrn4<-ESS9$yrbrn4
CarePolitics10$yrbrn4<-ESS10$yrbrn4;CarePolitics11$yrbrn4<-ESS11$yrbrn4

CarePolitics1$yrbrn5<-ESS1$yrbrn5;CarePolitics2$yrbrn5<-ESS2$yrbrn5;CarePolitics3$yrbrn5<-ESS3$yrbrn5
CarePolitics4$yrbrn5<-ESS4$yrbrn5;CarePolitics5$yrbrn5<-ESS5$yrbrn5;CarePolitics6$yrbrn5<-ESS6$yrbrn5
CarePolitics7$yrbrn5<-ESS7$yrbrn5;CarePolitics8$yrbrn5<-ESS8$yrbrn5;CarePolitics9$yrbrn5<-ESS9$yrbrn5
CarePolitics10$yrbrn5<-ESS10$yrbrn5;CarePolitics11$yrbrn5<-ESS11$yrbrn5

CarePolitics1$yrbrn6<-ESS1$yrbrn6;CarePolitics2$yrbrn6<-ESS2$yrbrn6;CarePolitics3$yrbrn6<-ESS3$yrbrn6
CarePolitics4$yrbrn6<-ESS4$yrbrn6;CarePolitics5$yrbrn6<-ESS5$yrbrn6;CarePolitics6$yrbrn6<-ESS6$yrbrn6
CarePolitics7$yrbrn6<-ESS7$yrbrn6;CarePolitics8$yrbrn6<-ESS8$yrbrn6;CarePolitics9$yrbrn6<-ESS9$yrbrn6
CarePolitics10$yrbrn6<-ESS10$yrbrn6;CarePolitics11$yrbrn6<-ESS11$yrbrn6

CarePolitics1$yrbrn7<-ESS1$yrbrn7;CarePolitics2$yrbrn7<-ESS2$yrbrn7;CarePolitics3$yrbrn7<-ESS3$yrbrn7
CarePolitics4$yrbrn7<-ESS4$yrbrn7;CarePolitics5$yrbrn7<-ESS5$yrbrn7;CarePolitics6$yrbrn7<-ESS6$yrbrn7
CarePolitics7$yrbrn7<-ESS7$yrbrn7;CarePolitics8$yrbrn7<-ESS8$yrbrn7;CarePolitics9$yrbrn7<-ESS9$yrbrn7
CarePolitics10$yrbrn7<-ESS10$yrbrn7;CarePolitics11$yrbrn7<-ESS11$yrbrn7

CarePolitics1$yrbrn8<-ESS1$yrbrn8;CarePolitics2$yrbrn8<-ESS2$yrbrn8;CarePolitics3$yrbrn8<-ESS3$yrbrn8
CarePolitics4$yrbrn8<-ESS4$yrbrn8;CarePolitics5$yrbrn8<-ESS5$yrbrn8;CarePolitics6$yrbrn8<-ESS6$yrbrn8
CarePolitics7$yrbrn8<-ESS7$yrbrn8;CarePolitics8$yrbrn8<-ESS8$yrbrn8;CarePolitics9$yrbrn8<-ESS9$yrbrn8
CarePolitics10$yrbrn8<-ESS10$yrbrn8;CarePolitics11$yrbrn8<-ESS11$yrbrn8

CarePolitics1$yrbrn9<-ESS1$yrbrn9;CarePolitics2$yrbrn9<-ESS2$yrbrn9;CarePolitics3$yrbrn9<-ESS3$yrbrn9
CarePolitics4$yrbrn9<-ESS4$yrbrn9;CarePolitics5$yrbrn9<-ESS5$yrbrn9;CarePolitics6$yrbrn9<-ESS6$yrbrn9
CarePolitics7$yrbrn9<-ESS7$yrbrn9;CarePolitics8$yrbrn9<-ESS8$yrbrn9;CarePolitics9$yrbrn9<-ESS9$yrbrn9
CarePolitics10$yrbrn9<-ESS10$yrbrn9;CarePolitics11$yrbrn9<-ESS11$yrbrn9

CarePolitics1$yrbrn10<-ESS1$yrbrn10;CarePolitics2$yrbrn10<-ESS2$yrbrn10;CarePolitics3$yrbrn10<-ESS3$yrbrn10
CarePolitics4$yrbrn10<-ESS4$yrbrn10;CarePolitics5$yrbrn10<-ESS5$yrbrn10;CarePolitics6$yrbrn10<-ESS6$yrbrn10
CarePolitics7$yrbrn10<-ESS7$yrbrn10;CarePolitics8$yrbrn10<-ESS8$yrbrn10;CarePolitics9$yrbrn10<-ESS9$yrbrn10
CarePolitics10$yrbrn10<-ESS10$yrbrn10;CarePolitics11$yrbrn10<-ESS11$yrbrn10

CarePolitics1$yrbrn11<-ESS1$yrbrn11;CarePolitics2$yrbrn11<-ESS2$yrbrn11;CarePolitics3$yrbrn11<-ESS3$yrbrn11
CarePolitics4$yrbrn11<-ESS4$yrbrn11;CarePolitics5$yrbrn11<-ESS5$yrbrn11;CarePolitics6$yrbrn11<-ESS6$yrbrn11
CarePolitics7$yrbrn11<-ESS7$yrbrn11;CarePolitics8$yrbrn11<-ESS8$yrbrn11;CarePolitics9$yrbrn11<-ESS9$yrbrn11
CarePolitics10$yrbrn11<-ESS10$yrbrn11;CarePolitics11$yrbrn11<-ESS11$yrbrn11

CarePolitics1$yrbrn12<-ESS1$yrbrn12;CarePolitics2$yrbrn12<-ESS2$yrbrn12;CarePolitics3$yrbrn12<-ESS3$yrbrn12
CarePolitics4$yrbrn12<-ESS4$yrbrn12;CarePolitics5$yrbrn12<-ESS5$yrbrn12;CarePolitics6$yrbrn12<-ESS6$yrbrn12
CarePolitics7$yrbrn12<-ESS7$yrbrn12;CarePolitics8$yrbrn12<-ESS8$yrbrn12;CarePolitics9$yrbrn12<-ESS9$yrbrn12
CarePolitics10$yrbrn12<-ESS10$yrbrn12;CarePolitics11$yrbrn12<-ESS11$yrbrn12

# Relationship to respondent of each hh member (rshipa2-12)
CarePolitics1$rshipa2<-ESS1$rship2;CarePolitics2$rshipa2<-ESS2$rshipa2;CarePolitics3$rshipa2<-ESS3$rshipa2
CarePolitics4$rshipa2<-ESS4$rshipa2;CarePolitics5$rshipa2<-ESS5$rshipa2;CarePolitics6$rshipa2<-ESS6$rshipa2
CarePolitics7$rshipa2<-ESS7$rshipa2;CarePolitics8$rshipa2<-ESS8$rshipa2;CarePolitics9$rshipa2<-ESS9$rshipa2
CarePolitics10$rshipa2<-ESS10$rshipa2;CarePolitics11$rshipa2<-ESS11$rshipa2

CarePolitics1$rshipa3<-ESS1$rship3;CarePolitics2$rshipa3<-ESS2$rshipa3;CarePolitics3$rshipa3<-ESS3$rshipa3
CarePolitics4$rshipa3<-ESS4$rshipa3;CarePolitics5$rshipa3<-ESS5$rshipa3;CarePolitics6$rshipa3<-ESS6$rshipa3
CarePolitics7$rshipa3<-ESS7$rshipa3;CarePolitics8$rshipa3<-ESS8$rshipa3;CarePolitics9$rshipa3<-ESS9$rshipa3
CarePolitics10$rshipa3<-ESS10$rshipa3;CarePolitics11$rshipa3<-ESS11$rshipa3

CarePolitics1$rshipa4<-ESS1$rship4;CarePolitics2$rshipa4<-ESS2$rshipa4;CarePolitics3$rshipa4<-ESS3$rshipa4
CarePolitics4$rshipa4<-ESS4$rshipa4;CarePolitics5$rshipa4<-ESS5$rshipa4;CarePolitics6$rshipa4<-ESS6$rshipa4
CarePolitics7$rshipa4<-ESS7$rshipa4;CarePolitics8$rshipa4<-ESS8$rshipa4;CarePolitics9$rshipa4<-ESS9$rshipa4
CarePolitics10$rshipa4<-ESS10$rshipa4;CarePolitics11$rshipa4<-ESS11$rshipa4

CarePolitics1$rshipa5<-ESS1$rship5;CarePolitics2$rshipa5<-ESS2$rshipa5;CarePolitics3$rshipa5<-ESS3$rshipa5
CarePolitics4$rshipa5<-ESS4$rshipa5;CarePolitics5$rshipa5<-ESS5$rshipa5;CarePolitics6$rshipa5<-ESS6$rshipa5
CarePolitics7$rshipa5<-ESS7$rshipa5;CarePolitics8$rshipa5<-ESS8$rshipa5;CarePolitics9$rshipa5<-ESS9$rshipa5
CarePolitics10$rshipa5<-ESS10$rshipa5;CarePolitics11$rshipa5<-ESS11$rshipa5

CarePolitics1$rshipa6<-ESS1$rship6;CarePolitics2$rshipa6<-ESS2$rshipa6;CarePolitics3$rshipa6<-ESS3$rshipa6
CarePolitics4$rshipa6<-ESS4$rshipa6;CarePolitics5$rshipa6<-ESS5$rshipa6;CarePolitics6$rshipa6<-ESS6$rshipa6
CarePolitics7$rshipa6<-ESS7$rshipa6;CarePolitics8$rshipa6<-ESS8$rshipa6;CarePolitics9$rshipa6<-ESS9$rshipa6
CarePolitics10$rshipa6<-ESS10$rshipa6;CarePolitics11$rshipa6<-ESS11$rshipa6

CarePolitics1$rshipa7<-ESS1$rship7;CarePolitics2$rshipa7<-ESS2$rshipa7;CarePolitics3$rshipa7<-ESS3$rshipa7
CarePolitics4$rshipa7<-ESS4$rshipa7;CarePolitics5$rshipa7<-ESS5$rshipa7;CarePolitics6$rshipa7<-ESS6$rshipa7
CarePolitics7$rshipa7<-ESS7$rshipa7;CarePolitics8$rshipa7<-ESS8$rshipa7;CarePolitics9$rshipa7<-ESS9$rshipa7
CarePolitics10$rshipa7<-ESS10$rshipa7;CarePolitics11$rshipa7<-ESS11$rshipa7

CarePolitics1$rshipa8<-ESS1$rship8;CarePolitics2$rshipa8<-ESS2$rshipa8;CarePolitics3$rshipa8<-ESS3$rshipa8
CarePolitics4$rshipa8<-ESS4$rshipa8;CarePolitics5$rshipa8<-ESS5$rshipa8;CarePolitics6$rshipa8<-ESS6$rshipa8
CarePolitics7$rshipa8<-ESS7$rshipa8;CarePolitics8$rshipa8<-ESS8$rshipa8;CarePolitics9$rshipa8<-ESS9$rshipa8
CarePolitics10$rshipa8<-ESS10$rshipa8;CarePolitics11$rshipa8<-ESS11$rshipa8

CarePolitics1$rshipa9<-ESS1$rship9;CarePolitics2$rshipa9<-ESS2$rshipa9;CarePolitics3$rshipa9<-ESS3$rshipa9
CarePolitics4$rshipa9<-ESS4$rshipa9;CarePolitics5$rshipa9<-ESS5$rshipa9;CarePolitics6$rshipa9<-ESS6$rshipa9
CarePolitics7$rshipa9<-ESS7$rshipa9;CarePolitics8$rshipa9<-ESS8$rshipa9;CarePolitics9$rshipa9<-ESS9$rshipa9
CarePolitics10$rshipa9<-ESS10$rshipa9;CarePolitics11$rshipa9<-ESS11$rshipa9

CarePolitics1$rshipa10<-ESS1$rship10;CarePolitics2$rshipa10<-ESS2$rshipa10;CarePolitics3$rshipa10<-ESS3$rshipa10
CarePolitics4$rshipa10<-ESS4$rshipa10;CarePolitics5$rshipa10<-ESS5$rshipa10;CarePolitics6$rshipa10<-ESS6$rshipa10
CarePolitics7$rshipa10<-ESS7$rshipa10;CarePolitics8$rshipa10<-ESS8$rshipa10;CarePolitics9$rshipa10<-ESS9$rshipa10
CarePolitics10$rshipa10<-ESS10$rshipa10;CarePolitics11$rshipa10<-ESS11$rshipa10

CarePolitics1$rshipa11<-ESS1$rship11;CarePolitics2$rshipa11<-ESS2$rshipa11;CarePolitics3$rshipa11<-ESS3$rshipa11
CarePolitics4$rshipa11<-ESS4$rshipa11;CarePolitics5$rshipa11<-ESS5$rshipa11;CarePolitics6$rshipa11<-ESS6$rshipa11
CarePolitics7$rshipa11<-ESS7$rshipa11;CarePolitics8$rshipa11<-ESS8$rshipa11;CarePolitics9$rshipa11<-ESS9$rshipa11
CarePolitics10$rshipa11<-ESS10$rshipa11;CarePolitics11$rshipa11<-ESS11$rshipa11

CarePolitics1$rshipa12<-ESS1$rship12;CarePolitics2$rshipa12<-ESS2$rshipa12;CarePolitics3$rshipa12<-ESS3$rshipa12
CarePolitics4$rshipa12<-ESS4$rshipa12;CarePolitics5$rshipa12<-ESS5$rshipa12;CarePolitics6$rshipa12<-ESS6$rshipa12
CarePolitics7$rshipa12<-ESS7$rshipa12;CarePolitics8$rshipa12<-ESS8$rshipa12;CarePolitics9$rshipa12<-ESS9$rshipa12
CarePolitics10$rshipa12<-ESS10$rshipa12;CarePolitics11$rshipa12<-ESS11$rshipa12

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

# Lives with partner (icpart)
CarePolitics1$lwp<-ESS1$partner;CarePolitics2$lwp<-ESS2$partner
CarePolitics3$lwp<-ESS3$partner;CarePolitics4$lwp<-ESS4$partner
CarePolitics5$lwp<-ESS5$icpart1;CarePolitics6$lwp<-ESS6$icpart1
CarePolitics7$lwp<-ESS7$icpart1;CarePolitics8$lwp<-ESS8$icpart1

CarePolitics9$lwp<-factor(nrow(CarePolitics9),levels=levels(CarePolitics8$lwp))
for (i in 1:nrow(CarePolitics9)){
  if ("Husband/wife/partner" %in% c(CarePolitics9$rshipa2[i],CarePolitics9$rshipa3[i],CarePolitics9$rshipa4[i],
                                    CarePolitics9$rshipa5[i],CarePolitics9$rshipa6[i],CarePolitics9$rshipa7[i],
                                    CarePolitics9$rshipa8[i],CarePolitics9$rshipa9[i],CarePolitics9$rshipa10[i],
                                    CarePolitics9$rshipa11[i],CarePolitics9$rshipa12[i])){
    CarePolitics9$lwp[i]="Respondent lives with husband/wife/partner"}
  else{CarePolitics9$lwp[i]="Does not"}
}
CarePolitics9$lwp[which(CarePolitics9$hhmmb!=1 & CarePolitics9$lwp=="Does not" & 
      CarePolitics9$rshipa2 %in% c("Not applicable","Refusal","Don't know","No answer"))]="Not available"

CarePolitics10$lwp<-factor(nrow(CarePolitics10),levels=levels(CarePolitics8$lwp))
for (i in 1:nrow(CarePolitics10)){
  if ("Husband/wife/partner" %in% c(CarePolitics10$rshipa2[i],CarePolitics10$rshipa3[i],CarePolitics10$rshipa4[i],
                                    CarePolitics10$rshipa5[i],CarePolitics10$rshipa6[i],CarePolitics10$rshipa7[i],
                                    CarePolitics10$rshipa8[i],CarePolitics10$rshipa9[i],CarePolitics10$rshipa10[i],
                                    CarePolitics10$rshipa11[i],CarePolitics10$rshipa12[i])){
    CarePolitics10$lwp[i]="Respondent lives with husband/wife/partner"}
  else{CarePolitics10$lwp[i]="Does not"}
}
CarePolitics10$lwp[which(CarePolitics10$hhmmb!=1 & CarePolitics10$lwp=="Does not" & 
        CarePolitics10$rshipa2 %in% c("Not applicable","Refusal","Don't know","No answer"))]="Not available"

CarePolitics11$lwp<-factor(nrow(CarePolitics11),levels=levels(CarePolitics8$lwp))
for (i in 1:nrow(CarePolitics11)){
  if ("Husband/wife/partner" %in% c(CarePolitics11$rshipa2[i],CarePolitics11$rshipa3[i],CarePolitics11$rshipa4[i],
                                    CarePolitics11$rshipa5[i],CarePolitics11$rshipa6[i],CarePolitics11$rshipa7[i],
                                    CarePolitics11$rshipa8[i],CarePolitics11$rshipa9[i],CarePolitics11$rshipa10[i],
                                    CarePolitics11$rshipa11[i],CarePolitics11$rshipa12[i])){
    CarePolitics11$lwp[i]="Respondent lives with husband/wife/partner"}
  else{CarePolitics11$lwp[i]="Does not"}
}
CarePolitics11$lwp[which(CarePolitics11$hhmmb!=1 & CarePolitics11$lwp=="Does not" & 
        CarePolitics11$rshipa2 %in% c("Not applicable","Refusal","Don't know","No answer"))]="Not available"

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

# Marital status (marsts) (note: levels need to be harmonised) 
CarePolitics1$marsts<-ESS1$marital;CarePolitics2$marsts<-ESS2$marital
CarePolitics3$marsts<-ESS3$maritala;CarePolitics4$marsts<-ESS4$maritala
CarePolitics5$marsts<-ESS5$marsts;CarePolitics6$marsts<-ESS6$marsts
CarePolitics7$marsts<-ESS7$marsts;CarePolitics8$marsts<-ESS8$marsts
CarePolitics9$marsts<-ESS9$marsts;CarePolitics10$marsts<-ESS10$marsts
CarePolitics11$marsts<-ESS11$marsts

# Children living at home (chldhm)
CarePolitics1$chldhm<-ESS1$chldhm;CarePolitics2$chldhm<-ESS2$chldhm;CarePolitics3$chldhm<-ESS3$chldhm
CarePolitics4$chldhm<-ESS4$chldhm;CarePolitics5$chldhm<-ESS5$chldhm;CarePolitics6$chldhm<-ESS6$chldhm
CarePolitics7$chldhm<-ESS7$chldhm;CarePolitics8$chldhm<-ESS8$chldhm;
CarePolitics9$chldhm<-rep(NA,nrow(CarePolitics9));CarePolitics10$chldhm<-rep(NA,nrow(CarePolitics10))
CarePolitics11$chldhm<-rep(NA,nrow(CarePolitics11))

# Educational level (eisced)
CarePolitics1$eisced<-ESS1$eisced;CarePolitics2$eisced<-ESS2$eisced;CarePolitics3$eisced<-ESS3$eisced
CarePolitics4$eisced<-ESS4$eisced;CarePolitics5$eisced<-ESS5$eisced;CarePolitics6$eisced<-ESS6$eisced
CarePolitics7$eisced<-ESS7$eisced;CarePolitics8$eisced<-ESS8$eisced;CarePolitics9$eisced<-ESS9$eisced
CarePolitics10$eisced<-ESS10$eisced;CarePolitics11$eisced<-ESS11$eisced

# Educational level in France
CarePolitics1$edlvfr<-ESS1$edlvfr;CarePolitics2$edlvfr<-ESS2$edlvfr
CarePolitics3$edlvfr<-rep(NA,nrow(CarePolitics3));CarePolitics4$edlvfr<-rep(NA,nrow(CarePolitics4))
CarePolitics5$edlvfr<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvfr<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvfr<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvfr<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvfr<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvfr<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvfr<-rep(NA,nrow(CarePolitics11))

# Educational level in UK
CarePolitics1$edlvgb<-ESS1$edlvgb;CarePolitics2$edlvgb<-ESS2$edlvagb
CarePolitics3$edlvgb<-ESS3$edlvgb;CarePolitics4$edlvgb<-ESS4$edlvgb
CarePolitics5$edlvgb<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvgb<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvgb<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvgb<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvgb<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvgb<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvgb<-rep(NA,nrow(CarePolitics11))

# Educational level in Greece
CarePolitics1$edlvgr<-ESS1$edlvgr;CarePolitics2$edlvgr<-ESS2$edlvgr
CarePolitics3$edlvgr<-rep(NA,nrow(CarePolitics3));CarePolitics4$edlvgr<-ESS4$edlvagr
CarePolitics5$edlvgr<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvgr<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvgr<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvgr<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvgr<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvgr<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvgr<-rep(NA,nrow(CarePolitics11))

# Educational level in Ireland
CarePolitics1$edlvie<-ESS1$edlvie;CarePolitics2$edlvie<-ESS2$edlvie
CarePolitics3$edlvie<-ESS3$edlvaie;CarePolitics4$edlvie<-ESS4$edlvbie
CarePolitics5$edlvie<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvie<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvie<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvie<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvie<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvie<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvie<-rep(NA,nrow(CarePolitics11))

# Educational level in Italy
CarePolitics1$edlvit<-ESS1$edlvit;CarePolitics2$edlvit<-ESS2$edlvait
CarePolitics3$edlvit<-rep(NA,nrow(CarePolitics3));CarePolitics4$edlvit<-rep(NA,nrow(CarePolitics4))
CarePolitics5$edlvit<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvit<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvit<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvit<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvit<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvit<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvit<-rep(NA,nrow(CarePolitics11))

# Educational level in Portugal
CarePolitics1$edlvpt<-ESS1$edlvpt;CarePolitics2$edlvpt<-ESS2$edlvpt
CarePolitics3$edlvpt<-ESS3$edlvapt;CarePolitics4$edlvpt<-rep(NA,nrow(CarePolitics4))
CarePolitics5$edlvpt<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvpt<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvpt<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvpt<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvpt<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvpt<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvpt<-rep(NA,nrow(CarePolitics11))

# Educational level in Sweden
CarePolitics1$edlvse<-ESS1$edlvse;CarePolitics2$edlvse<-ESS2$edlvse
CarePolitics3$edlvse<-ESS3$edlvase;CarePolitics4$edlvse<-ESS4$edlvase
CarePolitics5$edlvse<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvse<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvse<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvse<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvse<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvse<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvse<-rep(NA,nrow(CarePolitics11))

# Educational level in Ukraine
CarePolitics1$edlvua<-rep(NA,nrow(CarePolitics1));CarePolitics2$edlvua<-ESS2$edlvua
CarePolitics3$edlvua<-ESS3$edlvua;CarePolitics4$edlvua<-rep(NA,nrow(CarePolitics4))
CarePolitics5$edlvua<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvua<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvua<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvua<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvua<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvua<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvua<-rep(NA,nrow(CarePolitics11))

# Educational level in Bulgaria
CarePolitics1$edlvbg<-rep(NA,nrow(CarePolitics1));CarePolitics2$edlvbg<-rep(NA,nrow(CarePolitics2))
CarePolitics3$edlvbg<-ESS3$edlvbg;CarePolitics4$edlvbg<-rep(NA,nrow(CarePolitics4))
CarePolitics5$edlvbg<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvbg<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvbg<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvbg<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvbg<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvbg<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvbg<-rep(NA,nrow(CarePolitics11))

# Educational level in Cyprus
CarePolitics1$edlvcy<-rep(NA,nrow(CarePolitics1));CarePolitics2$edlvcy<-rep(NA,nrow(CarePolitics2))
CarePolitics3$edlvcy<-ESS3$edlvcy;CarePolitics4$edlvcy<-ESS3$edlvacy
CarePolitics5$edlvcy<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvcy<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvcy<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvcy<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvcy<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvcy<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvcy<-rep(NA,nrow(CarePolitics11))

# Educational level in Austria
CarePolitics1$edlvat<-rep(NA,nrow(CarePolitics1));CarePolitics2$edlvat<-rep(NA,nrow(CarePolitics2))
CarePolitics3$edlvat<-rep(NA,nrow(CarePolitics3));CarePolitics4$edlvat<-ESS4$edlvat
CarePolitics5$edlvat<-rep(NA,nrow(CarePolitics5));CarePolitics6$edlvat<-rep(NA,nrow(CarePolitics6))
CarePolitics7$edlvat<-rep(NA,nrow(CarePolitics7));CarePolitics8$edlvat<-rep(NA,nrow(CarePolitics8))
CarePolitics9$edlvat<-rep(NA,nrow(CarePolitics9));CarePolitics10$edlvat<-rep(NA,nrow(CarePolitics10))
CarePolitics11$edlvat<-rep(NA,nrow(CarePolitics11))

#######

# Main activity in last 7 days: all respondents respondents (mainact)
CarePolitics1$mnactic<-ESS1$mnactic;CarePolitics2$mnactic<-ESS2$mnactic;CarePolitics3$mnactic<-ESS3$mnactic
CarePolitics4$mnactic<-ESS4$mnactic;CarePolitics5$mnactic<-ESS5$mnactic;CarePolitics6$mnactic<-ESS6$mnactic
CarePolitics7$mnactic<-ESS7$mnactic;CarePolitics8$mnactic<-ESS8$mnactic;CarePolitics9$mnactic<-ESS9$mnactic
CarePolitics10$mnactic<-ESS10$mnactic;CarePolitics11$mnactic<-ESS11$mnactic

# Main activity in last 7 days: only employed respondents (mainact)
CarePolitics1$mainact<-ESS1$mainact;CarePolitics2$mainact<-ESS2$mainact;CarePolitics3$mainact<-ESS3$mainact
CarePolitics4$mainact<-ESS4$mainact;CarePolitics5$mainact<-ESS5$mainact;CarePolitics6$mainact<-ESS6$mainact
CarePolitics7$mainact<-ESS7$mainact;CarePolitics8$mainact<-ESS8$mainact;CarePolitics9$mainact<-ESS9$mainact
CarePolitics10$mainact<-ESS10$mainact;CarePolitics11$mainact<-ESS11$mainact

# Paid work in last 7 days (pdwrk)
CarePolitics1$pdwrk<-ESS1$pdwrk;CarePolitics2$pdwrk<-ESS2$pdwrk;CarePolitics3$pdwrk<-ESS3$pdwrk
CarePolitics4$pdwrk<-ESS4$pdwrk;CarePolitics5$pdwrk<-ESS5$pdwrk;CarePolitics6$pdwrk<-ESS6$pdwrk
CarePolitics7$pdwrk<-ESS7$pdwrk;CarePolitics8$pdwrk<-ESS8$pdwrk;CarePolitics9$pdwrk<-ESS9$pdwrk
CarePolitics10$pdwrk<-ESS10$pdwrk;CarePolitics11$pdwrk<-ESS11$pdwrk

# Main source of HH income (hincsrca)
CarePolitics1$hincsrca<-ESS1$hincsrc;CarePolitics2$hincsrca<-ESS2$hincsrca;CarePolitics3$hincsrca<-ESS3$hincsrca
CarePolitics4$hincsrca<-ESS4$hincsrca;CarePolitics5$hincsrca<-ESS5$hincsrca;CarePolitics6$hincsrca<-ESS6$hincsrca
CarePolitics7$hincsrca<-ESS7$hincsrca;CarePolitics8$hincsrca<-ESS8$hincsrca;CarePolitics9$hincsrca<-ESS9$hincsrca
CarePolitics10$hincsrca<-ESS10$hincsrca;CarePolitics11$hincsrca<-ESS11$hincsrca

# Net household income (hinctnt): different groupings in Waves 1-3 and Waves 4-11
CarePolitics1$hinctnt<-ESS1$hinctnt;CarePolitics2$hinctnt<-ESS2$hinctnt;CarePolitics3$hinctnt<-ESS3$hinctnt
CarePolitics4$hinctnt<-ESS4$hinctnta;CarePolitics5$hinctnt<-ESS5$hinctnta;CarePolitics6$hinctnt<-ESS6$hinctnta
CarePolitics7$hinctnt<-ESS7$hinctnta;CarePolitics8$hinctnt<-ESS8$hinctnta;CarePolitics9$hinctnt<-ESS9$hinctnta
CarePolitics10$hinctnt<-ESS10$hinctnta;CarePolitics11$hinctnt<-ESS11$hinctnta

# Another variable harmonising (cateegories H, U and N are collapsed in Waves 1-3)
CarePolitics4$hinctnta<-ESS4$hinctnta;CarePolitics5$hinctnta<-ESS5$hinctnta;CarePolitics6$hinctnta<-ESS6$hinctnta
CarePolitics7$hinctnta<-ESS7$hinctnta;CarePolitics8$hinctnta<-ESS8$hinctnta;CarePolitics9$hinctnta<-ESS9$hinctnta
CarePolitics10$hinctnta<-ESS10$hinctnta;CarePolitics11$hinctnta<-ESS11$hinctnta

CarePolitics1$hinctnta<-recode_factor(CarePolitics1$hinctnt,"U"="H","N"="H")
levels(CarePolitics1$hinctnta)<-levels(CarePolitics4$hinctnta)
CarePolitics2$hinctnta<-recode_factor(CarePolitics2$hinctnt,"U"="H","N"="H")
levels(CarePolitics2$hinctnta)<-levels(CarePolitics4$hinctnta)
CarePolitics3$hinctnta<-recode_factor(CarePolitics3$hinctnt,"U"="H","N"="H")
levels(CarePolitics3$hinctnta)<-levels(CarePolitics4$hinctnta)

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

## Health and disability
# Subjective general health (health)
CarePolitics1$health<-ESS1$health;CarePolitics2$health<-ESS2$health;CarePolitics3$health<-ESS3$health
CarePolitics4$health<-ESS4$health;CarePolitics5$health<-ESS5$health;CarePolitics6$health<-ESS6$health
CarePolitics7$health<-ESS7$health;CarePolitics8$health<-ESS8$health;CarePolitics9$health<-ESS9$health
CarePolitics10$health<-ESS10$health;CarePolitics11$health<-ESS11$health

# Hampered in daily activities by illness/disability/infirmity (hlthhmp)
CarePolitics1$hlthhmp<-ESS1$hlthhmp;CarePolitics2$hlthhmp<-ESS2$hlthhmp;CarePolitics3$hlthhmp<-ESS3$hlthhmp
CarePolitics4$hlthhmp<-ESS4$hlthhmp;CarePolitics5$hlthhmp<-ESS5$hlthhmp;CarePolitics6$hlthhmp<-ESS6$hlthhmp
CarePolitics7$hlthhmp<-ESS7$hlthhmp;CarePolitics8$hlthhmp<-ESS8$hlthhmp;CarePolitics9$hlthhmp<-ESS9$hlthhmp
CarePolitics10$hlthhmp<-ESS10$hlthhmp;CarePolitics11$hlthhmp<-ESS11$hlthhmp

# Permanently sick or disabled in last 7 days (dsbld)
CarePolitics1$dsbld<-ESS1$dsbld;CarePolitics2$dsbld<-ESS2$dsbld;CarePolitics3$dsbld<-ESS3$dsbld
CarePolitics4$dsbld<-ESS4$dsbld;CarePolitics5$dsbld<-ESS5$dsbld;CarePolitics6$dsbld<-ESS6$dsbld
CarePolitics7$dsbld<-ESS7$dsbld;CarePolitics8$dsbld<-ESS8$dsbld;CarePolitics9$dsbld<-ESS9$dsbld
CarePolitics10$dsbld<-ESS10$dsbld;CarePolitics11$dsbld<-ESS11$dsbld

# Partner permanently sick or disabled in last 7 days (dsbldp)
CarePolitics1$dsbldp<-ESS1$dsbldp;CarePolitics2$dsbldp<-ESS2$dsbldp;CarePolitics3$dsbldp<-ESS3$dsbldp
CarePolitics4$dsbldp<-ESS4$dsbldp;CarePolitics5$dsbldp<-ESS5$dsbldp;CarePolitics6$dsbldp<-ESS6$dsbldp
CarePolitics7$dsbldp<-ESS7$dsbldp;CarePolitics8$dsbldp<-ESS8$dsbldp;CarePolitics9$dsbldp<-ESS9$dsbldp
CarePolitics10$dsbldp<-ESS10$dsbldp;CarePolitics11$dsbldp<-ESS11$dsbldp

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

################################

####
## 4) OBTAIN A POOLED DATASET
###

CarePolitics<-bind_rows(CarePolitics1,CarePolitics2,CarePolitics3,CarePolitics4,CarePolitics5,CarePolitics6,
                        CarePolitics7,CarePolitics8,CarePolitics9,CarePolitics10,CarePolitics11)

#save(list=ls(),file="ESS_AllWaves.RData")
#load("ESS_AllWaves.RData")

###
# 5) CHANGE VARIABLE TYPES AND REORDER/HARMONISE LEVELS WHEN NECESSARY
###

# Turn all refusals, don't knows and no answers into NAs across every variable, and drop empty levels
CarePolitics[] <- lapply(
  CarePolitics,
  function(x) {
    x[x %in% c("Refusal", "Don't know", "No answer","Not applicable")] <- NA
    if (is.factor(x)) x <- droplevels(x)
    x
  }
)

# Change variable types and recode when necessary
CarePolitics$wave<-as.ordered(CarePolitics$wave)

CarePolitics$year<-as.ordered(CarePolitics$year)

CarePolitics$isco08_merged<-as.factor(CarePolitics$isco08_merged)
CarePolitics$isco08_merged<-recode_factor(CarePolitics$isco08_merged,"0"="No equivalence")

CarePolitics$skill_level<-as.ordered(CarePolitics$skill_level)

CarePolitics$polintr<-droplevels(CarePolitics$polintr)
CarePolitics$polintr<-ordered(CarePolitics$polintr,levels=c("Not at all interested","Hardly interested",
                                                            "Quite interested","Very interested"))

CarePolitics$polcmpl<-as.ordered(CarePolitics$polcmpl)

CarePolitics$poldcs<-as.ordered(CarePolitics$poldcs)

CarePolitics$polactiv<-as.ordered(CarePolitics$polactiv)

CarePolitics$pltcare<-as.ordered(CarePolitics$pltcare)

CarePolitics$pltinvt<-as.ordered(CarePolitics$pltinvt)

CarePolitics$ptcpplt<-as.ordered(CarePolitics$ptcpplt)

CarePolitics$actrolg<-as.ordered(CarePolitics$actrolg)

CarePolitics$psppsgv<-as.ordered(CarePolitics$psppsgv)

CarePolitics$psppipl<-as.ordered(CarePolitics$psppipl)

CarePolitics$cptppol<-as.ordered(CarePolitics$cptppol)

CarePolitics$trstplt<-as.ordered(CarePolitics$trstplt)

CarePolitics$trstep<-as.ordered(CarePolitics$trstep)

CarePolitics$trstprl<-as.ordered(CarePolitics$trstprl)

CarePolitics$trstprt<-as.ordered(CarePolitics$trstprt)

CarePolitics$clsprty<-factor(CarePolitics$clsprty,levels=c("No","Yes"))

CarePolitics$prtdgcl[which(CarePolitics$prtdgcl=="Not applicable")]=NA
CarePolitics$prtdgcl<-as.ordered(factor(droplevels(CarePolitics$prtdgcl),levels=c("Not at all close",
                                                                      "Not close",
                                                                      "Quite close",
                                                                      "Very close")))

CarePolitics$lrscale<-as.ordered(CarePolitics$lrscale)

CarePolitics$stfeco<-as.ordered(CarePolitics$stfeco)
CarePolitics$stfdem<-as.ordered(CarePolitics$stfdem)
CarePolitics$stfgov<-as.ordered(CarePolitics$stfgov)
CarePolitics$stfhlth<-as.ordered(CarePolitics$stfhlth)

CarePolitics$gincdif<-ordered(CarePolitics$gincdif,levels=c("Disagree strongly","Disagree",
                                                            "Neither agree nor disagree",
                                                            "Agree","Agree strongly"))

CarePolitics$imbgeco<-as.ordered(CarePolitics$imbgeco)

CarePolitics$mnrgtjb<-ordered(CarePolitics$mnrgtjb,levels=c("Disagree strongly","Disagree",
                                                            "Neither agree nor disagree",
                                                            "Agree","Agree strongly"))

CarePolitics$eqpaybg<-as.ordered(CarePolitics$eqpaybg)

CarePolitics$freinsw<-ordered(CarePolitics$freinsw,levels=c("Strongly against","Somewhat against",
                                                            "Neither in favour nor against",
                                                            "Somewhat in favour","Strongly in favour"))

CarePolitics$fineqpy<-ordered(CarePolitics$fineqpy,levels=c("Strongly against","Somewhat against",
                                                            "Neither in favour nor against",
                                                            "Somewhat in favour","Strongly in favour"))

CarePolitics$vote<-factor(CarePolitics$vote,levels=c("No","Yes","Not eligible to vote"))

CarePolitics$contplt<-factor(CarePolitics$contplt,levels=c("No","Yes"))
CarePolitics$wrkprty<-factor(CarePolitics$wrkprty,levels=c("No","Yes"))
CarePolitics$wrkorg<-factor(CarePolitics$wrkorg,levels=c("No","Yes"))
CarePolitics$badge<-factor(CarePolitics$badge,levels=c("No","Yes"))
CarePolitics$sgnptit<-factor(CarePolitics$sgnptit,levels=c("No","Yes"))
CarePolitics$pbldmn<-factor(CarePolitics$pbldmn,levels=c("No","Yes"))
CarePolitics$pstplonl<-factor(CarePolitics$pstplonl,levels=c("No","Yes"))

CarePolitics$mbtru<-factor(CarePolitics$mbtru,levels=c("No","Yes, currently","Yes, previously"))

CarePolitics$wrkctra<-factor(CarePolitics$wrkctra,levels=c("Unlimited","Limited","No contract","Not applicable"))
CarePolitics$wrkctra<-droplevels(CarePolitics$wrkctra)

CarePolitics$wkhct<-as.numeric(paste(CarePolitics$wkhct))
CarePolitics$wkhct[which(CarePolitics$wkhct>168)]=NA

CarePolitics$wkhtot<-as.numeric(paste(CarePolitics$wkhtot))
CarePolitics$wkhtot[which(CarePolitics$wkhtot>168)]=NA

CarePolitics$tporgwk<-recode_factor(CarePolitics$tporgwk,"Self employed"="Self-employed")
CarePolitics$tporgwk<-factor(CarePolitics$tporgwk,levels=c("Central or local government",
                                                           "Other public sector (such as education and health)",
                                                           "A state owned enterprise",
                                                           "A private firm",
                                                           "Self-employed",
                                                           "Other","Not applicable"))

CarePolitics$sector<-recode_factor(CarePolitics$tporgwk,"Central or local government"="Public",
                                   "Other public sector (such as education and health)"="Public",
                                   "A state owned enterprise"="Public",
                                   "A private firm"="Private",
                                   "Self-employed"="Self-employed",
                                   "Other"="Other")
CarePolitics$sector<-droplevels(CarePolitics$sector)

CarePolitics$estsz<-as.ordered(CarePolitics$estsz)

CarePolitics$jbspv<-factor(CarePolitics$jbspv,levels=c("No","Yes"))

CarePolitics$njbspv<-as.numeric(paste(CarePolitics$njbspv))

CarePolitics$wkdcorga<-recode_factor(CarePolitics$wkdcorga,
                                     "I have no influence"="No influence",
                                     "I have/had no influence"="No influence",
                                     "I have complete control"="Complete control",
                                     "I have/had complete control"="Complete control")
CarePolitics$wkdcorga<-ordered(CarePolitics$wkdcorga,levels=c("No influence","1","2","3","4","5","6","7","8","9",
                                                              "Complete control"))

CarePolitics$iorgact<-recode_factor(CarePolitics$iorgact,
                                    "I have/had no influence"="No influence",
                                    "I have/had complete control"="Complete control")
CarePolitics$iorgact<-ordered(CarePolitics$iorgact,levels=c("No influence","1","2","3","4","5","6","7","8","9",
                                                              "Complete control"))

CarePolitics$trwrkmw<-factor(CarePolitics$trwrkmw,levels=c("No","Yes, once", "Yes, more than once",
                                                           "Have never had job or applied for a job"))

CarePolitics$rlgblg<-factor(CarePolitics$rlgblg,levels=c("No","Yes"))

CarePolitics$rlgdnm<-recode_factor(CarePolitics$rlgdnm,"Other non-Christian religions"="Other Non-Christian religions")

CarePolitics$rlgdgr<-as.ordered(CarePolitics$rlgdgr)

CarePolitics$rlgatnd<-as.ordered(factor(CarePolitics$rlgatnd,levels=c("Never","Less often","Only on special holy days",
                                                           "At least once a month","Once a week",
                                                           "More than once a week", "Every day")))

CarePolitics$dscrgrp<-factor(CarePolitics$dscrgrp,levels=c("No","Yes"))
CarePolitics$ctzcntr<-factor(CarePolitics$ctzcntr,levels=c("No","Yes"))
CarePolitics$brncntr<-factor(CarePolitics$brncntr,levels=c("No","Yes"))

CarePolitics$livecnta<-as.numeric(paste(CarePolitics$livecnta))

CarePolitics$livecntr<-as.ordered(CarePolitics$livecntr)

CarePolitics$blgetmg<-factor(CarePolitics$blgetmg,levels=c("No","Yes"))

CarePolitics$agea<-as.numeric(paste(CarePolitics$agea))
CarePolitics$hhmmb<-as.numeric(paste(CarePolitics$hhmmb))

CarePolitics$yrbrn2<-as.numeric(paste(CarePolitics$yrbrn2))
CarePolitics$yrbrn3<-as.numeric(paste(CarePolitics$yrbrn3))
CarePolitics$yrbrn4<-as.numeric(paste(CarePolitics$yrbrn4))
CarePolitics$yrbrn5<-as.numeric(paste(CarePolitics$yrbrn5))
CarePolitics$yrbrn6<-as.numeric(paste(CarePolitics$yrbrn6))
CarePolitics$yrbrn7<-as.numeric(paste(CarePolitics$yrbrn7))
CarePolitics$yrbrn8<-as.numeric(paste(CarePolitics$yrbrn8))
CarePolitics$yrbrn9<-as.numeric(paste(CarePolitics$yrbrn9))
CarePolitics$yrbrn10<-as.numeric(paste(CarePolitics$yrbrn10))
CarePolitics$yrbrn11<-as.numeric(paste(CarePolitics$yrbrn11))
CarePolitics$yrbrn12<-as.numeric(paste(CarePolitics$yrbrn12))

CarePolitics$yrbrn2[which(CarePolitics$yrbrn2<1900)]=NA
CarePolitics$yrbrn3[which(CarePolitics$yrbrn3<1900)]=NA
CarePolitics$yrbrn4[which(CarePolitics$yrbrn4<1900)]=NA
CarePolitics$yrbrn5[which(CarePolitics$yrbrn5<1900)]=NA
CarePolitics$yrbrn6[which(CarePolitics$yrbrn6<1900)]=NA
CarePolitics$yrbrn7[which(CarePolitics$yrbrn7<1900)]=NA
CarePolitics$yrbrn8[which(CarePolitics$yrbrn8<1900)]=NA
CarePolitics$yrbrn9[which(CarePolitics$yrbrn9<1900)]=NA
CarePolitics$yrbrn10[which(CarePolitics$yrbrn10<1900)]=NA
CarePolitics$yrbrn11[which(CarePolitics$yrbrn11<1900)]=NA
CarePolitics$yrbrn12[which(CarePolitics$yrbrn12<1900)]=NA

CarePolitics$rshipa2<-recode_factor(CarePolitics$rshipa2,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa3<-recode_factor(CarePolitics$rshipa3,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa4<-recode_factor(CarePolitics$rshipa4,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa5<-recode_factor(CarePolitics$rshipa5,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa6<-recode_factor(CarePolitics$rshipa6,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa7<-recode_factor(CarePolitics$rshipa7,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa8<-recode_factor(CarePolitics$rshipa8,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa9<-recode_factor(CarePolitics$rshipa9,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa10<-recode_factor(CarePolitics$rshipa10,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa11<-recode_factor(CarePolitics$rshipa11,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")
CarePolitics$rshipa12<-recode_factor(CarePolitics$rshipa12,"Son/daughter/step/adopted"="Son/daughter/step/adopted/foster")

# Lives with partner
CarePolitics$lwp<-recode_factor(CarePolitics$lwp,"Does not"="Does not",
                                "Lives with husband/wife/partner at F4"="Lives with husband/wife/partner",
                                "Respondent lives with husband/wife/partner"="Lives with husband/wife/partner")
CarePolitics$lwp[which(CarePolitics$lwp %in% c("Not available","All others"))]=NA
CarePolitics$lwp<-droplevels(CarePolitics$lwp)

# Marital status
CarePolitics$marsts<-recode_factor(CarePolitics$marsts,
                                   "Married"="Married or in civil partnership",
                                   "Separated"="Separated (still legally married or in civil partnership)",
                                   "Divorced"="No longer married or in civil partnership",
                                   "Widowed"="Widowed/civil partner died",
                                   "Never married"="Never married or in civil partnership",
                                   "In a civil partnership"="Married or in civil partnership",
                                   "Separated (still legally married)"="Separated (still legally married or in civil partnership)",
                                   "Separated (still in a civil partnership)"="Separated (still legally married or in civil partnership)",
                                   "Formerly in civil partnership, now dissolved"="No longer married or in civil partnership",
                                   "Formerly in civil partnership, partner died"="Widowed/civil partner died",
                                   "Never married and never in civil partnership"="Never married or in civil partnership",
                                   "Legally married"="Married or in civil partnership",
                                   "In a legally registered civil union"="Married or in civil partnership",
                                   "Legally separated"="No longer married or in civil partnership",
                                   "Legally divorced/Civil union dissolved"="No longer married or in civil partnership",
                                   "Widowed/Civil partner died"="Widowed/civil partner died",
                                   "None of these (NEVER married or in legally registered civil union)"="Never married or in civil partnership",
                                   "Legally divorced/civil union dissolved"="No longer married or in civil partnership",
                                   "Widowed/civil partner died"="Widowed/civil partner died")
CarePolitics$marsts<-droplevels(CarePolitics$marsts)

# Children at home (too many missings)
CarePolitics$chldhm<-recode_factor(CarePolitics$chldhm,"Does not"="Does not",
                                   "Respondent lives with children at household grid"="Lives with children")
CarePolitics$chldhm[which(CarePolitics$chldhm=="Not available")]=NA
CarePolitics$chldhm<-droplevels(CarePolitics$chldhm)

# Number of own children in HH
CarePolitics$nkids<-numeric(nrow(CarePolitics))
for (i in 1:nrow(CarePolitics)){
  CarePolitics$nkids[i]<-length(which(c(CarePolitics$rshipa2[i],CarePolitics$rshipa3[i],CarePolitics$rshipa4[i],
                                        CarePolitics$rshipa5[i],CarePolitics$rshipa6[i],CarePolitics$rshipa7[i],
                                        CarePolitics$rshipa8[i],CarePolitics$rshipa9[i],CarePolitics$rshipa10[i],
                                        CarePolitics$rshipa11[i])=="Son/daughter/step/adopted/foster"))
}

CarePolitics$hinctnt[which(as.numeric(paste(CarePolitics$wave))>3)]=NA
CarePolitics$hinctnt<-droplevels(CarePolitics$hinctnt)
CarePolitics$hinctnt<-as.ordered(CarePolitics$hinctnt)
CarePolitics$hinctnta<-as.ordered(CarePolitics$hinctnta)

CarePolitics$health<-ordered(CarePolitics$health,levels=c("Very bad","Bad","Fair","Good","Very good"))

CarePolitics$hlthhmp<-factor(CarePolitics$hlthhmp,levels=c("No","Yes to some extent","Yes a lot"))

#save(list=ls(),file="ESS_AllWaves.RData")

###
# 6) ADD SOME A POSTERIORI VARIABLES (THESE ARE VARIABLES THAT CAME UP DURING THE ANALYSES)
###

# Broader occupational categories: service workers and working class
CarePolitics$isco88_num<-as.numeric(paste(CarePolitics$isco88))
CarePolitics$isco08_num<-as.numeric(paste(CarePolitics$isco08))

CarePolitics$isco88_merged<-numeric(nrow(CarePolitics))

CarePolitics$isco88_merged[which(as.numeric(paste(CarePolitics$wave))<=5)]=CarePolitics$isco88_num[which(as.numeric(paste(CarePolitics$wave))<=5)]
  
library(ISCO08ConveRsions)

for (i in which(as.numeric(paste(CarePolitics$wave))>5)){
  if (is.na(CarePolitics$isco08[i])){
    CarePolitics$isco88_merged[i]=NA}
  else{
    CarePolitics$isco88_merged[i]<-try(isco08toisco88(paste(CarePolitics$isco08[i])),silent=T) # Use of try to ignore errors
  }}

CarePolitics$isco88_merged[which(CarePolitics$isco88_merged=="Error in isco08toisco88(paste(CarePolitics$isco08[i])) : \n  Invalid ISCO08-Code\n")]="100"
# The few error cases correspond to the isco08=0 (Armed Forces Occupations) category. All are recoded to "100"
CarePolitics$isco88_merged<-droplevels(as.factor(CarePolitics$isco88_merged))

CarePolitics$servicework<-factor(nrow(CarePolitics),levels=c("No","Yes")) # Is respondent a service worker?
# Using the operational definitions in Rkesi and Oesch (2003):
#isco88: 3221, 3225, 3227 - 3228,  5122, 5141, 5143, 5110 5113, 5150 - 5163, 5200 5210, 8323
# 5120 - 5121, 5123 - 5130, 5131 - 5140, 5142, 5149,  5169, 5220 - 5230, 8320 8322, 9100 - 9153
CarePolitics$servicework[-which(CarePolitics$isco88_merged %in% c("3221","3225","3227","3228","5100",
                                                                  "5122","5141","5143","5110","5111","5112","5113",
                                                                  "5152","5160","5161","5162","5163",
                                                                  "5200","5210","8323", # Until here: skilled service
                                                                  "5120","5121","5123", # From here: routine service
                                                                  "5130","5131","5132","5133","5139","5140",
                                                                  "5142","5149","5169","5220","5230",
                                                                  "8320","8321","8322",
                                                                  "9100","9110","9111","9112","9113",
                                                                  "9120","9130","9131","9132","9133",
                                                                  "9140","9141","9142","9150","9151","9152",'9153'))]="No"
CarePolitics$servicework[which(CarePolitics$isco88_merged %in% c("3221","3225","3227","3228","5100",
                                                                 "5122","5141","5143","5110","5111","5112","5113",
                                                                 "5152","5160","5161","5162","5163",
                                                                 "5200","5210","8323", # Until here: skilled service
                                                                 "5120","5121","5123", # From here: routine service
                                                                 "5130","5131","5132","5133","5139","5140",
                                                                 "5142","5149","5169","5220","5230",
                                                                 "8320","8321","8322",
                                                                 "9100","9110","9111","9112","9113",
                                                                 "9120","9130","9131","9132","9133",
                                                                 "9140","9141","9142","9150","9151","9152",'9153'))]="Yes"
CarePolitics$servicework[which(is.na(CarePolitics$isco88_merged))]=NA


CarePolitics$bluecollar<-factor(nrow(CarePolitics),levels=c("No","Yes")) # Is respondent a blue collar worker
# Using the operational definitions in Rkesi and Oesch (2003):
#isco88: 7120-7142,7200-7233,7240-7423,7430-7520,8311,8324,8333,
# 7100-7113, 7129-7130, 7143, 7234, 7424, 8000-8310, 8312, 8334-8400, 9160-9162,9300-9333
# 6100-6210, 8330-8332, 9200-9213
CarePolitics$bluecollar[-which(CarePolitics$isco88_merged %in% c("7120","7121","7122","7123","7124","7129","7130",
                                                                 "7131","7132","7133","7134","7135","7136","7137",
                                                                 "7138","7139","7140","7142","7142",
                                                                 "7200","7210","7211","7212","7213","7214","7215", 
                                                                 "7216","7220","7221","7222","7223","7224","7230", 
                                                                 "7231","7232","7233",
                                                                 "7240","7241","7242","7243","7244","7245", 
                                                                 "7300","7310","7311","7312","7313","7320","7321",
                                                                 "7322","7323","7324","7330","7331","7332",
                                                                 "7340","7341","7342","7343","7344","7345","7346",
                                                                 "7400","7410","7411","7412","7413","7414","7415",
                                                                 "7416","7420","7421","7422","7423",
                                                                 "7430","7431","7432","7433","7434","7435","7436",
                                                                 "7437","7440","7441","7442",
                                                                 "8311","8324","8333",# Until here: Skilled crafts
                                                                 "7100","7110","7111","7112","7113",# From here: routine operatives
                                                                 "7129","7130","7143","7234","7424",
                                                                 "8000","8100","8110","8111","8112","8113","8120",
                                                                 "8121","8122","8123","8124","8130","8131","8139",
                                                                 "8140","8141","8142","8143","8150","8151","8152", 
                                                                 "8153","8154","8155","8159","8160","8161","8162",
                                                                 "8163","8170","8200","8210","8211",
                                                                 "8212","8220","8221","8222","8223","8224","8229",
                                                                 "8230","8231","8232","8240","8250","8251","8252",
                                                                 "8253","8260","8261","8262","8263","8264","8265",
                                                                 "8266","8269","8270","8271","8272","8273","8274", 
                                                                 "8275","8276","8277","8278","8279","8280","8281", 
                                                                 "8282","8283","8284","8285","8286","8287","8290",
                                                                 "8300","8310","8312","8334","8340",
                                                                 "9160","9161","9162","9300","9310","9311","9312", 
                                                                 "9313","9320","9322","9330","9331","9332","9333",
                                                                 # From here: routine agriculture
                                                                 "6100","6110","6111","6112","6113","6114","6120",
                                                                 "6121","6122","6123","6124","6129","6130","6140",
                                                                 "6141","6142","6150","6151","6152","6153",
                                                                 "6154","6200","6210",
                                                                 "8330","8331","8332",
                                                                 "9200","9210","9211","9212","9213"))]="No"

CarePolitics$bluecollar[which(CarePolitics$isco88_merged %in% c("7120","7121","7122","7123","7124","7129","7130",
                                                                 "7131","7132","7133","7134","7135","7136","7137",
                                                                 "7138","7139","7140","7142","7142",
                                                                 "7200","7210","7211","7212","7213","7214","7215", 
                                                                 "7216","7220","7221","7222","7223","7224","7230", 
                                                                 "7231","7232","7233",
                                                                 "7240","7241","7242","7243","7244","7245", 
                                                                 "7300","7310","7311","7312","7313","7320","7321",
                                                                 "7322","7323","7324","7330","7331","7332",
                                                                 "7340","7341","7342","7343","7344","7345","7346",
                                                                 "7400","7410","7411","7412","7413","7414","7415",
                                                                 "7416","7420","7421","7422","7423",
                                                                 "7430","7431","7432","7433","7434","7435","7436",
                                                                 "7437","7440","7441","7442",
                                                                 "8311","8324","8333",# Until here: Skilled crafts
                                                                 "7100","7110","7111","7112","7113",# From here: routine operatives
                                                                 "7129","7130","7143","7234","7424",
                                                                 "8000","8100","8110","8111","8112","8113","8120",
                                                                 "8121","8122","8123","8124","8130","8131","8139",
                                                                 "8140","8141","8142","8143","8150","8151","8152", 
                                                                 "8153","8154","8155","8159","8160","8161","8162",
                                                                 "8163","8170","8200","8210","8211",
                                                                 "8212","8220","8221","8222","8223","8224","8229",
                                                                 "8230","8231","8232","8240","8250","8251","8252",
                                                                 "8253","8260","8261","8262","8263","8264","8265",
                                                                 "8266","8269","8270","8271","8272","8273","8274", 
                                                                 "8275","8276","8277","8278","8279","8280","8281", 
                                                                 "8282","8283","8284","8285","8286","8287","8290",
                                                                 "8300","8310","8312","8334","8340",
                                                                 "9160","9161","9162","9300","9310","9311","9312", 
                                                                 "9313","9320","9322","9330","9331","9332","9333",
                                                                 # From here: routine agriculture
                                                                 "6100","6110","6111","6112","6113","6114","6120",
                                                                 "6121","6122","6123","6124","6129","6130","6140",
                                                                 "6141","6142","6150","6151","6152","6153",
                                                                 "6154","6200","6210",
                                                                 "8330","8331","8332",
                                                                 "9200","9210","9211","9212","9213"))]="Yes"
CarePolitics$bluecollar[which(is.na(CarePolitics$isco88_merged))]=NA

# Fix an issue with care workers category
CarePolitics$pcareworker<-factor(nrow(CarePolitics),levels=c("No","Yes"))
CarePolitics$pcareworker[-which(CarePolitics$isco88_merged %in% c("5130","5131","5132","5133"))]="No"
CarePolitics$pcareworker[which(CarePolitics$isco88_merged %in% c("5130","5131","5132","5133"))]="Yes"
CarePolitics$pcareworker[which(is.na(CarePolitics$isco88_merged))]=NA

# Fix an issue with the nurses category
CarePolitics$nurse[which(CarePolitics$isco88 %in% c("3231","3232"))]="Yes"

# Child care workers (isco08: 5311, isco88: 5131)
CarePolitics$childcare<-factor(nrow(CarePolitics),levels=c("No","Yes"))

CarePolitics$childcare[-which(CarePolitics$isco88=="5131")]="No"
CarePolitics$childcare[-which(CarePolitics$isco08=="5311")]="No"
CarePolitics$childcare[which(CarePolitics$isco88=="5131")]="Yes"
CarePolitics$childcare[which(CarePolitics$isco08=="5311")]="Yes"
CarePolitics$childcare[which(is.na(CarePolitics$isco88) & as.numeric(paste(CarePolitics$wave))<=5)]=NA
CarePolitics$childcare[which(is.na(CarePolitics$isco08) & as.numeric(paste(CarePolitics$wave))>=6)]=NA

# Institution-based care workers (isco08: 5321, isco88: 5132)
CarePolitics$instcare<-factor(nrow(CarePolitics),levels=c("No","Yes"))

CarePolitics$instcare[-which(CarePolitics$isco88=="5132")]="No"
CarePolitics$instcare[-which(CarePolitics$isco08=="5321")]="No"
CarePolitics$instcare[which(CarePolitics$isco88=="5132")]="Yes"
CarePolitics$instcare[which(CarePolitics$isco08=="5321")]="Yes"
CarePolitics$instcare[which(is.na(CarePolitics$isco88) & as.numeric(paste(CarePolitics$wave))<=5)]=NA
CarePolitics$instcare[which(is.na(CarePolitics$isco08) & as.numeric(paste(CarePolitics$wave))>=6)]=NA

# Home-based care workers (isco08: 5322, isco88: 5133)
CarePolitics$homebcare<-factor(nrow(CarePolitics),levels=c("No","Yes"))

CarePolitics$homebcare[-which(CarePolitics$isco88=="5133")]="No"
CarePolitics$homebcare[-which(CarePolitics$isco08=="5322")]="No"
CarePolitics$homebcare[which(CarePolitics$isco88=="5133")]="Yes"
CarePolitics$homebcare[which(CarePolitics$isco08=="5322")]="Yes"
CarePolitics$homebcare[which(is.na(CarePolitics$isco88) & as.numeric(paste(CarePolitics$wave))<=5)]=NA
CarePolitics$homebcare[which(is.na(CarePolitics$isco08) & as.numeric(paste(CarePolitics$wave))>=6)]=NA

# Other main activity categories
CarePolitics$edctn<-factor(nrow(CarePolitics),levels=c("Not marked","Marked"))
CarePolitics$edctn[which(CarePolitics$wave=="1")]<-ESS1$edctn
CarePolitics$edctn[which(CarePolitics$wave=="2")]<-ESS2$edctn
CarePolitics$edctn[which(CarePolitics$wave=="3")]<-ESS3$edctn
CarePolitics$edctn[which(CarePolitics$wave=="4")]<-ESS4$edctn
CarePolitics$edctn[which(CarePolitics$wave=="5")]<-ESS5$edctn
CarePolitics$edctn[which(CarePolitics$wave=="6")]<-ESS6$edctn
CarePolitics$edctn[which(CarePolitics$wave=="7")]<-ESS7$edctn
CarePolitics$edctn[which(CarePolitics$wave=="8")]<-ESS8$edctn
CarePolitics$edctn[which(CarePolitics$wave=="9")]<-ESS9$edctn
CarePolitics$edctn[which(CarePolitics$wave=="10")]<-ESS10$edctn
CarePolitics$edctn[which(CarePolitics$wave=="11")]<-ESS11$edctn

CarePolitics$uempla<-factor(nrow(CarePolitics),levels=c("Not marked","Marked"))
CarePolitics$uempla[which(CarePolitics$wave=="1")]<-ESS1$uempla
CarePolitics$uempla[which(CarePolitics$wave=="2")]<-ESS2$uempla
CarePolitics$uempla[which(CarePolitics$wave=="3")]<-ESS3$uempla
CarePolitics$uempla[which(CarePolitics$wave=="4")]<-ESS4$uempla
CarePolitics$uempla[which(CarePolitics$wave=="5")]<-ESS5$uempla
CarePolitics$uempla[which(CarePolitics$wave=="6")]<-ESS6$uempla
CarePolitics$uempla[which(CarePolitics$wave=="7")]<-ESS7$uempla
CarePolitics$uempla[which(CarePolitics$wave=="8")]<-ESS8$uempla
CarePolitics$uempla[which(CarePolitics$wave=="9")]<-ESS9$uempla
CarePolitics$uempla[which(CarePolitics$wave=="10")]<-ESS10$uempla
CarePolitics$uempla[which(CarePolitics$wave=="11")]<-ESS11$uempla

CarePolitics$uempli<-factor(nrow(CarePolitics),levels=c("Not marked","Marked"))
CarePolitics$uempli[which(CarePolitics$wave=="1")]<-ESS1$uempli
CarePolitics$uempli[which(CarePolitics$wave=="2")]<-ESS2$uempli
CarePolitics$uempli[which(CarePolitics$wave=="3")]<-ESS3$uempli
CarePolitics$uempli[which(CarePolitics$wave=="4")]<-ESS4$uempli
CarePolitics$uempli[which(CarePolitics$wave=="5")]<-ESS5$uempli
CarePolitics$uempli[which(CarePolitics$wave=="6")]<-ESS6$uempli
CarePolitics$uempli[which(CarePolitics$wave=="7")]<-ESS7$uempli
CarePolitics$uempli[which(CarePolitics$wave=="8")]<-ESS8$uempli
CarePolitics$uempli[which(CarePolitics$wave=="9")]<-ESS9$uempli
CarePolitics$uempli[which(CarePolitics$wave=="10")]<-ESS10$uempli
CarePolitics$uempli[which(CarePolitics$wave=="11")]<-ESS11$uempli

CarePolitics$dsbld<-factor(nrow(CarePolitics),levels=c("Not marked","Marked"))
CarePolitics$dsbld[which(CarePolitics$wave=="1")]<-ESS1$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="2")]<-ESS2$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="3")]<-ESS3$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="4")]<-ESS4$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="5")]<-ESS5$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="6")]<-ESS6$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="7")]<-ESS7$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="8")]<-ESS8$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="9")]<-ESS9$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="10")]<-ESS10$dsbld
CarePolitics$dsbld[which(CarePolitics$wave=="11")]<-ESS11$dsbld

CarePolitics$rtrd<-factor(nrow(CarePolitics),levels=c("Not marked","Marked"))
CarePolitics$rtrd[which(CarePolitics$wave=="1")]<-ESS1$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="2")]<-ESS2$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="3")]<-ESS3$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="4")]<-ESS4$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="5")]<-ESS5$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="6")]<-ESS6$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="7")]<-ESS7$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="8")]<-ESS8$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="9")]<-ESS9$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="10")]<-ESS10$rtrd
CarePolitics$rtrd[which(CarePolitics$wave=="11")]<-ESS11$rtrd

CarePolitics$cmsrv<-factor(nrow(CarePolitics),levels=c("Not marked","Marked"))
CarePolitics$cmsrv[which(CarePolitics$wave=="1")]<-ESS1$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="2")]<-ESS2$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="3")]<-ESS3$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="4")]<-ESS4$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="5")]<-ESS5$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="6")]<-ESS6$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="7")]<-ESS7$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="8")]<-ESS8$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="9")]<-ESS9$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="10")]<-ESS10$cmsrv
CarePolitics$cmsrv[which(CarePolitics$wave=="11")]<-ESS11$cmsrv

CarePolitics$hswrk<-factor(nrow(CarePolitics),levels=c("Not marked","Marked"))
CarePolitics$hswrk[which(CarePolitics$wave=="1")]<-ESS1$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="2")]<-ESS2$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="3")]<-ESS3$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="4")]<-ESS4$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="5")]<-ESS5$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="6")]<-ESS6$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="7")]<-ESS7$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="8")]<-ESS8$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="9")]<-ESS9$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="10")]<-ESS10$hswrk
CarePolitics$hswrk[which(CarePolitics$wave=="11")]<-ESS11$hswrk

CarePolitics$dngoth<-factor(nrow(CarePolitics),levels=c("Not marked","Marked"))
CarePolitics$dngoth[which(CarePolitics$wave=="1")]<-ESS1$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="2")]<-ESS2$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="3")]<-ESS3$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="4")]<-ESS4$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="5")]<-ESS5$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="6")]<-ESS6$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="7")]<-ESS7$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="8")]<-ESS8$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="9")]<-ESS9$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="10")]<-ESS10$dngoth
CarePolitics$dngoth[which(CarePolitics$wave=="11")]<-ESS11$dngoth

# Number of employees respondent has/had
CarePolitics$emplno<-character(nrow(CarePolitics))
CarePolitics$emplno[which(CarePolitics$wave==1)]=paste(ESS1$emplno)
CarePolitics$emplno[which(CarePolitics$wave==2)]=paste(ESS2$emplno)
CarePolitics$emplno[which(CarePolitics$wave==3)]=paste(ESS3$emplno)
CarePolitics$emplno[which(CarePolitics$wave==4)]=paste(ESS4$emplno)
CarePolitics$emplno[which(CarePolitics$wave==5)]=paste(ESS5$emplno)
CarePolitics$emplno[which(CarePolitics$wave==6)]=paste(ESS6$emplno)
CarePolitics$emplno[which(CarePolitics$wave==7)]=paste(ESS7$emplno)
CarePolitics$emplno[which(CarePolitics$wave==8)]=paste(ESS8$emplno)
CarePolitics$emplno[which(CarePolitics$wave==9)]=paste(ESS9$emplno)
CarePolitics$emplno[which(CarePolitics$wave==10)]=paste(ESS10$emplno)
CarePolitics$emplno[which(CarePolitics$wave==11)]=paste(ESS11$emplno)
CarePolitics$emplno<-as.numeric(paste(CarePolitics$emplno))

# An isco-88 numeric variable to facilitate coding
CarePolitics$isco88_num<-as.numeric(paste(CarePolitics$isco88_merged))

# Create a variable classifying all respondents into an occupational class (following Kitschelt and Rehm, 2007)
CarePolitics$class_kr<-factor(nrow(CarePolitics),levels=c("Large employers","Self-employed professionals",
                                                         "Petty bourgeoisie with employees",
                                                         "Petty bourgeoisie without employees",
                                                         "Technical experts",
                                                         "Technicians","Skilled crafts",
                                                         "Routine operatives","Routine agriculture",
                                                         "Higher-grade managers",
                                                         "Associate managers",
                                                         "Skilled office","Routine office",
                                                         "Socio-cultural professionals",
                                                         "Socio-cultural semi-professionals",
                                                         "Skilled service", "Routine service"))

CarePolitics$class_kr[which(CarePolitics$emplrel=="Self-employed" & CarePolitics$emplno>=10)]="Large employers"
#
CarePolitics$class_kr[which(CarePolitics$emplrel=="Self-employed" & CarePolitics$isco88_num>=2000 &
                           CarePolitics$isco88_num<=2470 & CarePolitics$emplno<10)]="Self-employed professionals"
#
CarePolitics$class_kr[which(CarePolitics$emplrel=="Self-employed" & CarePolitics$isco88_num<2000 &
                              CarePolitics$emplno<10 & CarePolitics$emplno>0)]="Petty bourgeoisie with employees"
CarePolitics$class_kr[which(CarePolitics$emplrel=="Self-employed" & CarePolitics$isco88_num>2470 &
                              CarePolitics$emplno<10 & CarePolitics$emplno>0)]="Petty bourgeoisie with employees"
#
CarePolitics$class_kr[which(CarePolitics$emplrel=="Self-employed" & CarePolitics$isco88_num<2000 &
                              CarePolitics$emplno==0)]="Petty bourgeoisie without employees"
CarePolitics$class_kr[which(CarePolitics$emplrel=="Self-employed" & CarePolitics$isco88_num>2470 &
                              CarePolitics$emplno==0)]="Petty bourgeoisie without employees"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2100 &
                              CarePolitics$isco88_num<=2213)]="Technical experts"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3100 &
                              CarePolitics$isco88_num<=3213)]="Technicians"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num==3471)]="Technicians"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num==100)]="Skilled crafts"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=7120 &
                              CarePolitics$isco88_num<=7142)]="Skilled crafts"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=7200 &
                              CarePolitics$isco88_num<=7233)]="Skilled crafts"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=7240 &
                              CarePolitics$isco88_num<=7423)]="Skilled crafts"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=7430 &
                              CarePolitics$isco88_num<=7520)]="Skilled crafts"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num==8311)]="Skilled crafts"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num==8324)]="Skilled crafts"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num==8333)]="Skilled crafts"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=7100 &
                              CarePolitics$isco88_num<=7113)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=7129 &
                              CarePolitics$isco88_num<=7130)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==7143)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==7234)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==7424)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=8000 &
                              CarePolitics$isco88_num<=8310)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==8312)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=8334 &
                              CarePolitics$isco88_num<=8400)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=9160 &
                              CarePolitics$isco88_num<=9162)]="Routine operatives"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=9300 &
                              CarePolitics$isco88_num<=9333)]="Routine operatives"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=6000 &
                              CarePolitics$isco88_num<=6210)]="Routine agriculture"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=8330 &
                              CarePolitics$isco88_num<=8332)]="Routine agriculture"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=9200 &
                              CarePolitics$isco88_num<=9213)]="Routine agriculture"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=1000 &
                              CarePolitics$isco88_num<=1251)]="Higher-grade managers"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2410 &
                              CarePolitics$isco88_num<=2419)]="Higher-grade managers"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==2441)]="Higher-grade managers"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==2470)]="Higher-grade managers"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=1252 &
                              CarePolitics$isco88_num<=1319)]="Associate managers"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3410 &
                              CarePolitics$isco88_num<=3449)]="Associate managers"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==3452)]="Associate managers"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=4000 &
                              CarePolitics$isco88_num<=4112)]="Skilled office"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=4114 &
                              CarePolitics$isco88_num<=4141)]="Skilled office"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==4143)]="Skilled office"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=4190 &
                              CarePolitics$isco88_num<=4210)]="Skilled office"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=4213 &
                              CarePolitics$isco88_num<=4221)]="Skilled office"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==4113)]="Routine office"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==4142)]="Routine office"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==4144)]="Routine office"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=4211 &
                              CarePolitics$isco88_num<=4212)]="Routine office"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=4222 &
                              CarePolitics$isco88_num<=4223)]="Routine office"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2220 &
                              CarePolitics$isco88_num<=2323)]="Socio-cultural professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2350 &
                              CarePolitics$isco88_num<=2351)]="Socio-cultural professionals" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==2359)]="Socio-cultural professionals" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2420 &
                              CarePolitics$isco88_num<=2440)]="Socio-cultural professionals" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2442 &
                              CarePolitics$isco88_num<=2443)]="Socio-cultural professionals" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==2445)]="Socio-cultural professionals" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==2451)]="Socio-cultural professionals" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==2460)]="Socio-cultural professionals" 
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2330 &
                              CarePolitics$isco88_num<=2340)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==2352)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==2444)]="Socio-cultural semi-professionals"                           
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2446 &
                              CarePolitics$isco88_num<=2450)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=2452 &
                              CarePolitics$isco88_num<=2455)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==3220)]="Socio-cultural semi-professionals"  
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3222 &
                              CarePolitics$isco88_num<=3224)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==3226)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3229 &
                              CarePolitics$isco88_num<=3232)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3240 &
                              CarePolitics$isco88_num<=3400)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3450 &
                              CarePolitics$isco88_num<=3451)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3460 &
                              CarePolitics$isco88_num<=3470)]="Socio-cultural semi-professionals"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3472 &
                              CarePolitics$isco88_num<=3480)]="Socio-cultural semi-professionals"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==3221)]="Skilled service" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==3225)]="Skilled service"  
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=3227 &
                              CarePolitics$isco88_num<=3228)]="Skilled service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==5122)]="Skilled service" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==5141)]="Skilled service" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==5143)]="Skilled service" 
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=5110 &
                              CarePolitics$isco88_num<=5113)]="Skilled service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=5150 &
                              CarePolitics$isco88_num<=5163)]="Skilled service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=5200 &
                              CarePolitics$isco88_num<=5210)]="Skilled service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==8323)]="Skilled service"
#
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=5120 &
                              CarePolitics$isco88_num<=5121)]="Routine service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=5123 &
                              CarePolitics$isco88_num<=5130)]="Routine service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=5131 &
                              CarePolitics$isco88_num<=5140)]="Routine service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==5142)]="Routine service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==5149)]="Routine service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & 
                              CarePolitics$isco88_num==5169)]="Routine service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=5220 &
                              CarePolitics$isco88_num<=5230)]="Routine service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=8320 &
                              CarePolitics$isco88_num<=8322)]="Routine service"
CarePolitics$class_kr[which(CarePolitics$emplrel!="Self-employed" & CarePolitics$isco88_num>=9100 &
                              CarePolitics$isco88_num<=9153)]="Routine service"


# A second class variable using Schwander and Häusermann's (2013) groupings:
CarePolitics$class_sh<-factor(nrow(CarePolitics),levels=c("Capital accumulators","Socio-cultural professionals",
                                                          "Mixed service functionaries","Low service functionaries",
                                                          "Blue collar workers"))
CarePolitics$class_sh[which(CarePolitics$class_kr=="Large employers")]="Capital accumulators"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Self-employed professionals")]="Capital accumulators"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Petty bourgeoisie with employees")]="Capital accumulators"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Technical experts")]="Capital accumulators"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Higher-grade managers")]="Capital accumulators"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Associate managers")]="Capital accumulators"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Petty bourgeoisie without employees")]="Mixed service functionaries"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Technicians")]="Mixed service functionaries"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Skilled office")]="Mixed service functionaries"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Routine office")]="Mixed service functionaries"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Socio-cultural professionals")]="Socio-cultural professionals"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Socio-cultural semi-professionals")]="Socio-cultural professionals"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Skilled crafts")]="Blue collar workers"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Routine operatives")]="Blue collar workers"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Routine agriculture")]="Blue collar workers"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Skilled service")]="Low service functionaries"
CarePolitics$class_sh[which(CarePolitics$class_kr=="Routine service")]="Low service functionaries"

# Add a social workers occupational category
CarePolitics$socialwork<-factor(nrow(CarePolitics),level=c("No","Yes"))
CarePolitics$socialwork[which(CarePolitics$wave %in% c("1","2","3","4","5") &
                              CarePolitics$isco88 %in% c("2446","3460"))]="Yes"
CarePolitics$socialwork[which(CarePolitics$wave %in% c("1","2","3","4","5") &
                          CarePolitics$isco88!="2446" & CarePolitics$isco88!="3460")]="No"
CarePolitics$socialwork[which(CarePolitics$wave %in% c("1","2","3","4","5") &
                          is.na(CarePolitics$isco88))]=NA
CarePolitics$socialwork[which(CarePolitics$wave %in% c("6","7","8","9","10","11") &
                                CarePolitics$isco08 %in% c("2635","3412"))]="Yes"
CarePolitics$socialwork[which(CarePolitics$wave %in% c("6","7","8","9","10","11") &
                                CarePolitics$isco08!="2635" & CarePolitics$isco08!="3412")]="No"
CarePolitics$socialwork[which(CarePolitics$wave %in% c("6","7","8","9","10","11") &
                                is.na(CarePolitics$isco08))]=NA

# Add a teachers category (includes ECE, primary, secondary and special education)
CarePolitics$teacher<-factor(nrow(CarePolitics),level=c("No","Yes"))
CarePolitics$teacher[which(CarePolitics$wave %in% c("1","2","3","4","5") &
                                CarePolitics$isco88 %in% c("232","2320",
                                                           "233","2331","2332","234","2340",
                                                           "331","3310","332","3320","333","3330"))]="Yes"
CarePolitics$teacher[which(CarePolitics$wave %in% c("1","2","3","4","5") &
                             CarePolitics$isco88 %notin% c("232","2320",
                                                        "233","2331","2332","234","2340",
                                                        "331","3310","332","3320","333","3330"))]="No"
CarePolitics$teacher[which(CarePolitics$wave %in% c("1","2","3","4","5") &
                                is.na(CarePolitics$isco88))]=NA
CarePolitics$teacher[which(CarePolitics$wave %in% c("6","7","8","9","10","11") &
                                CarePolitics$isco08 %in% c("233","2330","234","2341","2342","2352"))]="Yes"
CarePolitics$teacher[which(CarePolitics$wave %in% c("6","7","8","9","10","11") &
                             CarePolitics$isco08 %notin% c("233","2330","234","2341","2342","2352"))]="No"
CarePolitics$teacher[which(CarePolitics$wave %in% c("6","7","8","9","10","11") &
                                is.na(CarePolitics$isco08))]=NA

#save(list=ls(),file="ESS_AllWaves.RData")
#save(CarePolitics,file="CarePolitics.RData")

## A simple indicator of current union membership
CarePolitics$mbtru_bin<-recode_factor(CarePolitics$mbtru,"Yes, previously"="No",
                                      "Yes, currently"="Yes")
CarePolitics$mbtru_bin<-droplevels(CarePolitics$mbtru_bin)

## A simple indicator of voting
CarePolitics$vote_bin<-recode_factor(CarePolitics$vote,"Not eligible to vote"="No")
CarePolitics$vote_bin<-droplevels(CarePolitics$vote_bin)

## Grouping of political participation items
CarePolitics$part_inst<-factor(nrow(CarePolitics),levels=c("No","Yes"))
CarePolitics$part_inst[which(CarePolitics$wrkprty=="No")]="No"
CarePolitics$part_inst[which(CarePolitics$contplt=="No")]="No"
CarePolitics$part_inst[which(CarePolitics$wrkprty=="Yes")]="Yes"
CarePolitics$part_inst[which(CarePolitics$contplt=="Yes")]="Yes"

CarePolitics$part_ntinst<-factor(nrow(CarePolitics),levels=c("No","Yes"))
CarePolitics$part_ntinst[which(CarePolitics$pbldmn=="No")]="No"
CarePolitics$part_ntinst[which(CarePolitics$pbldmn=="No")]="No"
CarePolitics$part_ntinst[which(CarePolitics$pbldmn=="Yes")]="Yes"
CarePolitics$part_ntinst[which(CarePolitics$sgnptit=="Yes")]="Yes"


## An indicator of labour market outsiderness
CarePolitics$outsider<-factor(nrow(CarePolitics),levels=c("Insider","Outsider"))

CarePolitics$outsider[which(CarePolitics$pdwrk=="Not marked")]="Outsider"
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              CarePolitics$wrkctra %in% c("Limited","No contract"))]="Outsider"
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              is.na(CarePolitics$wrkctra))]=NA
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              CarePolitics$wrkctra=="Unlimited" &
                              CarePolitics$icwhct=="No")]="Outsider"
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              CarePolitics$wrkctra=="Unlimited" &
                              is.na(CarePolitics$icwhct) & is.na(CarePolitics$wkhct))]=NA
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              CarePolitics$wrkctra=="Unlimited" &
                              CarePolitics$icwhct=="Yes" & CarePolitics$wkhct<20)]="Outsider"
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              CarePolitics$wrkctra=="Unlimited" &
                              CarePolitics$icwhct=="Yes" & CarePolitics$wkhct>=20)]="Insider"
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              CarePolitics$wrkctra=="Unlimited" &
                              is.na(CarePolitics$icwhct) & CarePolitics$wkhct<20)]="Outsider"
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              CarePolitics$wrkctra=="Unlimited" &
                              is.na(CarePolitics$icwhct) & CarePolitics$wkhct>=20)]="Insider"
CarePolitics$outsider[which(CarePolitics$pdwrk=="Marked" & 
                              CarePolitics$wrkctra=="Unlimited" &
                              CarePolitics$icwhct=="Yes" & is.na(CarePolitics$wkhct))]=NA

## An indicator of citizenship with categories: national, EU, 3rd country
# 1st: is a country in the EU?
CarePolitics$EUcntry<-factor(nrow(CarePolitics),levels=c("Not EU","EU"))
CarePolitics$EUcntry[which(CarePolitics$cntry %in% c("Austria","Belgium","Germany","Denmark",
                                                     "Spain","Finland","France","Greece","Ireland",
                                                     "Italy","Luxembourg","Netherlands","Portugal",
                                                     "Sweden"))]="EU"

CarePolitics$EUcntry[which(CarePolitics$cntry %in% c("Cyprus","Czechia","Estonia","Hungary",
                                                     "Lithuania","Latvia","Poland","Slovenia",
                                                     "Slovakia") & 
                             as.numeric(CarePolitics$wave)>=2)]="EU"

CarePolitics$EUcntry[which(CarePolitics$cntry %in% c("Cyprus","Czechia","Estonia","Hungary",
                                                     "Lithuania","Latvia","Poland","Slovenia",
                                                     "Slovakia") & 
                             as.numeric(CarePolitics$wave)<2)]="Not EU"

CarePolitics$EUcntry[which(CarePolitics$cntry %in% c("Bulgaria","Romania") & 
                             as.numeric(CarePolitics$wave)>=4)]="EU"

CarePolitics$EUcntry[which(CarePolitics$cntry %in% c("Bulgaria","Romania") & 
                             as.numeric(CarePolitics$wave)<4)]="Not EU"

CarePolitics$EUcntry[which(CarePolitics$cntry=="Croatia" & 
                             as.numeric(CarePolitics$wave)>=9)]="EU"

CarePolitics$EUcntry[which(CarePolitics$cntry=="Croatia" & 
                             as.numeric(CarePolitics$wave)<9)]="Not EU"

CarePolitics$EUcntry[which(CarePolitics$cntry=="United Kingdom" & 
                             as.numeric(CarePolitics$wave)<=9)]="EU"

CarePolitics$EUcntry[which(CarePolitics$cntry=="United Kingdom" & 
                             as.numeric(CarePolitics$wave)>9)]="Not EU"

CarePolitics$EUcntry[which(CarePolitics$cntry %in% c("Albania","Switzerland","Israel","Iceland",
                                                    "Montenegro","North Macedonia","Norway",
                                                    "Serbia","Russian Federation","Turkey",
                                                    "Ukraine","Kosovo"))]="Not EU"

# 2nd: is respondent's citizenship an EU citizenship?
CarePolitics$EUctzship<-factor(nrow(CarePolitics),levels=c("Not EU","EU","National"))

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="Yes")]="National"

# Countries that are members in all Waves, in Waves 10 and 11 we use country of birth
CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & as.numeric(CarePolitics$wave)<=9 &
                               CarePolitics$ctzship %in% c("Austria","Belgium","Germany","Denmark",
                                                           "Spain","Finland","France","Greece","Ireland",
                                                           "Italy","Luxembourg","Netherlands","Portugal",
                                                           "Sweden"))]="EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & as.numeric(CarePolitics$wave)>=10 &
                               CarePolitics$cntbrth %in% c("Austria","Belgium","Germany","Denmark",
                                                           "Spain","Finland","France","Greece","Ireland",
                                                           "Italy","Luxembourg","Netherlands","Portugal",
                                                           "Sweden"))]="EU"

# Countries that joined in 2004, members since Wave 2, in Waves 10 and 11 we use country of birth
CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & 
                               CarePolitics$ctzship %in% c("Cyprus","Czechia","Estonia","Hungary",
                                                           "Lithuania","Latvia","Poland","Slovenia",
                                                           "Slovakia") & 
                               as.numeric(CarePolitics$wave)==1)]="Not EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & 
                               CarePolitics$ctzship %in% c("Cyprus","Czechia","Estonia","Hungary",
                                                           "Lithuania","Latvia","Poland","Slovenia",
                                                           "Slovakia") & 
                               as.numeric(CarePolitics$wave) %in% 2:9)]="EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" &
                               CarePolitics$cntbrth %in% c("Cyprus","Czechia","Estonia","Hungary",
                                                           "Lithuania","Latvia","Poland","Slovenia",
                                                           "Slovakia") & 
                               as.numeric(CarePolitics$wave) >=10)]="EU"

# Bulgaria and Romania: part of EU since Wave 4, in Waves 10 and 11 we use country of birth
CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" &
                               CarePolitics$ctzship %in% c("Bulgaria","Romania") & 
                               as.numeric(CarePolitics$wave)<=3)]="Not EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" &
                               CarePolitics$ctzship %in% c("Bulgaria","Romania") & 
                               as.numeric(CarePolitics$wave) %in% 4:9)]="EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & 
                               CarePolitics$cntbrth %in% c("Bulgaria","Romania") & 
                               as.numeric(CarePolitics$wave) >=10)]="EU"

# Croatia: member since Wave 9, in Waves 10 and 11 we use country of birth
CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & 
                               CarePolitics$ctzship=="Croatia" & 
                               as.numeric(CarePolitics$wave)<9)]="Not EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & 
                               CarePolitics$ctzship=="Croatia" & 
                               as.numeric(CarePolitics$wave)==9)]="EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & 
                               CarePolitics$cntbrth=="Croatia" & 
                               as.numeric(CarePolitics$wave)>=10)]="EU"

# UK: member until Wave 10, we use country of birth for Waves 10 and 11
CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & 
                               CarePolitics$ctzship=="United Kingdom" & 
                               as.numeric(CarePolitics$wave)<=9)]="EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" & 
                               CarePolitics$cntbrth=="United Kingdom" & 
                               as.numeric(CarePolitics$wave)>=10)]="Not EU"

# Countries that have never been in the EU. In Waves 10 and 11 we use country of birth
ind1<-which(levels(CarePolitics$ctzship) %in% c("Austria","Belgium","Germany","Denmark",
                                                "Spain","Finland","France","Greece","Ireland",
                                                "Italy","Luxembourg","Netherlands","Portugal",
                                                "Sweden","Cyprus","Czechia","Estonia","Hungary",
                                                "Lithuania","Latvia","Poland","Slovenia",
                                                "Slovakia","Bulgaria","Romania","Croatia",
                                                "United Kingdom"))
ind2<-which(levels(CarePolitics$cntbrth) %in% c("Austria","Belgium","Germany","Denmark",
                                                "Spain","Finland","France","Greece","Ireland",
                                                "Italy","Luxembourg","Netherlands","Portugal",
                                                "Sweden","Cyprus","Czechia","Estonia","Hungary",
                                                "Lithuania","Latvia","Poland","Slovenia",
                                                "Slovakia","Bulgaria","Romania","Croatia",
                                                "United Kingdom"))

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" &
                               as.numeric(CarePolitics$wave)<=9 &
                               CarePolitics$ctzship %in% (levels(CarePolitics$ctzship)[-ind1]))]="Not EU"

CarePolitics$EUctzship[which(CarePolitics$ctzcntr=="No" &
                               CarePolitics$ctzcntr=="No" & as.numeric(CarePolitics$wave)>=10 &
                               CarePolitics$cntbrth %in% (levels(CarePolitics$cntbrth)[-ind2]))]="Not EU"

## 3rd: the variable we will use in the analyses
CarePolitics$citizen<-factor(nrow(CarePolitics),levels=c("National","EU","Third country"))
CarePolitics$citizen[which(CarePolitics$EUctzship=="National")]="National"
CarePolitics$citizen[which(CarePolitics$EUctzship=="Not EU")]="Third country"

CarePolitics$citizen[which(CarePolitics$EUctzship=="EU" & CarePolitics$EUcntry=="EU")]="EU"
CarePolitics$citizen[which(CarePolitics$EUctzship=="EU" & CarePolitics$EUcntry=="Not EU")]="Third country"

CarePolitics$citizen[which(is.na(CarePolitics$EUctzship) & CarePolitics$EUcntry=="EU")]=NA
CarePolitics$citizen[which(is.na(CarePolitics$EUctzship) & CarePolitics$EUcntry=="Not EU")]="Third country"

# What to do with all the foreigners whose citizenship we do not know? 
# Classify as third country? Merge all foreign categories for the analysis?

## Numeric household income indicator
CarePolitics$hinctnta_num<-as.numeric(CarePolitics$hinctnta)

#####

###
## Fixing countries in first 4 waves not harmonised into EISCED
###

## France: Waves 1 and 2
CarePolitics$eisced[which(CarePolitics$cntry=="France"& CarePolitics$wave %in% 1:2 & 
                             CarePolitics$edlvfr %in% c("Sans diplôme",
                                                        "Certificat d'études primaires"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="France"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvfr %in% c("Non diplômés jusqu'à la fin 3ème, 2nde, 1ère filière général",
                                                       "Non diplômés du CAP BEP filière professionnelle",
                                                       "Brevet élementaire, brevet d'étude du premier cycle, brevet"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="France"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvfr %in% c("CAP, examen de fin d'apprentissage artisanal",
                                                       "BEP, BP, BEA, BEC, BEI, BES"))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="France"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvfr %in% c("Baccalauréat général, brevet supérieur",
                                                       "Brevet de technicien, baccalauréat de technicien, baccalauré"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="France"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvfr %in% c("Diplôme universitaire du premier cycle (DEUG), diplôme unive"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="France"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvfr %in% c("Dipl. univ. du deuxième cycle, CAPES, Diplôme des grandes écoles"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="France"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvfr %in% c("Diplôme universitaire des deuxième et troisième cycles, Doct"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## UK: Waves 1, 2, 3, 4
CarePolitics$eisced[which(CarePolitics$cntry=="United Kingdom" & CarePolitics$wave %in% 1:4 &
                            CarePolitics$edlvgb %in% c("No qualifications", "No Qualifications"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="United Kingdom"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvgb %in% c("CSE Grade 1 / O-Level /  GCSE Grades A-C Or Equivalent",
                            "CSE Grade 2-5 / GCSE Grades D-G Or Equivalent"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="United Kingdom"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvgb %in% c("GCSE/O-level/CSE/NVQ1/NVQ2 or equiv"))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="United Kingdom"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvgb %in% c("A-level/NVQ3 or equiv","A-Level, As-Level Or Equivalent"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="United Kingdom"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvgb %in% c("NVQ4/NVQ5 or equiv",
                            "Degree/HNC/teacher training/nursing or equiv"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="United Kingdom"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvgb %in% c("Degree / Postgraduate Qualification Or Equivalent"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="United Kingdom"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvgb %in% c("PhD/DPhil or equiv"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Greece: Waves 1, 2, 4
CarePolitics$eisced[which(CarePolitics$cntry=="Greece" & CarePolitics$wave %in% c(1,2,4) &
                            CarePolitics$edlvgr %in% c("Illiterate/not completed primary","Primary",
                                                       "Analphabetic (not knowing reading and writing),not completed primary education/only few classes of primary education",
                                                       "Primary education"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Greece"& CarePolitics$wave %in% c(1,2,4) & 
                            CarePolitics$edlvgr %in% c("Lower secondary education (i.e. only few classes of secondary education,night school,technical vocational schools)",
                                                       "Partial secondary"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Greece"& CarePolitics$wave %in% c(1,2,4) & 
                            CarePolitics$edlvgr %in% c("Upper secondary education"))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Greece"& CarePolitics$wave %in% c(1,2,4) & 
                            CarePolitics$edlvgr %in% c("Full secondary"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Greece"& CarePolitics$wave %in% c(1,2,4) & 
                            CarePolitics$edlvgr %in% c("Post-compulsory Secondary Education/non-tertiary education (i.e.Public and Private Vocational Training Institutes etc.)",
                                                       "Post secondary/polytechnic"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Greece"& CarePolitics$wave %in% c(1,2,4) & 
                            CarePolitics$edlvgr %in% c("Higher Education:University Diplomas holders and Technical Educational Institutions Diplomas holders",
                                                       "University degree"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Greece"& CarePolitics$wave %in% c(1,2,4) & 
                            CarePolitics$edlvgr %in% c("MA Degree","Post graduate degree","PhD Degree"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Ireland: Waves 1, 2, 3, 4
CarePolitics$eisced[which(CarePolitics$cntry=="Ireland" & CarePolitics$wave %in% 1:4 &
                            CarePolitics$edlvie %in% c("None/primary not completed","None/Primary not completed",
                                                       "Primary or equivalent"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Ireland"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvie %in% c("Intermediate/junior/group cert or equiv",
                                                       "Intermediate/Group/Junior Cert or equivalent"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Ireland"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvie %in% c("Leaving cert or equivalent",
                                                       "Leaving Cert or equivalent"))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Ireland"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvie %in% c())]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Ireland"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvie %in% c())]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Ireland"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvie %in% c("Diploma/certificate","Primary degree",
                                                       "Diploma or Certificate","Primary Degree"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Ireland"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvie %in% c("Postgraduate/higher degree","Postgraduate Higher Diploma/Masters",
                                                       "PhD"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Italy: Waves 1 and 2
CarePolitics$eisced[which(CarePolitics$cntry=="Italy"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvit %in% c("Senza titolo","Licenza elementare"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Italy"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvit %in% c("Licenza media / avviamento professionale"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Italy"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvit %in% c("Licenza media/avviamento professionale"))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Italy"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvit %in% c("Diploma scuola media superiore",
                                                      "Diploma di scuola media superiore"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Italy"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvit %in% c("Specializzazione post-diploma"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Italy"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvit %in% c("Diploma universitario","Laurea",
                                                       "Diploma universitario o Laurea triennale"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Italy"& CarePolitics$wave %in% 1:2 & 
                            CarePolitics$edlvit %in% c("Specializzazione post-laurea",
                                                       "Laurea specialistica"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Portugal: Waves 1, 2, 3
CarePolitics$eisced[which(CarePolitics$cntry=="Portugal" & CarePolitics$wave %in% 1:3 &
                            CarePolitics$edlvpt %in% c("Nenhum","1 ciclo"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Portugal"& CarePolitics$wave %in% 1:3 & 
                            CarePolitics$edlvpt %in% c("2 ciclo"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Portugal"& CarePolitics$wave %in% 1:3 & 
                            CarePolitics$edlvpt %in% c("3 ciclo"))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Portugal"& CarePolitics$wave %in% 1:3 & 
                            CarePolitics$edlvpt %in% c("Secundario"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Portugal"& CarePolitics$wave %in% 1:3 & 
                            CarePolitics$edlvpt %in% c("Superior Politecnico",
                                                       "Superior politecnico"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Portugal"& CarePolitics$wave %in% 1:3 & 
                            CarePolitics$edlvpt %in% c("Superior Universitario",
                                                       "Superior universitario"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Portugal"& CarePolitics$wave %in% 1:3 & 
                            CarePolitics$edlvpt %in% c("Mestrado/Doutoramento",
                                                       "Pós-graduação","Mestrado","Doutoramento"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Sweden: Waves 1, 2, 3, 4
CarePolitics$eisced[which(CarePolitics$cntry=="Sweden" & CarePolitics$wave %in% 1:4 &
                            CarePolitics$edlvse %in% c("Not finished elementary school",
                                                       "Elementary school, old",
                                                       "Elementary school"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Sweden"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvse %in% c("Lower secondary and elementary school, old",
                                                       "Vocational school 1963-1970","Ej avslutad folkskola/grundskola",
                                                       "Folkskola","Grundskola/Enhetsskola",
                                                       "Realskola/Flickskola","Fackskola (1963-1970)"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Sweden"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvse %in% c("2 year high school","3-4 year high school prior 1995",
                                                      "2-årig gymnasielinje, 2-årig yrkesskola",
                                                      "3- eller 4 årig gymnasium (före 1995)"))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Sweden"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvse %in% c("Theoretical high school after 1992",
                                                       "Teoretiskt gymnasium (efter 1992)"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Sweden"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvse %in% c("Vocational high school after 1992",
                                                       "Yrkesinriktat gymnasium (efter 1992)"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Sweden"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvse %in% c("University, no exam","University, exam less than 3 years",
                                                       "Universitet/högskola utan examen",
                                                       "Universitet/högskola, kortare än 3 år, med examen"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Sweden"& CarePolitics$wave %in% 1:4 & 
                            CarePolitics$edlvse %in% c("University, exam more than 3 years",
                                                       "Universitet/högskola, 3 år eller längre, med examen",
                                                       "Forskarutbildning"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Ukraine: Waves 2, 3
CarePolitics$eisced[which(CarePolitics$cntry=="Ukraine" & CarePolitics$wave %in% 2:3 &
                            CarePolitics$edlvua %in% c("Not completed primary education (less than 4 years of secondary school)",
                                                       "Primary education (4-7 years of secondary school)"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Ukraine"& CarePolitics$wave %in% 2:3 & 
                            CarePolitics$edlvua %in% c("Not completed secondary education (8 -9 years of secondary school)"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Ukraine"& CarePolitics$wave %in% 2:3 & 
                            CarePolitics$edlvua %in% c(""))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Ukraine"& CarePolitics$wave %in% 2:3 & 
                            CarePolitics$edlvua %in% c("Completed secondary education (10-11 years of secondary school)"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Ukraine"& CarePolitics$wave %in% 2:3 & 
                            CarePolitics$edlvua %in% c("Secondary technical education (college, more than secondary, but not high)"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Ukraine"& CarePolitics$wave %in% 2:3 & 
                            CarePolitics$edlvua %in% c("First stage of high education (bachelor)"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Ukraine"& CarePolitics$wave %in% 2:3 & 
                            CarePolitics$edlvua %in% c("Completed high education (specialist, master, post-graduate, scientific degree)"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Bulgaria: Wave 3
CarePolitics$eisced[which(CarePolitics$cntry=="Bulgaria" & CarePolitics$wave==3 &
                            CarePolitics$edlvbg %in% c("Not completed primary education",
                                                       "Primary education (I-IV grade)"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Bulgaria"& CarePolitics$wave==3 & 
                            CarePolitics$edlvbg %in% c("Lower secondary education (V-VIII grade)"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Bulgaria"& CarePolitics$wave==3 & 
                            CarePolitics$edlvbg %in% c(""))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Bulgaria"& CarePolitics$wave==3 & 
                            CarePolitics$edlvbg %in% c("Upper secondary (IX-XIII grade)"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Bulgaria"& CarePolitics$wave==3 & 
                            CarePolitics$edlvbg %in% c("Post secondary, non-tertiary education"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Bulgaria"& CarePolitics$wave==3 & 
                            CarePolitics$edlvbg %in% c("Tertiary education"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Bulgaria"& CarePolitics$wave==3 & 
                            CarePolitics$edlvbg %in% c(""))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Cyprus: Waves 3, 4
CarePolitics$eisced[which(CarePolitics$cntry=="Cyprus" & CarePolitics$wave %in% 3:4 &
                            CarePolitics$edlvcy %in% c("Not completed primary education",
                                                       "Primary or first stage of basic"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Cyprus"& CarePolitics$wave %in% 3:4 & 
                            CarePolitics$edlvcy %in% c("Lower secondary or second stage of basic"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Cyprus"& CarePolitics$wave %in% 3:4 & 
                            CarePolitics$edlvcy %in% c(""))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Cyprus"& CarePolitics$wave %in% 3:4 & 
                            CarePolitics$edlvcy %in% c("Upper secondary"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Cyprus"& CarePolitics$wave %in% 3:4 & 
                            CarePolitics$edlvcy %in% c("Diploma (Tertiary not university)"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Cyprus"& CarePolitics$wave %in% 3:4 & 
                            CarePolitics$edlvcy %in% c("Bachelor/Master/PhD"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Cyprus"& CarePolitics$wave %in% 3:4 & 
                            CarePolitics$edlvcy %in% c(""))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Austria: Wave 4
CarePolitics$eisced[which(CarePolitics$cntry=="Austria" & CarePolitics$wave==4 &
                            CarePolitics$edlvat %in% c("Pflichtschule nicht abgeschlossen",
                                                       "Pflichtschule"))]="ES-ISCED I , less than lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Austria"& CarePolitics$wave==4 & 
                            CarePolitics$edlvat %in% c("BMS, Berufslehre, Berufsschule"))]="ES-ISCED II, lower secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Austria"& CarePolitics$wave==4 & 
                            CarePolitics$edlvat %in% c(""))]="ES-ISCED IIIb, lower tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Austria"& CarePolitics$wave==4 & 
                            CarePolitics$edlvat %in% c("Höhere Schule mit Matura"))]="ES-ISCED IIIa, upper tier upper secondary"
CarePolitics$eisced[which(CarePolitics$cntry=="Austria"& CarePolitics$wave==4 & 
                            CarePolitics$edlvat %in% c("Kolleg, andere postsekundäre Einrichtungen"))]="ES-ISCED IV, advanced vocational, sub-degree"
CarePolitics$eisced[which(CarePolitics$cntry=="Austria"& CarePolitics$wave==4 & 
                            CarePolitics$edlvat %in% c("Erste Stufe universitäre Ausbildung"))]="ES-ISCED V1, lower tertiary education, BA level"
CarePolitics$eisced[which(CarePolitics$cntry=="Austria"& CarePolitics$wave==4 & 
                            CarePolitics$edlvat %in% c("Akademischer Grad"))]="ES-ISCED V2, higher tertiary education, >= MA level"

## Fixing the eisced education variable and reducing to three categories
CarePolitics$education<-factor(nrow(CarePolitics),levels=c("Secondary or lower","Vocational","Tertiary"))
CarePolitics$education[which(CarePolitics$eisced %in% c("ES-ISCED I , less than lower secondary",
                                                        "ES-ISCED II, lower secondary",
                                                        "ES-ISCED IIIb, lower tier upper secondary",
                                                        "ES-ISCED IIIa, upper tier upper secondary"))]="Secondary or lower"
CarePolitics$education[which(CarePolitics$eisced=="ES-ISCED IV, advanced vocational, sub-degree")]="Vocational"
CarePolitics$education[which(CarePolitics$eisced %in% c("ES-ISCED V1, lower tertiary education, BA level",
                                                        "ES-ISCED V2, higher tertiary education, >= MA level"))]="Tertiary"
CarePolitics$education[which(CarePolitics$eisced %in% c("Not possible to harmonise into ES-ISCED",
                                                        "Other"))]=NA

# Full or part-time
CarePolitics$part_time<-factor(nrow(CarePolitics),levels=c("Full-time","Part-time"))
CarePolitics$part_time[which(CarePolitics$wkhct>=20)]<-"Full-time"
CarePolitics$part_time[which(CarePolitics$wkhct<20)]<-"Part-time"

# Wave as a non-ordered factor
CarePolitics$wave_fe<-factor(CarePolitics$wave,ordered=F)

################################################################################

###
# 7) REMOVE COUNTRIES WITH LOW N
###

### 3.1) Appendix: Sample sizes and presence of care workers

# Number of cases in each country and wave
t1.1<-addmargins(table(CarePolitics$cntry,CarePolitics$wave))
t1.1[which(t1.1==0)]="No data"
write.csv2(t1.1,"t1.1.csv")

# Number of personal care workers per country and wave
t1.2.1<-addmargins(table(CarePolitics$cntry[which(CarePolitics$pcareworker=="Yes")],
                         CarePolitics$wave[which(CarePolitics$pcareworker=="Yes")]))
t1.2.1[which(t1.1=="No data")]="No data"
write.csv2(t1.2.1,"t1.2.1.csv")

t1.2.2<-addmargins(table(CarePolitics$cntry[which(CarePolitics$pcareworker=="Yes" & CarePolitics$pdwrk=="Marked")],
                         CarePolitics$wave[which(CarePolitics$pcareworker=="Yes" & CarePolitics$pdwrk=="Marked")]))
t1.2.2[which(t1.1=="No data")]="No data"
write.csv2(t1.2.2,"t1.2.2.csv")

# Number of nursing (associate) professionals per country and wave
t1.3<-addmargins(table(CarePolitics$cntry[which(CarePolitics$nurse=="Yes")],
                       CarePolitics$wave[which(CarePolitics$nurse=="Yes")]))
t1.3[which(t1.1=="No data")]="No data"
write.csv2(t1.3,"t1.3.csv")

t1.3.2<-addmargins(table(CarePolitics$cntry[which(CarePolitics$nurse=="Yes" & CarePolitics$pdwrk=="Marked")],
                         CarePolitics$wave[which(CarePolitics$nurse=="Yes" & CarePolitics$pdwrk=="Marked")]))
t1.3.2[which(t1.1=="No data")]="No data"
write.csv2(t1.3.2,"t1.3.2.csv")

####

# Based on this analysis we decide to remove some countries

CarePolitics_all<-CarePolitics # Create a backup with all countries
CarePolitics<-CarePolitics[-which(CarePolitics$cntry %in% c("Albania","Montenegro",
                                                            "North Macedonia","Romania",
                                                            "Serbia","Luxembourg",
                                                            "Turkey","Kosovo",
                                                            "Russian Federation",
                                                            "Israel")),]
CarePolitics$cntry<-droplevels(CarePolitics$cntry)

####

# Add ECEC and LTC values

ECEC_Exp<-read.csv2("ECEC_Eurostat.csv",sep=",")

names(ECEC_Exp)[names(ECEC_Exp)=="Geopolitical.entity..reporting."]="cntry"
names(ECEC_Exp)[names(ECEC_Exp)=="TIME_PERIOD"]="year"
names(ECEC_Exp)[names(ECEC_Exp)=="OBS_VALUE"]="value"
names(ECEC_Exp)[names(ECEC_Exp)=="International.Standard.Classification.of.Education..ISCED.2011."]="ISCED"

ECEC_Exp<-ECEC_Exp[,c("cntry","year","ISCED","value")]

ECEC_Exp<-ECEC_Exp[c(which(ECEC_Exp$cntry=="Austria" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Belgium" & ECEC_Exp$ISCED=="Pre-primary education"),
                     which(ECEC_Exp$cntry=="Bulgaria" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Croatia" & ECEC_Exp$ISCED=="Early childhood education"), 
                     which(ECEC_Exp$cntry=="Cyprus" & ECEC_Exp$ISCED=="Pre-primary education"),
                     which(ECEC_Exp$cntry=="Czechia" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Denmark" & ECEC_Exp$ISCED=="Early childhood education"), 
                     which(ECEC_Exp$cntry=="Estonia" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Finland" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="France" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Germany" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Greece" & ECEC_Exp$ISCED=="Pre-primary education"), 
                     which(ECEC_Exp$cntry=="Hungary" & ECEC_Exp$ISCED=="Pre-primary education"),
                     which(ECEC_Exp$cntry=="Iceland" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Ireland" & ECEC_Exp$ISCED=="Early childhood education"), 
                     which(ECEC_Exp$cntry=="Italy" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Latvia" & ECEC_Exp$ISCED=="Pre-primary education"),
                     which(ECEC_Exp$cntry=="Lithuania" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Netherlands" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Norway" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Poland" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Portugal" & ECEC_Exp$ISCED=="Pre-primary education"),
                     which(ECEC_Exp$cntry=="Slovakia" & ECEC_Exp$ISCED=="Early childhood education"), 
                     which(ECEC_Exp$cntry=="Slovenia" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Spain" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Sweden" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="Switzerland" & ECEC_Exp$ISCED=="Early childhood education"),
                     which(ECEC_Exp$cntry=="United Kingdom" & ECEC_Exp$ISCED=="Early childhood education")),]
ECEC_Exp$value<-as.numeric(ECEC_Exp$value)

ECEC_Exp$mean<-numeric(nrow(ECEC_Exp))
for (i in 1:nrow(ECEC_Exp)){
  ECEC_Exp$mean[i]=mean(ECEC_Exp$value[ECEC_Exp$cntry==ECEC_Exp$cntry[i]],na.rm=T)
}
ECEC_Exp$sd<-numeric(nrow(ECEC_Exp))
for (i in 1:nrow(ECEC_Exp)){
  ECEC_Exp$sd[i]=sd(ECEC_Exp$value[ECEC_Exp$cntry==ECEC_Exp$cntry[i]],na.rm=T)
}

CarePolitics$ECEC_Exp<-ECEC_Exp$value[match(paste(CarePolitics$cntry,CarePolitics$year),
                                          paste(ECEC_Exp$cntry,ECEC_Exp$year))]

CarePolitics$ECEC_Exp_mean<-ECEC_Exp$mean[match(paste(CarePolitics$cntry,CarePolitics$year),
                                              paste(ECEC_Exp$cntry,ECEC_Exp$year))]

#############

LTC_Exp<-read.csv2("LTC_Eurostat.csv",sep=",")

names(LTC_Exp)[names(LTC_Exp)=="Geopolitical.entity..reporting."]="cntry"
names(LTC_Exp)[names(LTC_Exp)=="TIME_PERIOD"]="year"
names(LTC_Exp)[names(LTC_Exp)=="OBS_VALUE"]="value"

LTC_Exp<-LTC_Exp[,c("cntry","year","value")]

LTC_Exp$value<-as.numeric(LTC_Exp$value)

LTC_Exp$mean<-numeric(nrow(LTC_Exp))
for (i in 1:nrow(LTC_Exp)){
  LTC_Exp$mean[i]=mean(LTC_Exp$value[LTC_Exp$cntry==LTC_Exp$cntry[i]],na.rm=T)
}
LTC_Exp$sd<-numeric(nrow(LTC_Exp))
for (i in 1:nrow(LTC_Exp)){
  LTC_Exp$sd[i]=sd(LTC_Exp$value[LTC_Exp$cntry==LTC_Exp$cntry[i]],na.rm=T)
}

CarePolitics$LTC_Exp<-LTC_Exp$value[match(paste(CarePolitics$cntry,CarePolitics$year),
                                          paste(LTC_Exp$cntry,LTC_Exp$year))]

CarePolitics$LTC_Exp_mean<-LTC_Exp$mean[match(paste(CarePolitics$cntry,CarePolitics$year),
                                               paste(LTC_Exp$cntry,LTC_Exp$year))]

CarePolitics$Care_Exp<-CarePolitics$ECEC_Exp+CarePolitics$LTC_Exp
CarePolitics$Care_Exp_mean<-CarePolitics$ECEC_Exp_mean+CarePolitics$LTC_Exp_mean

################################################################################

###
# 8) CREATE ANALYTICAL SAMPLE
###

# Furthermore, we exclude respondents below 18 years old and those who are not working

CPSample<-CarePolitics[-which(CarePolitics$agea<18),]
CPSample<-CPSample[-which(CPSample$pdwrk=="Not marked"),]

save(list=ls(),file="DataP1_base.RData")