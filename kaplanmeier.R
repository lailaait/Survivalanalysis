library(Hmisc)
library(dplyr)
library(ggplot2)
library(survival)

# IMPORTING DATASET
###
setwd("C:/Users/Laila/Desktop/flux_completbis")
dat04 <- read.csv("flux2004.csv", header=T, sep=",")
dat05 <- read.csv("flux2005.csv", header=T, sep=",")
dat06 <- read.csv("flux2006.csv", header=T, sep=",")
dat08 <- read.csv("flux2008.csv", header=T, sep=",")
dat10 <- read.csv("flux2010.csv", header=T, sep=",")

#convert to a local data frame
dem04 <- tbl_df(dat04)
dem05 <- tbl_df(dat05)
dem06 <- tbl_df(dat06)
dem08 <- tbl_df(dat08)
dem10 <- tbl_df(dat10)

## 1. KAPLAN MEIER ESTIMATIONS

# Kaplan-Meier non parametric analysis:
kmsurvival <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~1)
summary(kmsurvival)
plot(kmsurvival, main="Kaplan-Meier estimates of survival until first RA", xlab="Number of months", ylab="Survival Probability")
# the 95% confidence interval is narrowingly around the initial curve, which makes our observations quite solid


kmsurvivalint <- survfit(Surv(dem04$duree_avt_interrup1, dem04$censur_interrup1)~1)
summary(kmsurvivalint)
plot(kmsurvival, main="Kaplan-Meier estimates of survival in RA until first compensation interruption", xlab="Number of months", ylab="Survival Probability")
# the 95% confidence interval is narrowingly around the initial curve, which makes our observations quite solid


# Kaplan-Meier non parametric analysis by group:
## analysis per gender:
kmsurvival1a <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$SEXE)
plot(kmsurvival1, col=c(1:2),
     main="Hazard rates of starting a RA by gender (Kaplan-Meier)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Male","Female"), col=(1:2), lwd=0.5)


kmsurvival1b <- survfit(Surv(dem04$duree_avt_interrup1, dem04$censur_interrup1)~ dem04$SEXE)
plot(kmsurvival1, col=c(1:2),
     main="K-M estimates of not being compensated for a RA per gender",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Male","Female"), col=(1:2), lwd=0.5)

## analysis per age:
# age variable constructed as follows:
# if age<=20 then ageq=1; 
# if age>=21 and age<=25 then ageq=2; 
# if age>=26 and age<=30 then ageq=3; 
# if age>=31 and age<=35 then ageq=4; 
# if age>=36 and age<=40 then ageq=5; 
# if age>=41 and age<=45 then ageq=6; 
# if age>=46 and age<=50 then ageq=7; 
# if age>=51 then ageq=8; 

kmsurvival2a <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$ageq)
plot(kmsurvival2, col=c(1:8),
     main="Hazard rates of starting a RA by age (Kaplan-Meier)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5)

kmsurvival2b <- survfit(Surv(dem04$duree_avt_interrup1, dem04$censur_interrup1)~ dem04$ageq)
plot(kmsurvival2, col=c(1:8),
     main="K-M estimates of not being compensated for a RA per age",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5)




###### ONLY HAZARD RATES OF STARTING A RA #######

## analysis per sector of activity:
kmsurvival3 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$metier_service)
plot(kmsurvival3, col=c(1:2),
     main="Hazard rates of starting a RA if tertiary sector work",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival4 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$metier_industrie)
plot(kmsurvival4, col=c(1:2),
     main="Hazard rates of starting a RA for industry sector workers",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival5 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$metier_commerce)
plot(kmsurvival5, col=c(1:2),
     main="Hazard rates of starting a RA for trade sector workers",
     xlab="Number of months", ylab="Survival Probability")
legend(30,1, c("Yes", "No"), col=(1:2), lwd=0.5)


#per type of contract
kmsurvival6 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$contrat_cdi)
plot(kmsurvival6, col=c(1:2),
     main="Hazard rates of starting a RA for long term (CDI) jobs",
     xlab="Number of months", ylab="Survival Probability")
legend(30,1, c("Yes", "No"), col=(1:2), lwd=0.5)
