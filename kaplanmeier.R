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

## a.  The factors enhancing the risk to start a reduced activity


# Kaplan-Meier non parametric analysis:
kmsurvival04 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~1)
kmsurvival05 <- survfit(Surv(dem05$duree_avt_ar1, dem05$censur_ar1)~1)
kmsurvival06 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~1)
kmsurvival08 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~1)
kmsurvival10 <- survfit(Surv(dem10$duree_avt_ar1, dem10$censur_ar1)~1)

plot(kmsurvival04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(kmsurvival05, ann=FALSE, axes=FALSE,col='red')
par(new=TRUE)
plot(kmsurvival06, ann=FALSE, axes=FALSE,col='green')
par(new=TRUE)
plot(kmsurvival08, ann=FALSE, axes=FALSE,col='blue')
par(new=TRUE)
plot(kmsurvival10, axes=FALSE,col='lightblue',  main="Kaplan-Meier estimates of survival until first RA", xlab="Number of months", ylab="Survival Probability")
legend("topright", c("2004","2005","2006","2008","2010"), col=(1:5), lwd=0.5)
# the 95% confidence interval is narrowingly around the initial curve, which makes our observations quite solid


# Kaplan-Meier non parametric analysis by group:
## analysis per gender:
kmsurvival1a04 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$SEXE)
plot(kmsurvival1a04, col=c(1:2),
     main="Hazard rates of starting a RA by gender (K-M in 2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Male","Female"), col=(1:2), lwd=0.5)

kmsurvival1a06 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$SEXE)
plot(kmsurvival1a06, col=c(1:2),
     main="Hazard rates of starting a RA by gender (K-M in 2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Male","Female"), col=(1:2), lwd=0.5)

kmsurvival1a08 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$SEXE)
plot(kmsurvival1a08, col=c(1:2),
     main="Hazard rates of starting a RA by gender (K-M in 2008)",
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

kmsurvival2a04 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$ageq)
plot(kmsurvival2a04, col=c(1:8),
     main="Hazard rates of starting a RA by age (K-M in 2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5)

kmsurvival2a06 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$ageq)
plot(kmsurvival2a06, col=c(1:8),
     main="Hazard rates of starting a RA by age (K-M in 2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5)

kmsurvival2a08 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$ageq)
plot(kmsurvival2a08, col=c(1:8),
     main="Hazard rates of starting a RA by age (K-M in 2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5)

###### ONLY HAZARD RATES OF STARTING A RA #######

## analysis per sector of activity:
kmsurvival304 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$metier_service)
plot(kmsurvival304, col=c(1:2),
     main="Hazard rates of starting a RA if tertiary sector work (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival306 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$metier_service)
plot(kmsurvival306, col=c(1:2),
     main="Hazard rates of starting a RA if tertiary sector work (2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival308 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$metier_service)
plot(kmsurvival308, col=c(1:2),
     main="Hazard rates of starting a RA if tertiary sector work (2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

#########

kmsurvival404 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$metier_industrie)
plot(kmsurvival404, col=c(1:2),
     main="Surv. of starting a RA in industry sector (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival406 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$metier_industrie)
plot(kmsurvival406, col=c(1:2),
     main="Surv. of starting a RA in industry sector (2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival408 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$metier_industrie)
plot(kmsurvival408, col=c(1:2),
     main="Hazard rates of starting a RA in industry sector (2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

################

kmsurvival504 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$metier_commerce)
plot(kmsurvival504, col=c(1:2),
     main="Surv. of starting a RA in trade sector (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival506 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$metier_commerce)
plot(kmsurvival506, col=c(1:2),
     main="Surv. of starting a RA in trade sector (2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival508 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$metier_commerce)
plot(kmsurvival508, col=c(1:2),
     main="Surv. of starting a RA in trade sector (2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)


# per type of contract
kmsurvival604 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$contrat_cdi)
plot(kmsurvival604, col=c(1:2),
     main="Hazard rates of starting a RA for long term (CDI) jobs (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival606 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$contrat_cdi)
plot(kmsurvival606, col=c(1:2),
     main="Hazard rates of starting a RA for long term (CDI) jobs (2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival608 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$contrat_cdi)
plot(kmsurvival608, col=c(1:2),
     main="Hazard rates of starting a RA for long term (CDI) jobs (2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

#################


dem04$exper <- matrix("NA", nrow =length(dem04$experience), ncol =1)
dem05$exper <- matrix("NA", nrow =length(dem05$experience), ncol =1)
dem06$exper <- matrix("NA", nrow =length(dem06$experience), ncol =1)
dem08$exper <- matrix("NA", nrow =length(dem08$experience), ncol =1)
dem10$exper <- matrix("NA", nrow =length(dem10$experience), ncol =1)


# Creating a dummy for experience
for (i in 1:length(dem04$experience)){
  if (dem04$experience[i] <= "5"){
    dem04$exper[i] <- "1"
  } 
  else{ if(dem04$experience[i] > "5" & dem04$experience[i] <= "10"){
    dem04$exper[i] <- "2"
  } 
  else{ if(dem04$experience[i] > "10" & dem04$experience[i] <= "15"){
    dem04$exper[i] <- "3"
  } 
  else{ if(dem04$experience[i] > "15"){
    dem04$exper[i] <- "4"
  }
  }
  }
  }
}

for (i in 1:length(dem05$experience)){
  if (dem05$experience[i] <= "5"){
    dem05$exper[i] <- "1"
  } 
  else{ if(dem05$experience[i] > "5" & dem05$experience[i] <= "10"){
    dem05$exper[i] <- "2"
  } 
  else{ if(dem05$experience[i] > "10" & dem05$experience[i] <= "15"){
    dem05$exper[i] <- "3"
  } 
  else{ if(dem05$experience[i] > "15"){
    dem05$exper[i] <- "4"
  }
  }
  }
  }
}

for (i in 1:length(dem06$experience)){
  if (dem06$experience[i] <= "5"){
    dem06$exper[i] <- "1"
  } 
  else{ if(dem06$experience[i] > "5" & dem06$experience[i] <= "10"){
    dem06$exper[i] <- "2"
  } 
  else{ if(dem06$experience[i] > "10" & dem06$experience[i] <= "15"){
    dem06$exper[i] <- "3"
  } 
  else{ if(dem06$experience[i] > "15"){
    dem06$exper[i] <- "4"
  }
  }
  }
  }
}


for (i in 1:length(dem08$experience)){
  if (dem08$experience[i] <= "5"){
    dem08$exper[i] <- "1"
  } 
  else{ if(dem08$experience[i] > "5" & dem08$experience[i] <= "10"){
    dem08$exper[i] <- "2"
  } 
  else{ if(dem08$experience[i] > "10" & dem08$experience[i] <= "15"){
    dem08$exper[i] <- "3"
  } 
  else{ if(dem08$experience[i] > "15"){
    dem08$exper[i] <- "4"
  }
  }
  }
  }
}


for (i in 1:length(dem10$experience)){
  if (dem10$experience[i] <= "5"){
    dem10$exper[i] <- "1"
  } 
  else{ if(dem10$experience[i] > "5" & dem10$experience[i] <= "10"){
    dem10$exper[i] <- "2"
  } 
  else{ if(dem10$experience[i] > "10" & dem10$experience[i] <= "15"){
    dem10$exper[i] <- "3"
  } 
  else{ if(dem10$experience[i] > "15"){
    dem10$exper[i] <- "4"
  }
  }
  }
  }
}


## analysis per experience:
kmsurvival704 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$exper)
plot(kmsurvival704, col=c(1:2),
     main="Hazard rates of starting a RA by experience (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("under 5","over 15"), col=(1:2), lwd=0.5)

kmsurvival706 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$exper)
plot(kmsurvival706, col=c(1:2),
     main="Hazard rates of starting a RA by experience (2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("under 5","over 15"), col=(1:2), lwd=0.5)

kmsurvival708 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$exper)
plot(kmsurvival708, col=c(1:2),
     main="Hazard rates of starting a RA by experience (2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("under 5","over 15"), col=(1:2), lwd=0.5)


## analysis on the criterion of education:
kmsurvival804 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$Diplome_oui)
plot(kmsurvival804, col=c(1:2),
     main="Hazard rates of starting a RA by gender (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Educated", "Not educated"), col=(1:2), lwd=0.5)

kmsurvival806 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$Diplome_oui)
plot(kmsurvival806, col=c(1:2),
     main="Hazard rates of starting a RA by gender (2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Educated", "Not educated"), col=(1:2), lwd=0.5)

kmsurvival808 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$Diplome_oui)
plot(kmsurvival808, col=c(1:2),
     main="Hazard rates of starting a RA by gender (2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Educated", "Not educated"), col=(1:2), lwd=0.5)


## analysis on the criterion of being a temporary worker or not:

kmsurvival904 <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$interimaire)
plot(kmsurvival904, col=c(1:2),
     main="Hazard rates of starting a RA by gender (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Interimaire", "Not Interimaire"), col=(1:2), lwd=0.5)

kmsurvival906 <- survfit(Surv(dem06$duree_avt_ar1, dem06$censur_ar1)~ dem06$interimaire)
plot(kmsurvival906, col=c(1:2),
     main="Hazard rates of starting a RA by gender (2006)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Interimaire", "Not Interimaire"), col=(1:2), lwd=0.5)

kmsurvival908 <- survfit(Surv(dem08$duree_avt_ar1, dem08$censur_ar1)~ dem08$interimaire)
plot(kmsurvival908, col=c(1:2),
     main="Hazard rates of starting a RA by gender (2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Interimaire", "Not Interimaire"), col=(1:2), lwd=0.5)




## analysis of survival per number of months left with compensation
indemsurv04 <- survfit(Surv(dem04$INDEM_RESTANT, dem04$censur_ar1)~1)
indemsurv05 <- survfit(Surv(dem05$INDEM_RESTANT, dem05$censur_ar1)~1)
indemsurv06 <- survfit(Surv(dem06$INDEM_RESTANT, dem06$censur_ar1)~1)
indemsurv08 <- survfit(Surv(dem08$INDEM_RESTANT, dem08$censur_ar1)~1)
indemsurv10 <- survfit(Surv(dem10$INDEM_RESTANT, dem10$censur_ar1)~1)

plot(indemsurv04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(indemsurv06, ann=FALSE, axes=FALSE,col='red')
par(new=TRUE)
plot(indemsurv08, axes=FALSE,col='green', 
     main="Kaplan-Meier estimates of survival in RA until interruption of UI",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("2004", "2006","2008"), col=(1:5), lwd=0.5)
# the 95% confidence interval is narrowingly around the initial curve, which makes our observations quite solid



######## SURVIVAL FUNCTIONS FOR UI INTERRUPTION DUE TO RA ###############

kmsurvivalint04 <- survfit(Surv(dem04$duree_avt_interrup1, dem04$censur_interrup1)~1)
kmsurvivalint05 <- survfit(Surv(dem05$duree_avt_interrup1, dem05$censur_interrup1)~1)
kmsurvivalint06 <- survfit(Surv(dem06$duree_avt_interrup1, dem06$censur_interrup1)~1)
kmsurvivalint08 <- survfit(Surv(dem08$duree_avt_interrup1, dem08$censur_interrup1)~1)
kmsurvivalint10 <- survfit(Surv(dem10$duree_avt_interrup1, dem10$censur_interrup1)~1)

plot(kmsurvivalint04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(kmsurvivalint06, ann=FALSE, axes=FALSE, col='red')
par(new=TRUE)
plot(kmsurvivalint08, axes=FALSE, col='green', main="Kaplan-Meier estimates of survival until first RA", xlab="Number of months", ylab="Survival Probability")
legend("topright", c("2004","2006","2008"), col=(1:3), lwd=0.5)
# the 95% confidence interval is narrowingly around the initial curve, which makes our observations quite solid

#############################################


kmsurvival1b <- survfit(Surv(dem04$duree_avt_interrup1, dem04$censur_interrup1)~ dem04$SEXE)
plot(kmsurvival1b, col=c(1:2),
     main="K-M estimates of being in RA and not compensated (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Male","Female"), col=(1:2), lwd=0.5)

kmsurvival2b <- survfit(Surv(dem08$duree_avt_interrup1, dem08$censur_interrup1)~ dem08$SEXE)
plot(kmsurvival2b, col=c(1:8),
     main="K-M estimates of being in RA and not compensated (2008)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5)


# Creating a variable for specific subcases of "motins":

dem04$motins2 <- matrix("NA", nrow =length(dem04$MOTINS), ncol =1)
  for (i in 1:length(dem04$MOTINS)){
    if (dem04$MOTINS[i] == "3"){
      dem04$motins2[i] <- "3"
    } 
    else{ if(dem04$MOTINS[i] == "4"){
      dem04$motins2[i] <- "4"
    } 
    }
  }

kmsurvival2b <- survfit(Surv(dem04$duree_avt_ar1, dem04$censur_ar1)~ dem04$motins2)
plot(kmsurvival2b, col=c(1:2),
     main="K-M estimates of starting a RA per unemp motive (2004)",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Resignation", "End of contract"), col=(1:2), lwd=0.5)


#### SURVIVAL AVEC LES PRATIQUES INTENSIVES d'AR (HOUR DECISIONS):

indemsurv2a04 <- survfit(Surv(dem04$INDEM_RESTANT, dem04$PREM_SUP_110)~1)
indemsurv2a06 <- survfit(Surv(dem06$INDEM_RESTANT, dem06$PREM_SUP_110)~1)
indemsurv2a08 <- survfit(Surv(dem08$INDEM_RESTANT, dem08$PREM_SUP_110)~1)

plot(indemsurv2a04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(indemsurv2a06, ann=FALSE, axes=FALSE,col='red')
par(new=TRUE)
plot(indemsurv2a08, axes=FALSE,col='green', 
     main="Kaplan-Meier estimates of starting RA > 110H/months",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("2004", "2006","2008"), col=(1:3), lwd=0.5)
# the 95% confidence interval is narrowingly around the initial curve, which makes our observations quite solid



indemsur36a04 <- survfit(Surv(dem04$INDEM_RESTANT, dem04$PREM_SUP_136)~1)
indemsur36a06 <- survfit(Surv(dem06$INDEM_RESTANT, dem06$PREM_SUP_136)~1)
indemsur36a08 <- survfit(Surv(dem08$INDEM_RESTANT, dem08$PREM_SUP_136)~1)

plot(indemsur36a04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(indemsur36a06, ann=FALSE, axes=FALSE,col='red')
par(new=TRUE)
plot(indemsur36a08, axes=FALSE,col='green', 
     main="Kaplan-Meier estimates of starting RA > 136H/months",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("2004", "2006","2008"), col=(1:3), lwd=0.5)
# the 95% confidence interval is narrowingly around the initial curve, which makes our observations quite solid


####### NOT USED IN REPORT ######

# per number of unemployment months left
dem04$INDEM_RESTANT2 <- dem04$INDEM_RESTANT[which(dem04$INDEM_RESTANT >= "0")] 


kmsurvage04 <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_110)~ dem04$ageq)
plot(kmsurvage04, col=c(1:8),
     main="Hazard rates of starting a RA over 110H/month by age (Kaplan-Meier)",
     xlab="Number of months", ylab="Survival Probability",
legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5))

kmsurvage06 <- survfit(Surv(dem06$duree_avt_ar1, dem06$PREM_SUP_136)~ dem06$ageq)
plot(kmsurvage06, col=c(1:8),
     main="Hazard rates of starting a RA over 136H/month by age (Kaplan-Meier)",
     xlab="Number of months", ylab="Survival Probability",
legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5))

kmsurvage08 <- survfit(Surv(dem08$duree_avt_ar1, dem08$PREM_SUP_136)~ dem08$ageq)
plot(kmsurvage08, col=c(1:8),
     main="Hazard rates of starting a RA over 136H/month by age (Kaplan-Meier)",
     xlab="Number of months", ylab="Survival Probability",
     legend("topright", c("Under 20", "20 to 25", "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50", "Over 50"), col=(1:8), lwd=0.5))




## analysis per activity sector:
kmsurvival3a <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_110)~ dem04$metier_service)
plot(kmsurvival3a, col=c(1:2),
     main="Hazard rates of starting a RA over 110H/month if tertiary sector work",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival3b <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_136)~ dem04$metier_service)
plot(kmsurvival3b, col=c(1:2),
     main="Hazard rates of starting a RA over 136H/month if tertiary sector work",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival4a <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_110)~ dem04$metier_industrie)
plot(kmsurvival4a, col=c(1:2),
     main="Hazard rates of starting a RA over 110H/month for industry sector workers",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival4b <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_136)~ dem04$metier_industrie)
plot(kmsurvival4b, col=c(1:2),
     main="Hazard rates of starting a RA over 136H/month for industry sector workers",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)


kmsurvival5a <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_110)~ dem04$metier_commerce)
plot(kmsurvival5a, col=c(1:2),
     main="Hazard rates of starting a RA over 110H/month for trade sector workers",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival5b <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_136)~ dem04$metier_commerce)
plot(kmsurvival5b, col=c(1:2),
     main="Hazard rates of starting a RA over 136H/month for trade sector workers",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)


# per type of contract:
kmsurvival6a <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_110)~ dem04$contrat_cdi)
plot(kmsurvival6a, col=c(1:2),
     main="Hazard rates of starting a RA over 110H/month for long term (CDI) jobs",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)

kmsurvival6b <- survfit(Surv(dem04$duree_avt_ar1, dem04$PREM_SUP_136)~ dem04$contrat_cdi)
plot(kmsurvival6a, col=c(1:2),
     main="Hazard rates of starting a RA over 136H/month for long term (CDI) jobs",
     xlab="Number of months", ylab="Survival Probability")
legend("topright", c("Yes", "No"), col=(1:2), lwd=0.5)


