
####### A FEW DISTRIBUTIONS :

## DISTRIBUTION OF NB_HEUR WORKED IN RA:


# 1st month:
dem04$NBHEUR_AR1bis <- matrix("NA", nrow =length(dem04$nbheur_ar1), ncol =1)
# Creating a dummy for number of hours worked:
for (i in 1:length(dem04$nbheur_ar1)){
  if (dem04$duree_de_mois[i] >= "12"){
    dem04$NBHEUR_AR1bis[i] <- "1"
  } 
  else{ if(dem04$duree_de_mois[i] < "12"){
    dem04$NBHEUR_AR1bis[i] <- "0"
  } 
  }
}

# 3rd month:
dem04$NBHEUR_AR3bis <- matrix("NA", nrow =length(dem04$nbheur_ar3), ncol =1)
# Creating a dummy for number of hours worked:
for (i in 1:length(dem04$nbheur_ar3)){
  if (dem04$duree_de_mois[i] >= "12"){
    dem04$NBHEUR_AR3bis[i] <- "1"
  } 
  else{ if(dem04$duree_de_mois[i] < "12"){
    dem04$NBHEUR_AR3bis[i] <- "0"
  } 
  }
}

# 6th month:
dem04$NBHEUR_AR6bis <- matrix("NA", nrow =length(dem04$nbheur_ar6), ncol =1)
# Creating a dummy for number of hours worked:
for (i in 1:length(dem04$nbheur_ar6)){
  if (dem04$duree_de_mois[i] >= "12"){
    dem04$NBHEUR_AR6bis[i] <- "1"
  } 
  else{ if(dem04$duree_de_mois[i] < "12"){
    dem04$NBHEUR_AR6bis[i] <- "0"
  } 
  }
}

# 12th month:
dem04$NBHEUR_AR12bis <- matrix("NA", nrow =length(dem04$nbheur_ar12), ncol =1)
# Creating a dummy for number of hours worked:
for (i in 1:length(dem04$nbheur_ar12)){
  if (dem04$duree_de_mois[i] >= "12"){
    dem04$NBHEUR_AR12bis[i] <- "1"
  } 
  else{ if(dem04$duree_de_mois[i] < "12"){
    dem04$NBHEUR_AR12bis[i] <- "0"
  } 
  }
}
dem04$NBHEUR_AR12bis <- matrix("NA", nrow =length(dem04$nbheur_ar12), ncol =1)


####### 1er mois de RA
mytable <- table(dem04$NBHEUR_AR1bis, dem04$duree_de_mois)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of number of hours worked for 1st RA month for people unemp for over 12months", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

####### 3eme mois de RA
mytable <- table(dem04$NBHEUR_AR3bis, dem04$duree_de_mois)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of number of hours worked for 1st RA month for people unemp for over 12months", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

#######
mytable <- table(dem04$NBHEUR_AR6bis, dem04$duree_de_mois)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of number of hours worked for 1st RA month for people unemp for over 12months", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

mytable <- table(dem04$NBHEUR_AR12bis, dem04$duree_de_mois)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of number of hours worked for 1st RA month for people unemp for over 12months", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)



















## FIRST REDUCED ACTIVITY PER EXPERIENCE

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY PER UNEMPLOYMENT DURATION

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY PER LENGTH OF COMPENSATION

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY PER PAST RA BEHAVIOR

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY PER ACTIVITY SECTOR

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY PER SUBSCRIPTION MOTIVES

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY WHETHER THE INDIVIDUAL IS A TEMPORARY WORKER OR NOT

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY PER QUALIFICATION LEVEL

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY PER GENDER

mytable <- table(dem04$DUREE_AVT_AR1, dem04$AGE)
x <- round(prop.table(mytable), digits=4) * 100

barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)