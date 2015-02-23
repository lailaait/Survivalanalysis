### HOURLY DISTRIBUTIONS FOR SUBGROUP (UNEMPLOYED FOR OVER 12 MONTHS AND HAVING A RA)

dat1204 <- read.csv("chom12mois04.csv", header=T, sep=",")

dem1204 <- tbl_df(dat1204)

mytable <- table(dem1204$salpourc_ar1)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, main="Distribution of RA replacement rates, 1st RA month (2004)", xlab="Replacement rates", ylab="Frequency", col = "lightblue", xlim=c(0,200), ylim=c(0,6))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

mytable <- table(dem1204$salpourc_ar6)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, main="Distribution of RA replacement rates, 6th RA month (2004)", xlab="Replacement rates", ylab="Frequency", col = "lightblue", xlim=c(0,150), ylim=c(0,6))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

mytable <- table(dem1204$salpourc_ar12)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, main="Distribution of RA replacement rates, 12th RA month (2004)", xlab="Replacement rates", ylab="Frequency", col = "lightblue", xlim=c(0,150), ylim=c(0,6))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

dat1208 <- read.csv("chom12mois08.csv", header=T, sep=",")

dem1208 <- tbl_df(dat1208)

mytable <- table(dem1208$salpourc_ar1)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, main="Distribution of RA replacement rates, 1st RA month (2008)", xlab="Replacement rates", ylab="Frequency", col = "lightblue", xlim=c(0,200), ylim=c(0,6))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

mytable <- table(dem1208$salpourc_ar6)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, main="Distribution of RA replacement rates, 6th RA month (2008)", xlab="Replacement rates", ylab="Frequency", col = "lightblue", xlim=c(0,150), ylim=c(0,6))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

mytable <- table(dem1208$salpourc_ar12)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, main="Distribution of RA replacement rates, 12th RA month (2008)", xlab="Replacement rates", ylab="Frequency", col = "lightblue", xlim=c(0,150), ylim=c(0,6))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)



## FIRST REDUCED ACTIVITY PER UNEMPLOYMENT DURATION

mytable <- table(dem04$duree_avt_ar1)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, beside = T, main="Distribution of duration before RA in 2004", xlab="Duration", ylab="Frequency", col = "lightblue", ylim=c(0,25))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

mytable <- table(dem08$duree_avt_ar1)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, beside = T, main="Distribution of duration before RA in 2008", xlab="Duration", ylab="Frequency", col = "lightblue", ylim=c(0,25))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)



## FIRST REDUCED ACTIVITY PER LENGTH OF COMPENSATION

mytable <- table(dem04$DUREE_AVT_AR1, dem04$INDEM_RESTANT)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

## FIRST REDUCED ACTIVITY PER PAST RA BEHAVIOR

mytable <- table(dem04$DUREE_AVT_AR1, dem04$passe_duree_avt_ar1)
x <- round(prop.table(mytable), digits=4) * 100
barplot(x, beside = T, main="Distribution of unemployment motives in 2004", xlab="Type of contract", ylab="Frequency", col = "lightblue", ylim=c(0,100))
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)
