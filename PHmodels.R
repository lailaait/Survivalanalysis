
############################

library(muhaz)

# PRATIQUES EXTENSIVES (decision to start a RA):

MPH04 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=2, min.time=0, max.time=NA)
MPH05 <- pehaz(dem05$duree_avt_ar1, delta=dem05$censur_ar1, width=2, min.time=0, max.time=NA)
MPH06 <- pehaz(dem06$duree_avt_ar1, delta=dem06$censur_ar1, width=2, min.time=0, max.time=NA)
MPH08 <- pehaz(dem08$duree_avt_ar1, delta=dem08$censur_ar1, width=2, min.time=0, max.time=NA)
MPH10 <- pehaz(dem10$duree_avt_ar1, delta=dem10$censur_ar1, width=2, min.time=0, max.time=NA)


plot(MPH04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(MPH06, axes=FALSE,col='red', main="MPH model, piecewise exponential constant (binwidth=2)", xlab="Time", ylab="Hazard Rate")
legend("topright", c("2004","2006"), col=(1:2), lwd=0.5)


plot(MPH04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(MPH05, ann=FALSE, axes=FALSE,col='red')
par(new=TRUE)
plot(MPH06, axes=FALSE,col='green',  main="MPH model, piecewise exponential constant (binwidth=2)", xlab="Time", ylab="Hazard Rate")
legend("topright", c("2004","2005","2006"), col=(1:3), lwd=0.5)



# Pehaz measures piecewise exponential with bins of equal size:
MPH04 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=2, min.time=0, max.time=NA)
MPH05 <- pehaz(dem05$duree_avt_ar1, delta=dem05$censur_ar1, width=2, min.time=0, max.time=NA)
MPH06 <- pehaz(dem06$duree_avt_ar1, delta=dem06$censur_ar1, width=2, min.time=0, max.time=NA)
MPH08 <- pehaz(dem08$duree_avt_ar1, delta=dem08$censur_ar1, width=2, min.time=0, max.time=NA)
MPH10 <- pehaz(dem10$duree_avt_ar1, delta=dem10$censur_ar1, width=2, min.time=0, max.time=NA)

plot(MPH04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(MPH05, ann=FALSE, axes=FALSE,col='red')
par(new=TRUE)
plot(MPH06, axes=FALSE,col='green',  main="MPH model, piecewise exponential constant (binwidth=2)", xlab="Time", ylab="Hazard Rate")
legend("topright", c("2004","2005","2006"), col=(1:3), lwd=0.5)


# RISK FIRST RA
MPH2 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=2, min.time=0, max.time=2)
MPH2
0.05171039
MPH3 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=1, min.time=3, max.time=4)
MPH3
0.1127817
MPH4 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=1, min.time=5, max.time=6)
MPH4
0.07010622
MPH5 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=2, min.time=7, max.time=9)
MPH5
0.08171297
MPH6 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=2, min.time=10, max.time=12)
MPH6
0.06697469
MPH7 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=5, min.time=13, max.time=18)
MPH7
0.04915905
MPH8 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=5, min.time=19, max.time=24)
MPH8
0.03925707
MPH9 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=11, min.time=25, max.time=36)
MPH9
0.05361781
MPH10 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=24, min.time=36, max.time=60)
MPH10
0.06290048

## create a graph with respect to interval selection as the one picked in Fremigacci & Terracol (2013):



### PRATIQUES INTENSIVES DACTIVITE REDUITE

MPH04 <- pehaz(dem04$duree_avt_ar1, delta=dem04$PREM_SUP_110, width=2, min.time=0, max.time=NA)
MPH06 <- pehaz(dem06$duree_avt_ar1, delta=dem06$PREM_SUP_110, width=2, min.time=0, max.time=NA)
MPH08 <- pehaz(dem08$duree_avt_ar1, delta=dem08$PREM_SUP_110, width=2, min.time=0, max.time=NA)
MPH10 <- pehaz(dem10$duree_avt_ar1, delta=dem10$PREM_SUP_110, width=2, min.time=0, max.time=NA)

plot(MPH04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(MPH06, ann=FALSE, axes=FALSE,col='red')
par(new=TRUE)
plot(MPH08, axes=FALSE,col='green')
par(new=TRUE)
plot(MPH10, axes=FALSE,col='blue',main="MPH model, piecewise exponential constant (binwidth=2)", xlab="Time", ylab="Hazard Rate")
legend("topright", c("2004","2006","2008", "2010"), col=(1:4), lwd=0.5)



MPH04 <- pehaz(dem04$duree_avt_ar1, delta=dem04$PREM_SUP_136, width=2, min.time=0, max.time=NA)
MPH06 <- pehaz(dem06$duree_avt_ar1, delta=dem06$PREM_SUP_136, width=2, min.time=0, max.time=NA)
MPH08 <- pehaz(dem08$duree_avt_ar1, delta=dem08$PREM_SUP_136, width=2, min.time=0, max.time=NA)
MPH10 <- pehaz(dem10$duree_avt_ar1, delta=dem10$PREM_SUP_136, width=2, min.time=0, max.time=NA)

plot(MPH04, lwd=3, col="black", ann=FALSE)
par(new=TRUE)
plot(MPH06, ann=FALSE, axes=FALSE,col='red')
par(new=TRUE)
plot(MPH08, axes=FALSE,col='green')
par(new=TRUE)
plot(MPH10, axes=FALSE,col='blue',main="MPH model, piecewise exponential constant (binwidth=2)", xlab="Time", ylab="Hazard Rate")
legend("topright", c("2004","2006","2008", "2010"), col=(1:4), lwd=0.5)

