
############################

library(muhaz)
# Pehaz measures piecewise exponential with bins of equal size:
MPH1 <- pehaz(dem04$INDEM_RESTANT, delta=dem04$censur_ar1, width=2, min.time=0, max.time=NA)

# Pehaz measures piecewise exponential with bins of equal size:
MPH1 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=2, min.time=0, max.time=NA)
plot(MPH1, xlab="Time", ylab="Hazard Rate")
MPH2 <- pehaz(dem04$duree_avt_ar1, delta=dem04$censur_ar1, width=3, min.time=0, max.time=NA)
plot(MPH2, xlab="Time", ylab="Hazard Rate")
# note that ticks represent censored obs. + pointwise 95% confidence intervals are rep too

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
## import table