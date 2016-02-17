rm(list = ls())
library(dplyr)
library(ggplot2)

cust <- read.csv("maru data students final.csv")

# 1. aquisition cost
acq <- cust$contact.cost/cust$response.rate
cust$aquisition.cost <- acq

# 2. break-even
# NOTE: does NOT take into account retention
year.rev <- cust$price.per.hr*cust$annual.hours
year.cost <- (cust$workers.needed*cust$worker.labor.cost.per.hr + 
            cust$instructors.needed*cust$instructor.labor.cost.per.hr)*cust$annual.hours
year.mar <- year.rev-year.cost
be <- cust$aquisition.cost/year.mar
cust$be.years <- be

# 3. lifetime value 
# NOTE: do NOT include 100% attrition after annual profits fall below 100
# payment is assumed to be up front, for this reason first clv model is chosen
# clv = M(1+d)/1+d-r
clv.disc <- year.mar*(1+cust$interest.rate)/(1+(cust$interest.rate-cust$retention.rate))
cust$clv <- clv.disc

# clv with aquisition cost accounted for
clv.disc.aqc <- cust$clv-cust$aquisition.cost
cust$clv.aqc <- clv.disc.aqc
# 3.d.i - YES, Maru should throw the gala

# 4. Elite ballplayers are the most attractive segment to target because they have the highest clv
#    both when throwing the gala and when printing ads. Throwing the gala gives the higher clv
#    when accounting for aquisition cost however

# 5. clv is lower for chiyoda, but not half as much. with twice the number of little leaguers
#    Maru SHOULD sponser chiyoda over minato 
chiyoda <- cust[1,]
chiyoda[,1] = "little leaguers (chiyoda)"
chiyoda[,3] = .08
chiyoda[,10] = .65
chiyoda[,2] = 600
cust <- rbind(cust, chiyoda[1,])

# rerun code to update variables for chiyoda little leaguers
acq <- cust$contact.cost/cust$response.rate
cust$aquisition.cost <- acq

year.rev <- cust$price.per.hr*cust$annual.hours
year.cost <- (cust$workers.needed*cust$worker.labor.cost.per.hr + 
                cust$instructors.needed*cust$instructor.labor.cost.per.hr)*cust$annual.hours
year.mar <- year.rev-year.cost
be <- cust$aquisition.cost/year.mar
cust$be.years <- be

clv.disc <- year.mar*(1+cust$interest.rate)/(1+(cust$interest.rate-cust$retention.rate))
cust$clv <- clv.disc

clv.disc.aqc <- cust$clv-cust$aquisition.cost
cust$clv.aqc <- clv.disc.aqc

# 6. No Maru should not offer the discount, the clv is lower than without the discount
elite.disc <- cust[4,]
elite.disc[,1] <- "elite ballplayers (discount)"
elite.disc[,8] <- 7000
elite.disc[,10] <- .75
cust <- rbind(cust,elite.disc[1,])
row.names(cust)[7] <- "7"

# rerun code to update variables for elite ballplayers with discount
# NOTE: must take into account extra 500 in first year
acq <- cust$contact.cost/cust$response.rate
cust$aquisition.cost <- acq

year.rev <- cust$price.per.hr*cust$annual.hours
year.cost <- (cust$workers.needed*cust$worker.labor.cost.per.hr + 
                cust$instructors.needed*cust$instructor.labor.cost.per.hr)*cust$annual.hours
year.mar <- year.rev-year.cost
be <- cust$aquisition.cost/year.mar
cust$be.years <- be

clv.disc <- year.mar*(1+cust$interest.rate)/(1+(cust$interest.rate-cust$retention.rate))
cust$clv <- clv.disc
cust$clv[7] <- cust$clv[7]+500 #add 500 for first year

clv.disc.aqc <- cust$clv-cust$aquisition.cost
cust$clv.aqc <- clv.disc.aqc

# 7. No they should not offer this discount, it will cause clv to become negative after
#    taking into account aquisition cost
elite.bat <- cust[4,]
elite.bat[,1] <- "elite ballplayers (bat)"
elite.bat[,2] <- elite.bat[,2]+10000
elite.bat[,3] <- .29
cust <- rbind(cust,elite.bat[1,])
row.names(cust)[8] <- "8"

# rerun code to update variables for elite ballplayers with bat promotion
acq <- cust$contact.cost/cust$response.rate
cust$aquisition.cost <- acq

year.rev <- cust$price.per.hr*cust$annual.hours
year.cost <- (cust$workers.needed*cust$worker.labor.cost.per.hr + 
                cust$instructors.needed*cust$instructor.labor.cost.per.hr)*cust$annual.hours
year.mar <- year.rev-year.cost
be <- cust$aquisition.cost/year.mar
cust$be.years <- be

clv.disc <- year.mar*(1+cust$interest.rate)/(1+(cust$interest.rate-cust$retention.rate))
cust$clv <- clv.disc

clv.disc.aqc <- cust$clv-cust$aquisition.cost
cust$clv.aqc <- clv.disc.aqc

####################Senstivity Analysis###################
# create alternative scenario vectors
elite.ac <- seq(40000, 60000, by = 5000)
elite.am <- seq(24000, 36000, by = 3000)
elite.rr <- seq(.48, .72, by = .06)

# create data frame of scenarios
elite.alt <- expand.grid(aquisition = elite.ac, margin = elite.am, retention = elite.rr)

# calculate CLV
interest.rate <- .1
elite.alt$clv <- elite.alt$margin*(1+interest.rate)/(1+(interest.rate-elite.alt$retention)
                                                     )-elite.alt$aquisition

# graph negative clv - 15.2%
nrow(filter(elite.alt, clv<5714.29))/nrow(elite.alt)

# graph relationship between inputs and clv
qplot(aquisition, clv, data = filter(elite.alt, margin == 30000 & retention == .48), geom = "line")
qplot(margin, clv, data = filter(elite.alt, aquisition == 50000 & retention == .48), geom = "line")
qplot(retention, clv, data = filter(elite.alt, margin == 30000 & aquisition == 50000), geom = "line")

# percent when clv lower than little leaguers - 33.4% of time
nrow(filter(elite.alt, clv<5714))/nrow(elite.alt)
