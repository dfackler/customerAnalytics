rm (list= ls())

library(dplyr)

charities <- read.csv("charitiesCombined.csv", stringsAsFactors = FALSE)
econ <- read.csv("economicFactors.csv")

# get rid of entries where ein is NA
charities.clean <- filter(charities, !is.na(ein))
# match missing names
for (i in 1:nrow(charities.clean)){
  if (is.na(charities.clean$name[i]) | charities.clean$name[i] == ""){
    charities.clean$name[i] <- charities.clean$primary_name_of_organization[i]
  }
}
for (i in 1:nrow(charities.clean)){
  if (is.na(charities.clean$primary_name_of_organization[i]) | charities.clean$primary_name_of_organization[i] == ""){
    charities.clean$primary_name_of_organization[i] <- charities.clean$name[i]
  }
}
# match missing assets
for (i in 1:nrow(charities.clean)){
  if (is.na(charities.clean$assets[i]) | charities.clean$assets[i] == 0){
    charities.clean$assets[i] <- charities.clean$asset_amount[i]
  }
}
# change year column to be only year - take out month
for (i in 1:nrow(charities.clean)){
  charities.clean$year[i] <- substring(charities.clean$year[i], 1, 4)
}

# Take out columns with many NAs
#transpose it
charities.vars<-data.frame(t(charities.clean))
#add a column that counts the NAs
for (i in 1:nrow(charities.vars)){
  charities.vars$nullcount[i] <- sum(is.na(charities.vars[i,]))
}
#david's magic...creates a vector of the numbers of the columns we want to drop in 
#charities.clean
missing<-0
j=1
for (i in 1:nrow(charities.vars)){
  if (charities.vars$nullcount[i]>=160){
    missing[j]=i
    j=j+1
  }
}
#we drop the columns that have more than half the data 
charities.cleaner<-charities.clean[,-missing,]

# change Y and N to 1 and 0
for (i in 1:nrow(charities.clean)){
  for (j in 1:length(charities.clean)){
    if (charities.clean[i,j] == "Y" & !is.na(charities.clean[i,j])){
      charities.clean[i,j] <- 1
    } else if (charities.clean[i,j] == "N" & !is.na(charities.clean[i,j])){
      charities.clean[i,j] <- 0
    }
  }
}

# add in economic data
econ$year <- as.character(econ$year)
econ[,2:6] <- scale(econ[,2:6])
charities.clean <- left_join(charities.clean, econ, by = "year")

# change columns to factors
charities.clean$subsection_descrip <- as.factor(charities.clean$subsection_descrip)
charities.clean$exempt_descrip <- as.factor(charities.clean$exempt_descrip)
charities.clean$state <- as.factor(charities.clean$state)

# create total grant out column
charities.clean$totgrantsOut <- charities.clean$grntstogovt + charities.clean$grnsttoindiv + 
                            charities.clean$grntstofrgngovt
summary(charities.clean$totgrants)
# create total grant in column
charities.clean$totgrantsIn <- charities.clean$gftgrntsrcvd170 + charities.clean$totgftgrntrcvd509
# create tot grants out/total donations
charities.clean$donateratio <- charities.clean$totgrantsOut/charities.clean$totcntrbgfts
# create fundraising efficiency column
charities.clean$fundeffic <- charities.clean$netincfndrsng/charities.clean$lessdirfndrsng

# find grant variables
charities.grants <- select(charities.clean, year, name, assets, income_amount, totgrantsIn,
                           totgrantsOut, grantratio, grntstogovt, grnsttoindiv, grntstofrgngovt,
                           pldgegrntrcvblend, grntspayableend, gftgrntsrcvd170, totgftgrntrcvd509,
                           frgngrntscd, frgnaggragrntscd, rptgrntstogovtcd, rptgrntstoindvcd, subsection_descrip
                           )
# find financial variables
charities.finance <- select(charities.clean, year, name, donateratio, totfuncexpns, income_amount, revandgrants, totcntrbgfts,
                            revminusexp, totrevenue, totgrantsIn, totgrantsOut)

# create grant data set
charities.grants <- filter(charities.clean, totgrantsOut > 0)
ks.test(charities.clean$totcntrbgfts, charities.grants$totcntrbgfts)

# create scaled data set
charities.scale <- charities.clean
charities.scale$totcntrbgfts <- scale(charities.scale$totcntrbgfts)[1]
charities.scale$totgrantsIn <- scale(charities.scale$totgrantsIn)[1]
charities.scale$totgrantsOut <- scale(charities.scale$totgrantsOut)[1]
charities.scale$profndraising <- scale(charities.scale$profndraising)[1]
charities.scale$fundeffic <- scale(charities.scale$fundeffic)[1]

# create scaled data set for grants
charities.scale.grants <- charities.grants
charities.scale.grants$totcntrbgfts <- scale(charities.scale.grants$totcntrbgfts)
charities.scale.grants$totgrantsIn <- scale(charities.scale.grants$totgrantsIn)
charities.scale.grants$totgrantsOut <- scale(charities.scale.grants$totgrantsOut)
charities.scale.grants$profndraising <- scale(charities.scale.grants$profndraising)
charities.scale.grants$fundeffic <- scale(charities.scale.grants$fundeffic)

charities.scale.grants <- select(charities.scale.grants, year, name, totcntrbgfts,
                                 totgrantsIn, totgrantsOut, profndraising, fundeffic,
                                 lbbyingactvtscd, noindiv100kcnt, nocontractor100kcnt,
                                 rptincfnndrsngcd)

#### dependent variable as absolute grants out ####

# percent given out versus donated
summary(lm(totgrantsOut~totcntrbgfts, data = charities.grants))
# percent given out versus donated with lobbying
summary(lm(totgrantsOut~totcntrbgfts*lbbyingactvtscd, data = charities.scale.grants))
# # percent given out versus donated with officers
# summary(lm(totgrantsOut~totcntrbgfts*servasofficercd, data = charities.grants))
# percent given out versus donated controlling for highly paid indiv and contractors
summary(lm(totgrantsOut~totcntrbgfts+noindiv100kcnt+nocontractor100kcnt, 
           data = filter(charities.grants, noindiv100kcnt < 50 & nocontractor100kcnt < 50)))
# percent given out versus donated controlling for deductibility - exempt_descrip does stuff
summary(lm(totgrantsOut~totcntrbgfts + subsection_descrip + deductability_descrip +
             exempt_descrip, data = charities.grants))
# percent given out versus donated controlling for state - charities in Ohio give out significantly more
summary(lm(totgrantsOut~totcntrbgfts+state, data = charities.grants))
# percent given out versus donated on professional fundraising
summary(lm(totgrantsOut~totcntrbgfts*profndraising*rptincfnndrsngcd, data = charities.grants))

summary(charities.grants$noindiv100kcnt)
boxplot(charities.grants$noindiv100kcnt,
        main = "# Individuals Above $100k")
summary(charities.grants$nocontractor100kcnt)
boxplot(charities.grants$nocontractor100kcnt,
        main = "# Contractors Above 100k")


#### dependent variable as fundraising efficiency ####
charities.fund <- filter(charities.clean, !is.na(fundeffic) & fundeffic != Inf)
ks.test(charities.clean$totcntrbgfts, charities.fund$totcntrbgfts)
# fundraising efficiency on professional fundraising and fundraising activities - not significant
summary(lm(netincfndrsng~profndraising*rptincfnndrsngcd, data = charities.fund))

#### dependent variable as total grants in ####
# totgrantsIn on professional fundraising and fundraising activities - professional fundraising good
summary(lm(totgrantsIn~profndraising*rptincfnndrsngcd, data = charities.fund))
# totgrantsIn on lobbying efforts - lobbying has a positive impact on totgrantsIn
summary(lm(totgrantsIn~lbbyingactvtscd, data = charities.clean))

summary(charities.fund$profndraising)
boxplot(charities.fund$profndraising)

#### dependent variable as total donations ####
# total donations on economic factors - not significant
summary(lm(totcntrbgfts~unemploymentRate,
           data = charities.clean))
# total donations on economic factors - not significant
summary(lm(totgrantsIn~personalPerCapitaIncome,
           data = charities.scale))





# percent given out versus donated
summary(lm(totgrantsOut~totcntrbgfts, data = charities.grants))
# percent given out versus donated with lobbying
summary(lm(totgrantsOut~totcntrbgfts*lbbyingactvtscd, data = charities.scale.grants))
# percent given out versus donated controlling for highly paid indiv and contractors
summary(lm(totgrantsOut~totcntrbgfts+noindiv100kcnt+nocontractor100kcnt, 
           data = filter(charities.grants, noindiv100kcnt < 50 & nocontractor100kcnt < 50)))
# fundraising efficiency on professional fundraising and fundraising activities - not significant
summary(lm(netincfndrsng~profndraising*rptincfnndrsngcd, data = charities.fund))