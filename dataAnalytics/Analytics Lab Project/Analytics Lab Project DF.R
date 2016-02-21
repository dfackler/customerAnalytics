rm(list = ls())

library(dplyr)
#install.packages("plm")
library(plm)

countries <- read.csv("country_data.csv", stringsAsFactors = FALSE)
gov <- read.csv("government_structure.csv")
religion <- read.csv("statereligion.csv")

str(countries)
str(gov)
str(religion)

# clean country names
gov$country <- as.character(gov$country)
gov <- gov[-c(42, 152), ]
for (i in 1:nrow(gov)){
  gov$country[i] <- tolower(substr(gov$country[i], 2, nchar(gov$country[i])))
}

# clean religion
religion <- religion[,-c(6,7)]
religion$country <- as.character(religion$country)
for (i in 1:nrow(religion)){
  religion$country[i] <- tolower(religion$country[i])
}

# join tables to add in goverment attributes
newCountry <- left_join(countries, gov,by = "country" )
newCountry <- left_join(newCountry, religion, by = "country")
str(newCountry)

# write.csv(newCountry, file = "country_w_gov_relig.csv")

# religion only subset - 17 missing countries
relig <- newCountry %>% select(country, gdp_us, year, religion90, religion00, religion10, religionsequence)
# some variables not defined because of singularities - check for multicollinearity (hard with categorical)
fit.relig <- lm(gdp_us~.-country, data = relig)
summary(fit.relig)

# only muslim and none are significant - change options to muslim, none, or other
levels(relig$religion90) <- c(levels(relig$religion90), "other")
levels(relig$religion00) <- c(levels(relig$religion00), "other")
levels(relig$religion10) <- c(levels(relig$religion10), "other")

for(i in 1:nrow(relig)){
  if(relig$religion90[i] != "Muslim" & relig$religion90[i] != "none" & is.na(relig$religion90[i]) == FALSE){
    relig$religion90[i] = "other"
  }
  if(relig$religion00[i] != "Muslim" & relig$religion00[i] != "none" & is.na(relig$religion00[i]) == FALSE){
    relig$religion00[i] = "other"
  }
  if(relig$religion10[i] != "Muslim" & relig$religion10[i] != "none" & is.na(relig$religion10[i]) == FALSE){
    relig$religion10[i] = "other"
  }
}

# only religion 90 is significant, religion00 and religion10 are perfectly colinear or insignificant
fit.relig2 <- lm(gdp_us~religion90, data = relig)
summary(fit.relig2)

# change religon to agree with panel data
relig$religion <- rep(NA, nrow(relig))
for(i in 1:nrow(relig)){
  if(relig$year[i] >= 2010){
    relig$religion[i] <- as.character(relig$religion10[i])
  }
  else if(relig$year[i] >= 2000){
    relig$religion[i] <- as.character(relig$religion00[i])
  }
  else{
    relig$religion[i] <- as.character(relig$religion90[i])
  }
}
# add religion column to newCountry and remove other religion columns
newCountry$religion <- relig$religion
#newCountry <- newCountry[,-c(55:58)]
newCountry$religion <- as.factor(newCountry$religion)
fit.relig3 <- lm(gdp_us~religion, data = newCountry)
summary(fit.relig3)

# regression on government variables
fit <- lm(nextyr_gdp~constitutional.form+head.of.state+basis.of.executive, data = newCountry)
summary(fit)

govAndRelig <- select(newCountry, constitutional.form,head.of.state,basis.of.executive,religion)
str(govAndRelig)

# create column to check if row has missing values
newCountry$complete <- rep(0, nrow(newCountry))
newCountry$missing <- rep(0, nrow(newCountry))
for(i in 1:nrow(newCountry)){
  complete <- 1
  missing <- 0
  for(j in 1:length(newCountry)){
    if(is.na(newCountry[i,j])){
      complete <- 0
      missing <- missing + 1
    }
  }
  newCountry$complete[i] <- complete
  newCountry$missing[i] <- missing
}



write.csv(newCountry, "countriesReligGovPercent")
# 249 rows are complete
# 147 rows are missing more than 6 entries
sum(newCountry$complete)
summary(newCountry$missing)
nrow(newCountry[newCountry$missing > 6,])

# columns with more than 75 missing values
## net_tax_on_priducts # gross_expenditure # gas_price #
## manufacturing_added # trademarks_apps_total # religion
countryClean <- filter(newCountry, missing < 7)
summary(countryClean)
nrow(countryClean[countryClean$missing == 0,])

# regression on clean data set
# notes: run on gdp/capita?
fit1 <- lm(nextyr_gdp~constitutional.form+head.of.state+basis.of.executive, data = newCountry)
summary(fit)

fit2 <- lm(nextyr_gdp~religion, data = newCountry)
summary(fit2)

fit3 <- lm(nextyr_gdp~year+age_dependency+agri_land+co2_emission+consumption_expenditure+
             deposit_int_rate+electricity_consumption+electricity_generation+foreign_investment+exchange_rate+
             gas_price+industry_value_added+inflation+lend_int_rate+manufacturing_added+
             natural_resources_rent+imports_goods_of_gdp+imports_metals_of_gdp+population_growth+primary_edu_pupils+primary_edu_teachers+
             rural_population+science_articles+seconday_edu_progression+services_value_added+
             technical_grants+total_roads+trade_of_gdp+unemployment+urban_population+
             worker_compensation, data = countryClean)
summary(fit3)


# columns to add:
newCountry$total_population <- newCountry$urban_population+newCountry$rural_population
newCountry$urban_percentage <- newCountry$urban_population/newCountry$total_population
newCountry$co2_emissions_per_capita <- newCountry$co2_emission/newCountry$total_population
newCountry$primary_edu_pupils_per_capita <- newCountry$primary_edu_pupils/newCountry$total_population
newCountry$primary_edu_teachers_per_capita <- newCountry$primary_edu_teachers/newCountry$total_population
newCountry$consumption_expenditure_per_capita <- newCountry$consumption_expenditure/newCountry$total_population
newCountry$gross_expenditure_per_capita <- newCountry$gross_expenditure/newCountry$total_population
newCountry$worker_compensation_per_capita <- newCountry$worker_compensation/newCountry$total_population
newCountry$gdp_per_capita <- newCountry$gdp_us/newCountry$total_population


# fit on per capita and ratio columns
fit4 <- lm(nextyr_gdp~urban_population+co2_emission+primary_edu_teachers+
             primary_edu_pupils+consumption_expenditure+
             gross_expenditure+worker_compensation, data = newCountry)
summary(fit4)

# fit on absolute columns
fit5 <- lm(nextyr_gdp~urban_percentage+co2_emissions_per_capita+primary_edu_teachers_per_capita+
             primary_edu_pupils_per_capita+consumption_expenditure_per_capita+
             gross_expenditure_per_capita+worker_compensation_per_capita, data = newCountry)
summary(fit5)

# panel fit on absolute columns
fit6 <- plm(nextyr_gdp~urban_population+primary_edu_teachers+
             worker_compensation+consumption_expenditure, data = newCountry, index = c("country", "year"),
            model = "within")
summary(fit6)

# panel fit on per capita and ratio columns
fit7 <- plm(nextyr_gdp~urban_percentage+primary_edu_teachers_per_capita+
             consumption_expenditure_per_capita+
             gross_expenditure_per_capita+worker_compensation_per_capita, data = newCountry,
            index = c("country", "year"), model = "within")
summary(fit7)

# regression on religion and government
fit8 <- lm(nextyr_gdp~constitutional.form+head.of.state+basis.of.executive+religion, data = 
             newCountry)
summary(fit8)
table(newCountry$religion)
summary(newCountry$constitutional.form)
# regression on gdp
fit9 <- lm(nextyr_gdp~gdp_us, data = newCountry)
summary(fit9)


