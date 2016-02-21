rm(list = ls())

setwd("/Users/dfackler/Desktop/R")

hotel1 <- read.csv("Bodea - Choice based Revenue Management - Data Set - Hotel 1.csv", stringsAsFactors = FALSE)
hotel2 <- read.csv("Bodea - Choice based Revenue Management - Data Set - Hotel 2.csv", stringsAsFactors = FALSE)
hotel3 <- read.csv("Bodea - Choice based Revenue Management - Data Set - Hotel 3.csv", stringsAsFactors = FALSE)
hotel4 <- read.csv("Bodea - Choice based Revenue Management - Data Set - Hotel 4.csv", stringsAsFactors = FALSE)
hotel5 <- read.csv("Bodea - Choice based Revenue Management - Data Set - Hotel 5.csv", stringsAsFactors = FALSE)

install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("outliers")
library(tidyr)
library(dplyr)
library(ggplot2)
library(outliers)

# create hotels dataframe with all hotel data
hotels <- hotel1
hotels <- rbind(hotels, hotel2)
hotels <- rbind(hotels, hotel3)
hotels <- rbind(hotels, hotel4)
hotels <- rbind(hotels, hotel5)

# calculate discount rate
hotels$Discount_Rate <- (hotels$Arrival_Rate-hotels$Nightly_Rate)/hotels$Arrival_Rate

# create hotels.clean
# remove advance purchase == -1
# remove merge indicator == 0
hotels.clean <- filter(hotels, Advance_Purchase >= 0, Merge_Indicator == 1)

# create room type code for double, queen, king, suite, other
hotels.clean$Room_Code <- 0
for(i in 1:nrow(hotels.clean)){
  if(grepl("Double", hotels.clean$Room_Type[i], ignore.case = TRUE)){
    hotels.clean$Room_Code[i] <- 1
  } else if (grepl("Queen", hotels.clean$Room_Type[i], ignore.case = TRUE)){
    hotels.clean$Room_Code[i] <- 2
  } else if (grepl("King", hotels.clean$Room_Type[i], ignore.case = TRUE)){
    hotels.clean$Room_Code[i] <- 3
  } else if (grepl("Suite", hotels.clean$Room_Type[i], ignore.case = TRUE)){
    hotels.clean$Room_Code[i] <- 4
  }
}

# remove all special rooms
hotels.clean <- filter(hotels.clean, Room_Code > 0)

# change columns to factors
hotels.clean$Room_Code <- as.factor(hotels.clean$Room_Code)
hotels.clean$Membership_Status <- as.factor(hotels.clean$Membership_Status)
hotels.clean$VIP_Membership_Status <- as.factor(hotels.clean$VIP_Membership_Status)

# remove outliers
hotels.outlier <- hotels.clean
LOS.outlier <- outlier(hotels.outlier[,c(9:14, 26)], logical = TRUE)
find.outlier <- which(LOS.outlier == TRUE, arr.ind = TRUE)
hotels.outlier <- hotels.outlier[-find.outlier,]
# remove discout rates of 99%
hotels.outlier <- hotels.outlier[hotels.outlier$Discount_Rate <.9,]
hotels.clean <- hotels.outlier

# create hotels.scale
hotels.scale <- hotels.outlier
hotels.scale[,c(9:14, 26)] <- scale(hotels.outlier[,c(9:14, 26)])

# histograms of aggregated variables
  qplot(Distribution_Channel, data = hotels.clean, geom = "histogram")
  qplot(Advance_Purchase, data = hotels.clean, geom = "histogram")
  qplot(Party_Size, data = hotels.clean, geom = "histogram")
  qplot(Length_of_Stay, data = hotels.clean, geom = "histogram")
  qplot(Nightly_Rate, data = hotels.clean, geom = "histogram")
  qplot(Total_Revenue, data = hotels.clean, geom = "histogram")
  qplot(Purchased_Rate_Code, data = hotels.clean, geom = "histogram")
  qplot(Purchased_Room_Type, data = hotels.clean, geom = "histogram")
  qplot(Merge_Indicator, data = hotels.clean, geom = "histogram")
  qplot(Rate_Code, data = hotels.clean, geom = "histogram")
  qplot(VIP_Membership_Status, data = hotels.clean, geom = "histogram")
  qplot(Discount_Rate, data = hotels.clean, geom = "histogram")
  qplot(Room_Code, data = hotels.clean, geom = "histogram")

# histograms of variables by hotel
  qplot(Distribution_Channel, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Advance_Purchase, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Party_Size, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Length_of_Stay, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Nightly_Rate, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Total_Revenue, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Purchased_Rate_Code, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Purchased_Room_Type, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Merge_Indicator, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Rate_Code, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(VIP_Membership_Status, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Discount_Rate, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  qplot(Room_Code, data = hotels.clean, geom = "histogram", facets = Hotel_ID~.)
  
# histograms of variables by room type
  qplot(Distribution_Channel, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Advance_Purchase, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Party_Size, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Length_of_Stay, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Nightly_Rate, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Total_Revenue, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Purchased_Rate_Code, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Purchased_Room_Type, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Merge_Indicator, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Rate_Code, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(VIP_Membership_Status, data = hotels.clean, geom = "histogram", facets = Room_Code~.)
  qplot(Discount_Rate, data = hotels.clean, geom = "histogram", facets = Room_Code~.)

  
# advance purchase vs. price
  qplot(Nightly_Rate, Discount_Rate,data = hotels.clean, geom = 'point', facets = Room_Code~.)
  qplot(Total_Revenue, Discount_Rate,data = hotels.clean, geom = 'point', facets = Room_Code~.)
  qplot(Advance_Purchase, Discount_Rate, data = hotels.clean, geom = 'point', facets = Room_Code~.)
  
# regression model on discount rate to find important variables
  reg.fit <- lm(Discount_Rate~Advance_Purchase+Party_Size+Length_of_Stay+
                   Number_of_Rooms+Nightly_Rate+Total_Revenue+
                   Purchased_Rate_Code+Arrival_Rate+
                   Rate_Code+Membership_Status+VIP_Membership_Status+
                   Room_Code, data = hotels.scale)
  summary(disc.fit)
  disc.pred <- predict(disc.fit, family = "response")
  (SSE <- sum((hotels.clean$Discount_Rate-disc.pred)^2)/nrow(hotels.clean))

# regression for positive discounts
  disc.fit <- lm(Discount_Rate~Advance_Purchase+Party_Size+Length_of_Stay+
                   Number_of_Rooms+Nightly_Rate+Total_Revenue+
                   Purchased_Rate_Code+Arrival_Rate+
                   Rate_Code+Membership_Status+VIP_Membership_Status+
                   Room_Code, data = filter(hotels.clean, Discount_Rate>=0))
  summary(disc.fit)
    
# regression for premiums
  prem.fit <- lm(Discount_Rate~Advance_Purchase+Party_Size+Length_of_Stay+
                   Number_of_Rooms+Nightly_Rate+Total_Revenue+
                   Purchased_Rate_Code+Arrival_Rate+
                   Rate_Code+Membership_Status+VIP_Membership_Status+
                   Room_Code, data = filter(hotels.clean, Discount_Rate<0))
  summary(prem.fit)
  