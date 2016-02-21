rm(list = ls())
setwd("/Users/david/Desktop/ownSpace/customerAnalytics")
pilgrim <- read.csv("pilgrim A2 data part1.csv")

library(partykit)
library(ggplot2)
library(dplyr)

### PART 1 ###
### Interaction Modeling ###

# 1. Basic regression
# A customer of age 0 with 0 income and 0 years with the bank and who doesn't go online
# will cost the bank on average $91.73.
# If a customer uses online banking they are $18.24 more profitable
# For each increase in age buckets, a customer will become $18.29 more profitable
# For each increase in income buckets, a customer will become $17.85 more profitable
# For each additional year a customer has been at the bank ,they will become $4.03 more profitable
#
# With a p-value of ~0.0000 the model as a whole is significant meaning it explains some significant
# amount of variance above 0%
# The R-squared is .05742 indicating that the model explains 5.74% of the variance in profit
# The residual standard error is 274.6 indicating that the model is on average off by $274.6
# per observation
fit1 <- lm(Profit99~.-ID, data = pilgrim)
summary(fit1)

# 2. Regression with interaction terms for Online
ct1 <- ctree(Profit99~.-ID, data = pilgrim, control = ctree_control(mincriterion = .95))
plot(ct1)
# Viewed tree for interaction terms
# Took out Age99:Inc99 once triple interaction terms added because became insignificant
fit2 <- lm(Profit99~.-ID+
             Online:Age99 + Online99:Inc99 
           , data = pilgrim)
summary(fit2)
# While Online99:Age99 is not significant at a .95 confidence level, it shows with some small
# level of confidence that as a customer increases in age buckets, their profitability for using
# online increases by an additional $4.14
# Online99:Inc99 shows that as a customer increases in income buckets, their profitability for
# using online increases by an additional $8.98
# This model changed the base Online coefficient from $18.24 to -49.19 indicating that Online
# has significant interaction effects with regards to profitability

# 3. Best model
# Interaction model had higher Adjusted R-squared, higher F-statistic
# Enough observations so that additional 2 paramaters is not a concern


# 4. Online banking significantly improves profitability for old, rich customers
# However, it decreases profitability for young, poor customers
# Pilgrim should do targeted advertising/incentives for their old,rich customers to use
# online banking and either charge young, poor customers for online, not allow them to use it,
# or at the very least not advertise the service to them

### PART 2 ###
### Experiment Analysis ###

pilgrim2 <- read.csv("pilgrim A2 data part 2 experiment.csv")
# Control: 1 = experiment; 0 = control?

# 1. Profit comparison
# Summary statistics show that both mean and median profit increased from 99 to 00 
# for the experimental group
# A paired t-test shows that at a confidence level of .005 the two means
# are significantly different for the experimental group (is this the right test?)
# A paired t-test shows that at a confidence level of .005 the two means
# are NOT significantly different for the control group (is this the right test?)
summary(pilgrim2$Profit99[pilgrim2$condition==0])
summary(pilgrim2$Profit00[pilgrim2$condition==0])
summary(pilgrim2$Profit99[pilgrim2$condition==1])
summary(pilgrim2$Profit00[pilgrim2$condition==1])
t.test(pilgrim2$Profit99[pilgrim2$condition==0], pilgrim2$Profit00[pilgrim2$condition==0], paired = TRUE)
t.test(pilgrim2$Profit99[pilgrim2$condition==1], pilgrim2$Profit00[pilgrim2$condition==1], paired = TRUE)

# 2. t-tests show that mean demographics are not significantly different between groups
# Randomization appears successful
ggplot(pilgrim2,aes(x=Profit99,y=Profit00,colour=factor(condition)))+geom_point()+
  stat_smooth(method='lm',formula=y~scale(x,scale=F))
t.test(pilgrim2$Age99[pilgrim2$condition==0], pilgrim2$Age99[pilgrim2$condition==1], paired = TRUE)
t.test(pilgrim2$Inc99[pilgrim2$condition==0], pilgrim2$Inc99[pilgrim2$condition==1], paired = TRUE)
t.test(pilgrim2$Tenure99[pilgrim2$condition==0], pilgrim2$Tenure99[pilgrim2$condition==1], paired = TRUE)

# 3. A t-test shows that the mean profit in 2000 between the two groups is significant
# It appears that migrating people to online was sucessful in increasing profit by on average $51.25
# per customer
t.test(pilgrim2$Profit00[pilgrim2$condition==0], pilgrim2$Profit00[pilgrim2$condition==1], paired = FALSE)

# 4. The idealized scenario is unrealistic for a recommendation. It depends on how much it costs 
# to approach each customer and convince them to use online in addition to what percent of customers
# approached actually swap
# In addition, it does not account for retention in the Online space or satisfaction of the customers
# who swapped to Online. If they are more profitable for a year but become disgruntled with the Online
# service and leave the bank, then this may hurt them in the long run. 
# More data with customers' feedback on the Online service would help generate a more robust recommendation


### PART 3 ###
### Out-of-Sample Prediction ###
pilgrim3 <- filter(pilgrim2, condition==1) %>% select(ID, Profit99, Age99, Inc99, Tenure99, Profit00)
# Age possibly square root? ln? log10? 
ggplot(pilgrim2, aes(x=Age99, y=Profit00)) +
  geom_point(shape=1) +   
  geom_smooth()
# Income possibly exponential?
ggplot(pilgrim2, aes(x=Inc99, y=Profit00)) +
  geom_point(shape=1) +   
  geom_smooth()
# Tenure possibly square root? ln? log10?
ggplot(pilgrim2, aes(x=Tenure99, y=Profit00)) +
  geom_point(shape=1) +   
  geom_smooth()

# base model
fit3 <- lm(Profit00~.-ID, data = pilgrim3)
summary(fit3)

# Transformations for variables
# log(Age99) - .19 significance - MAYBE - .06 on full model - KEEP
# I(Inc99^2) - .00 significance - YES
# log(Tenure99) - .56 significane - NO
# I(Tenure99^.5) - .25 significane - MAYBE - No significant increase in Adj-R^2 on full model
fit3 <- lm(Profit00~.-ID+log(Age99) + I(Inc99^2), data = pilgrim3)
summary(fit3)

# Additional Interaction Terms based on tree
ct2 <- ctree(Profit00~.-ID-Profit99, data = pilgrim3, control = ctree_control(mincriterion = .90))
plot(ct2)
fit4 <- lm(Profit00~.-ID + log(Age99) + I(Inc99^2) + 
             Inc99:Tenure99 + 
             Inc99:Age99 +
             Tenure99:Inc99:Age99, data = pilgrim3)
summary(fit4)

# Removing nonsignificant nonlinear tranformations
fit5 <- lm(Profit00~.-ID + 
             Inc99:Age99 +
             Tenure99:Inc99:Age99, data = pilgrim3)
summary(fit5)

# adding data feature for old, rich customers
pilgrim3$oldRich <- rep(0, times = nrow(pilgrim3))
pilgrim3$oldRich[pilgrim3$Age99>4 & pilgrim3$Inc99>4] <- 1
fit6 <- lm(Profit00~.-ID, data = pilgrim3)
summary(fit6)

# adding oldRich and transformations
fit7 <- lm(Profit00~.-ID+log(Age99) + I(Inc99^2), data = pilgrim3)
summary(fit7)

# adding oldRich and interactions
ct3 <- ctree(Profit00~.-ID-Profit99, data = pilgrim3, control = ctree_control(mincriterion = .90))
plot(ct3)
fit8 <- lm(Profit00~.-ID + 
             Inc99:Age99 +
             Tenure99:Inc99:Age99, data = pilgrim3)
summary(fit8)

# adding oldRich and interactions and transformations
fit9 <- lm(Profit00~.-ID + log(Age99) + I(Inc99^2) + 
             Inc99:Tenure99 + 
             Inc99:Age99 +
             Tenure99:Inc99:Age99, data = pilgrim3)
summary(fit9)

# Predict Profit00 for fit3-fit9
pilgrim3$pred3 <- round(predict(fit3),2)
pilgrim3$pred4 <- round(predict(fit4),2)
pilgrim3$pred5 <- round(predict(fit5),2)
pilgrim3$pred6 <- round(predict(fit6),2)
pilgrim3$pred7 <- round(predict(fit7),2)
pilgrim3$pred8 <- round(predict(fit8),2)
pilgrim3$pred9 <- round(predict(fit9),2)

# 2. While adjusted R-squared is highest for model 5 and 8, the sum of squared residuals is smallest
# for model 9. For this reason, model 9 appears to be the best, however there may be overfitting
# due to the small sample size and large amount of predictors

pilgrim3$resid3 <- pilgrim3$Profit00-pilgrim3$pred3
pilgrim3$resid4 <- pilgrim3$Profit00-pilgrim3$pred4
pilgrim3$resid5 <- pilgrim3$Profit00-pilgrim3$pred5
pilgrim3$resid6 <- pilgrim3$Profit00-pilgrim3$pred6
pilgrim3$resid7 <- pilgrim3$Profit00-pilgrim3$pred7
pilgrim3$resid8 <- pilgrim3$Profit00-pilgrim3$pred8
pilgrim3$resid9 <- pilgrim3$Profit00-pilgrim3$pred9

sum(pilgrim3$resid3^2)
sum(pilgrim3$resid4^2)
sum(pilgrim3$resid5^2)
sum(pilgrim3$resid6^2)
sum(pilgrim3$resid7^2)
sum(pilgrim3$resid8^2)
sum(pilgrim3$resid9^2) # best prediction


# 3. 
set.seed(12345)

 
