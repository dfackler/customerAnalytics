# transactions in-class mini case

trans <- read.csv("/Users/david/Downloads/transactions2.csv")
head(trans)
summary(trans)

fit1 <- lm(Order_Quantity~.-Customer, data = trans)
summary(fit1)

fit2 <- lm(Order_Quantity~Gender*Married*Income*Loyalty*Lag_Purchase*Lag_Order_Quantity, data = trans)
summary(fit2)

fit3 <- lm(Order_Quantity~Gender*Married*Income*Loyalty*Lag_Order_Quantity, data = trans)
summary(fit3)

fit4 <- lm(Order_Quantity~.-Customer+Income*Lag_Order_Quantity-Loyalty-Married-1, data = trans)
summary(fit4)

ct3 <- ctree(Order_Quantity~.-Customer, data = trans)
plot(ct3)

cvFit(fit1, data=trans, y=trans$Order_Quantity, K=5, R=50) #90.88597 
cvFit(fit2, data=trans, y=trans$Order_Quantity, K=5, R=50) #104.4769 
cvFit(fit3, data=trans, y=trans$Order_Quantity, K=5, R=50) #95.24658 
cvFit(fit4, data=trans, y=trans$Order_Quantity, K=5, R=50) #89.80307 
