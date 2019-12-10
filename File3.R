x = read.csv(file.choose())
View(x)
summary(x)
plot(x$Delivery.Time, x$Sorting.Time)
cor(Delivery.Time, Sorting.Time)
reg = lm(Sorting.Time ~ Delivery.Time)
summary(reg)
reg$residuals
pred = predict(reg)
sum(reg$residuals)
mean(reg$residuals)
mean(sum(reg$residuals))
sqrt(sum(reg$residuals^2)/nrow(x))  

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

library(ggplot2)


ggplot(data = x, aes(x = Delivery.Time, y = Sorting.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = x , aes(x=Delivery.Time, y=pred))

plot(log(Delivery.Time), Sorting.Time)
cor(log(Delivery.Time),Sorting.Time)

reg_log = lm(Sorting.Time ~ log(Delivery.Time))   

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(x))

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")


plot(Delivery.Time, Sorting.Time)
plot(Delivery.Time*Delivery.Time, Sorting.Time)

cor(Delivery.Time*Delivery.Time, Sorting.Time)

plot(Delivery.Time*Delivery.Time, log(Sorting.Time))

cor(Delivery.Time, log(Sorting.Time))
cor(Delivery.Time*Delivery.Time, log(Sorting.Time))


reg2degree = lm(log(Sorting.Time) ~ Delivery.Time + I(Delivery.Time*Delivery.Time))

summary(reg2degree)

logpol = predict(reg2degree)
expy = exp(logpol)

Error1 = x$Sorting.Time - expy

sqrt(sum(Error1^2)/nrow(x))  

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")


ggplot(data = x , aes(x = Delivery.Time + I(Delivery.Time^2), y = log(Sorting.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = x , aes(x= Delivery.Time +I(Delivery.Time^2), y=logpol))
