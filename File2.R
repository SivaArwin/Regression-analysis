x = read.csv(file.choose())
library(readr)
View(x)

summary(x)

plot(x$Weight.gained..grams., x$Calories.Consumed)
attach(x)

cor( Weight.gained..grams., Calories.Consumed)

reg = lm(Calories.Consumed ~ Weight.gained..grams.) 

summary(reg)

pred = predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(x))

sqrt(mean(reg$residuals^2))

confint(reg,level=0.99)
predict(reg,interval="predict")

library(ggplot2)

ggplot(data = x , aes(x = Weight.gained..grams., y = Calories.Consumed)) + 
  geom_point(color='red') +
  geom_line(color='blue',data = x , aes(x=Weight.gained..grams., y=pred))


plot(log(Weight.gained..grams.), Calories.Consumed)
cor(log(Weight.gained..grams.), Calories.Consumed)

reg_log = lm(Calories.Consumed ~ log(Weight.gained..grams.))   

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(x))  

confint(reg_log,level=0.99)
predict(reg_log,interval="confidence")


reg2degree = lm(log(Calories.Consumed) ~ Weight.gained..grams. + I(Weight.gained..grams.*Weight.gained..grams.))

summary(reg2degree)

logpol = predict(reg2degree)


confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")


ggplot(data = x , aes(x = Weight.gained..grams. + I(Weight.gained..grams.^2), y = log(Calories.Consumed))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = x, aes(x=Weight.gained..grams.+I(Weight.gained..grams.^2), y=logpol))

