x = read.csv(file.choose())
library(readr)
View(x)

summary(x)

plot(x$Waist, x$AT)
attach(x)
cor(Waist, AT)
reg = lm(AT ~ Waist) 

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

ggplot(data = x , aes(x = Waist, y = AT)) + 
  geom_point(color='red') +
  geom_line(color='blue',data = x , aes(x=Waist, y=pred))
