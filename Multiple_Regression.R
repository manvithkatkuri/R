#Importing libraries and data
rm(list=ls())
library(rio)
library(moments)
car_data=import("6304 Module 5 Assignment Data.xlsx")

#Preprocessing
colnames(car_data)=tolower(make.names(colnames(car_data)))
car_dataset = subset(car_data, car_data$model.year <=76 & car_data$cylinders)
set.seed(99718118)
car_sample=car_data[sample(nrow(car_data),100),]
car_sample$model.year=as.factor(car_sample$model.year)
car_sample$cylinders=as.factor(car_sample$cylinders)

#Analysis
str(car_sample)
mean(car_sample$mpg)
mean(car_sample$cubic.inches)

#Model Fitting
model=lm(formula = car_sample$mpg ~ car_sample$cubic.inches + car_sample$horsepower + 
           car_sample$weight, data = car_sample)
summary(model)
#Confidence intervel
confint(model)

#LINE assumptions of regression
par(mfrow=c(2,2))
 # Linearity
  
   plot(car_sample$mpg,model$fitted.values,
              pch=19,main="Actuals v. Fitteds, Car data")

 abline(0,1,col="red",lwd=3)

# Normality

 qqnorm(model$residuals,pch=19,
                  main="Normality Plot, Car Data")

 qqline(model$residuals,lwd=3,col="red")

 hist(model$residuals,col="red",
             main="Residuals, Cr data",
             probability=TRUE)

 curve(dnorm(x,mean(model$residuals),
                           sd(model$residuals)),
             from=min(model$residuals),
             to=max(model$residuals),
             lwd=3,col="blue",add=TRUE)

 # Equality of Variances
  
   plot(model$fitted.values,
            scale(model$residuals),
            pch=19,main="Equality of Variances, Car data")

 abline(0,0,lwd=3,col="red")



#7.Using  multiple regression model from Part 3 above, introducing the Cylinders variable into the model.
 car_sample$cylinders=relevel(car_sample,ref = "8")
 
 model4=lm(car_sample$mpg~car_sample$cubic.inches + car_sample$horsepower + car_sample$weight + car_sample$cylinders, data=car_sample)
 
  summary(model4)




