rm(list=ls())

visitors=rio::import("6304 Module 7 Assignment Data 1.xlsx")

visitors$time=seq(1:nrow(visitors))

attach(visitors)

plot(Year,China.visitors,pch=19,main="Year vs visitors")

names(visitors)
plot(Year,visitors$`China Visitors`,pch=19,main="Years vs visitors")
plot(visitors$Quarter,visitors$`China Visitors`,pch=19,main="Years vs visitors")
plot(time,visitors$`China Visitors`,pch=19,main="Years vs visitors")

visitors.out=lm(visitors$`China Visitors`~time,data = visitors)

summary(visitors.out)


points(visitors.out$fitted.values,type="l",lwd=3,col="red")

cor(visitors$`China Visitors`,visitors.out$fitted.values)
plot(time,rstandard(visitors.out),pch=19,type="o",
             main="Base Model, Standardized Residuals")
abline(0,0,col="red",lwd=3)

#Durbin-Watson test

durbin.out=car::durbinWatsonTest(visitors.out)
durbin.out

#Making Seasonal Indices

indices=data.frame(month=1:4,average=0,index=0)
 for(i in 1:4) {
    count=0
     for(j in 1:nrow(visitors)) {
       if(i==Quarter[j]) {
             indices$average[i]=indices$average[i]+visitors$`China Visitors`[j]
             count=count+1
           }
      }
     indices$average[i]=indices$average[i]/count
     indices$index[i]=indices$average[i]/mean(visitors$`China Visitors`)
   }
#Deseasonalizing the original data

for(i in 1:4){
     for(j in 1:nrow(visitors)){
         if(i==Quarter[j]){
            visitors$deseason.visitors$`China Visitors`[j]=visitors$`China Visitors`[j]/indices$index[i]
          }
      }
 } 

visitors1.out=lm(deseason.visitors$`China Visitors`~time,data = visitors)

visitors2.out=lm(deseason.visitors$`China Visitors`~time+I(time^2),data = visitors)

visitors3.out=lm(deseason.visitors$`China Visitors`~time+I(time^2)+I(time^3),data = visitors)


#For model 1

plot(time,visitors$`China Visitors`,type="o",pch=19,
           main="Original Data and Model1 data")
points(time,visitors.out$fitted.values,
                 type="o",pch=19,col="red")


For model 2

visitors$deseason.forecast=visitors1.out$fitted.values

for(i in 1:4){
    for(j in 1:nrow(visitors)){
        if(i==Quarter[j]){
            visitors$reseason.forecast[j]=visitors$deseason.forecast[j]*
                 indices$index[i]
          }
      }
   }
plot(time,visitors$`China Visitors`,type="o",pch=19,
           main="Original Data and Reseasonalized Forecasts")
 points(time,visitors$reseason.forecast,
                 type="o",pch=19,col="red")

 
# For model 3
 
 visitors$deseason.forecast=visitors2.out$fitted.values
  for(i in 1:4){
     for(j in 1:nrow(visitors)){
         if(i==Quarter[j]){
              visitors$reseason.forecast[j]=visitors$deseason.forecast[j]*
                  indices$index[i]
           }
       }
   }
  plot(time,visitors$`China Visitors`,type="o",pch=19,
              main="Original Data and Reseasonalized Forecasts")
  points(time,visitors$reseason.forecast,
                type="o",pch=19,col="red")

  
  
  For model 4
  
  visitors$deseason.forecast=visitors3.out$fitted.values
   for(i in 1:4){
       for(j in 1:nrow(visitors)){
           if(i==Quarter[j]){
             visitors$reseason.forecast[j]=visitors$deseason.forecast[j]*
                   indices$index[i]
             }
         }
     }
   plot(time,visitors$`China Visitors`,type="o",pch=19,
               main="Original Data and Reseasonalized Forecasts")
   points(time,visitors$reseason.forecast,
                   type="o",pch=19,col="red")

   
   summary(visitors.out)  
   summary(visitors1.out)
   summary(visitors2.out)
   summary(visitors3.out)
   
   
  # Model1 
   
   
   cor(visitors$`China Visitors`,visitors.out$fitted.values)
 
   
   
  # Model2
   
    visitors$deseason.forecast=visitors1.out$fitted.values
   
    cor(visitors$`China Visitors`,visitors$reseason.forecast)
   
   
   
  # Model3
   
    visitors$deseason.forecast=visitors2.out$fitted.values
   
   
   cor(visitors$`China Visitors`,visitors$reseason.forecast)
   
   
 #  Model4
   
    visitors$deseason.forecast=visitors3.out$fitted.values
   
    cor(visitors$`China Visitors`,visitors$reseason.forecast)












