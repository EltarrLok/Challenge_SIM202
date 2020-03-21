rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory
library(zoo)
source("rmse.R")
library(FNN)
library(gam)
library(xgboost)

data0<-read.csv(file="train.csv", sep=",", dec='.')
data1<-read.csv(file="test.csv", sep=",", dec='.')
#str(data0)

data0 <- subset( data0, select = -c(Instant,InstantF,Month,DayType,WeekStatus,Heure,RH_6,
                                    BE_wind_onshore_capacity,BE_wind_onshore_profile,rv1,rv2))
factors <-which(sapply(data0, is.factor))
nb_fact = length(factors) # Nombre de variables FACTOR prise dans data_train
data_num <- subset( data0, select = -factors)
data_num<-data.frame(na.approx(data_num))

data_train<-data.frame(data_num,data0[factors]) 


data1 <- subset( data1, select = -c(Instant,InstantF,Month,DayType,WeekStatus,Heure,RH_6,
                                BE_wind_onshore_capacity,BE_wind_onshore_profile,rv1,rv2))
factors1 <-which(sapply(data1, is.factor))
nb_fact1 = length(factors1) # Nombre de variables FACTOR prise dans data_train
data_num1 <- subset( data1, select = -factors)
data_num1<-data.frame(na.approx(data_num1))
#which(is.na(data_num1$Visibility))
data1<-data.frame(data_num1,data1[factors]) 



data_local_train <- subset(data_train,as.Date(data0$date)<as.Date("2016-05-19 23:50:00"))
data_local_test<- subset(data_train,as.Date(data1$date)<as.Date("2016-05-19 23:50:00"))






n <- nrow(data_local_train)
set.seed(28)
s <- sample(c(1:n), size=floor(n*0.6))
length(s)

xgboost(data = data_local_train[s], label = train$label, max.depth = 2, eta = 1, 
        nthread = 2, nrounds = 2, objective = "binary:logistic")



loo <- loess(Appliances~NSM+Posan,model =TRUE,data=subset(data_local_train[s,],
                              select=-c(date,Day_of_week)),span=0.005)
summary(loo)
lfit=predict(loo, newdata=data_local_train[-s,])
plot(lfit,type='l',col='darkgoldenrod3')
par(new=TRUE)
plot(data_local_train$Appliances[-s],type='l',col='aquamarine')
rmse(lfit,data_local_train$Appliances[-s])

ga <- lm(Appliances~lights + RH_1 + T2 + RH_2 + T3 + RH_3 + T4 + RH_5 + 
           T6 + T7 + RH_7 + T8 + RH_8 + T9 + T_out + RH_out + Windspeed + 
           Tdewpoint + NSM + Posan + BE_load_actual_entsoe_transparency + 
           BE_wind_onshore_generation_actual,data=subset(data_local_train[s,],select=-c(date,Day_of_week)))
gfit=predict(ga, newdata=data_local_train[-s,])
plot(gfit,type='l',col='darkgoldenrod3')
par(new=TRUE)
plot(data_local_train$Appliances[-s],type='l',col='aquamarine')
rmse(gfit,data_local_train$Appliances[-s])

### k=3
nn <- knn.reg(train=subset(data_local_train[s,],select=c(NSM,Posan)),
              test=subset(data_local_train[-s,],select=c(NSM,Posan)), y=data_local_train$Appliances[s])
nrow(nn)
plot(nn$pred,type='l',col='darkgoldenrod3')
par(new=TRUE)
plot(data_local_train$Appliances[-s],type='l',col='aquamarine')
rmse(nn$pred,data_local_train$Appliances[-s])

# k= 25 rmse = 94.45
# k= 120 rmse = 93.17

nn <- knn.reg(train=subset(data_local_train[s,],select=-c(date,Day_of_week)), k=250,
              test=subset(data_local_train[-s,],select=-c(date,Day_of_week)), 
              y=data_local_train$Appliances[s])
plot(nn$pred,type='l',col='darkgoldenrod3')
par(new=TRUE)
plot(data_local_train$Appliances[-s],type='l',col='aquamarine')



data_futur1 <- subset(data1,as.Date(data1$date)>as.Date("2016-05-18 23:50:00"))
data_non_futur1 <- subset(data1,as.Date(data1$date)<as.Date("2016-05-19 00:00:00"))
