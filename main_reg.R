
#na.omit
#statistique_et_logiciel




rm(list=objects())
library(tidyverse)
library(lubridate)
library(ranger)



setwd("/Users/Nefzaoui/Documents/SIM202/tp1")
train <- read.csv(file="train.csv", sep=",", dec='.')
test <- read.csv(file="test.csv", sep=",", dec='.')

Date_train = as.POSIXct(strptime(train$date, "%Y-%m-%d %H:%M:%S",tz="GMT"))
Date_test=as.POSIXct(strptime(test$date, "%Y-%m-%d %H:%M:%S"))

length(Date_train)
manquant_train=na.omit(Date_train)
length(manquant_train)
#which(is.na(Date_train), arr.ind=TRUE)
#Date_train[8176]


#as.POSIXlt(strptime("2016-03-27 02:00:00	", "%Y-%m-%d %H:%M:%S"))





#head(Date)
#plot(train$date,train$T1)
#plot(x=train$Date,y=train$T1)
#length((train$date))
library(xts)
n=length(Date_train)


T1<-xts(train$T1,order.by=Date_train)
Appliance=xts(train$Appliances,order.by=Date_train)


###estimation � noyau
h=1000
X=seq(1,n,length=n)
#test<-lapply(x,function(x){x^2})
#test[[1]]
#test
t<-c(1:n)

noyau<-function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))}
W<-matrix(unlist(lapply(X,noyau)),ncol=n,nrow=,byrow=F)

#plot(W[,50],type=
ychap.kernel<-colSums(as.numeric(X)*W)
ychap.kernel<-xts(ychap.kernel,order.by=Date_train)
#plot(X,type='l')
#lines(ychap.kernel,col='red')

     
acf(T1)


library(dygraphs)
library(xts)
dygraph(ychap.kernel)

dygraph(Appliance)

dygraph(T1)
plot.xts(Appliance)
zooom(n=1, eps=2)
zoomChart(Appliance)
is.ts(Appliance)
is.ts(T1)

decompose(Appliance)

acf(Appliance)
pacf(Appliance)

spectrum(Appliance)
hist(train$RH_3)
hist(train$RH_4)
train$RH_4[11]
t=train$RH_4
length((t))
t=t[sample(1:length(t), 5000, replace=TRUE)]

t=as.numeric(t)
hist(t)
#tw<-shapiro.test((t))

#####useful function
source("rmse.R")
names(train)








n <- nrow(train)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.1))
length(s)


#regression
reg0 <- lm(Appliances ~ T_out + InstantF + Day_of_week, data=train[-s,])
length(reg0$coefficients)
summary(reg0)
####erreur échantillon apprentissage
rmse(y=train$Appliances[-s], ychap=reg0$fitted.values)

####erreur échantillon test
reg0.forecast <- predict(reg0, newdata=train[s,])
rmse(y=train$Appliances[s], ychap=reg0.forecast[s])





plot(reg0)

a <- 1
b <- 144
plot(train$Appliances[a:b], type='l')
lines(reg0$fitted.values[a:b], col='red')

###stepwise selection

cov <- head(names(train)[-c(1,2)], 32) ####un peu long (instantF)
cov <- head(names(train)[-c(1,2)], 30) 
eq <- paste0("Appliances ~", paste0(cov, collapse='+'))
eq
full.model <- lm(eq, data = train[-s,])
summary(full.model)

#######################################################################################
#############################Stepwise regression model
#######################################################################################

library(MASS)
step.model <- stepAIC(full.model, direction = "both", trace = TRUE, 
                      data=train[-s,])

summary(step.model)
names(step.model)
step.model$anova

step.model.forecast <- predict(step.model, newdata=train[s,])
rmse(y=train$Appliances[s], ychap=step.model.forecast)

a <- 1
b <- 144
plot(train$Appliances[a:b], type='l')
lines(step.model$fitted.values[a:b], col='red')

#######################################################################################
#############################forward selection, test set
#######################################################################################

cov <- head(names(train)[-c(1,2)], 30)
eq_list <- list()
eq_list[[1]] <-  paste0("Appliances ~", paste0(cov[1], collapse='+'))
for(i in c(2:length(cov)))
{
  eq_list[[i]] <-  paste0("Appliances ~", paste0(cov[1:i], collapse='+'))
}

fitMod <- function(eq, subset)
{
  reg <- lm(eq, data=train[-subset,])
  return(reg)
}


reg_list <- lapply(eq_list, fitMod, subset=s)
length(reg_list)
reg_list_forecast <- lapply(reg_list, predict, newdata=train[s,])
rmse_list <- lapply(reg_list_forecast, rmse, y=train[s,]$Appliances)

plot(unlist(rmse_list))
which.min(rmse_list)

reg_list[[23]]


#########leaps
# install.packages("leaps")
# library(leaps)
# cov <- head(names(train)[-c(1,2)], 32) ####un peu long (instantF)
# eq <- paste0("Appliances ~", paste0(cov, collapse='+'))
# eq
# full.model <- lm(eq, data = train[-s,])
# 
# models <- regsubsets(eq, data = train[-s,], nvmax = 5, method = "seqrep")
# 

###########blowise CV



###########Exemple de soumission 



step.model.test <- predict(step.model, newdata=test)
which(is.na(step.model.test))

submit <- read.csv(file="Data/sample_submission.csv", sep=",", dec=".")
submit$Appliances <- step.model.test

write.table(submit, file="Data/submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)







