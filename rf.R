rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory

library(zoo)
library(randomForest)
#library(ggplot2)
#library(mgcv)
#library(cowplot)
library(gam)
library(forecast)
library(corrplot)
library(car)
library(mctest)
library(ppcor)

source("rmse.R")
##################################################################################
############################  Recupération des données ###########################
##################################################################################
data0<-read.csv(file="train.csv", sep=",", dec='.')
data1<-read.csv(file="test.csv", sep=",", dec='.')
#str(data0)
########################################################
##### On enlève les variables qu'on a jugé pas utiles
##### On repère les FCATOR et approximant les NA
########################################################
data0 <- subset( data0, select = -c(Instant,InstantF,Month,DayType,WeekStatus,Heure,RH_6,
                                    BE_wind_onshore_capacity,BE_wind_onshore_profile,rv1,rv2))
factors <-which(sapply(data0, is.factor))
nb_fact = length(factors) # Nombre de variables FACTOR prise dans data_train
data_num <- subset( data0, select = -factors)
data_num<-data.frame(na.approx(data_num))

data_train<-data.frame(data_num,data0[factors]) 


#####################################################################################################################
##### On fait des test et des observations pour voir si il y a des variables correlees et/ou colineaires (spoil =oui)
#####################################################################################################################

omcdiag(data0[,c(seq(4,14,2),seq(15,19,2),26)],data0$Appliances)
imcdiag(data0[,c(seq(4,14,2),seq(15,19,2))],data0$Appliances)
imcdiag(data0[,c(seq(5,13,2),seq(16,20,2))],data0$Appliances)
round(pcor(data0[,c(seq(4,14,2),seq(15,19,2),26)],method = 'pearson')$estimate,3)
names(data0)

#####################################################################################################################
####### Methode PCA
#####################################################################################################################
pr.com <- prcomp(data_num[,c(seq(3,13,2),seq(14,18,2))], scale. = F)
names(pr.com)
head(round(pr.com$x,2))
biplot(pr.com,scale=0)
std_dev <- pr.com$sdev
pr_var <- std_dev^2 
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained",type = "b")
## 3 variables à retenir


pr.com$rotation$PC1



train.data <- data.frame(data_num$Appliances, pr.com$x)
train.data <- train.data[,1:7]
names(train.data)[1] <- "Appliances"
RF <- randomForest(Appliances~., data =train.data,ntree = 500,replace=FALSE, importance = TRUE,keep.forest=TRUE)

test.data <- predict(pr.com, newdata = data1)
test.data <- as.data.frame(test.data)

rpart.prediction <- predict(RF, test.data) 


#####################################################################################################################
########### Periodicite des variables de temperatures
#####################################################################################################################
plot(data0$T,type='l')
summary(data0$T3)
cor(data0$T2,data0$T7)
cor(data0$T5,data0$T9)
acf(data0$Appliances,lag.max=400)
acf(data0$T3,lag.max=400)

#####################################################################################################################
########## On sépare la date en futur et en non futur
#####################################################################################################################


data_futur0 <- subset(data_train,as.Date(data1$date)>as.Date("2016-05-10 23:50:00"))
data_non_futur0 <- subset(data_train,as.Date(data1$date)<as.Date("2016-05-11 00:00:00"))

data_futur0 <- subset(data_train,select=-date)
data_non_futur0 <- subset(data_train,select=-date)

data_futur1 <- subset(data1,as.Date(data1$date)>as.Date("2016-05-18 23:50:00"))
data_non_futur0 <- subset(data1,as.Date(data1$date)<as.Date("2016-05-19 00:00:00"))
nrow(data_non_futur0)
tail(data0$date,1)
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

m <- nrow(data_non_futur0)
set.seed(1)
s <- sample(c(1:m), size=floor(m*0.8))
length(s)
data_train <- subset(data_train,select=-date)
RF <- randomForest(Appliances~., data =data_train[s,],ntree = 271,replace=FALSE,xtest=subset(data_train[-s,], select=-Appliances),
                   ytest=data_train[-s,]$Appliances,importance = TRUE,keep.forest=TRUE)

plot(RF)
step.model.test1 <- predict(RF,data_non_futur1)


MRF <- randomForest(Appliances~., data =data_non_futur0[,-14],ntree = 301,replace=FALSE,xtest=subset(data_futur0, select=-Appliances),
                   ytest=data_futur0$Appliances,importance = TRUE,keep.forest=TRUE)

tail(data_futur0$date)
step.model.test2 <- predict(RF,data_futur1)



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################


step(lm(Appliances~1,data_non_futur0), scope=~lights+T1+RH_1+T2+RH_2+T3+RH_3+T4+RH_4+T5+RH_5+T6+T7+RH_7+T8+RH_8+T9+RH_9+T_out
     +Press_mm_hg+RH_out+Windspeed+Visibility+Tdewpoint+NSM+Posan+BE_load_actual_entsoe_transparency
     +BE_load_forecast_entsoe_transparency+BE_wind_onshore_generation_actual+Day_of_week,direction = "both")

Appliance.lm <- lm(formula = Appliances ~ lights + BE_load_forecast_entsoe_transparency + T3 + Day_of_week + RH_out + T9 + T8 +
                     BE_load_actual_entsoe_transparency + RH_1 + RH_2 + T2 + RH_8 + RH_3 + T6 + RH_9 + Windspeed + 
                     BE_wind_onshore_generation_actual + Press_mm_hg + RH_7 + T7 + RH_5 + Visibility, data = data_non_futur0)



step.model.test2<-predict.lm(Appliance.lm, data_non_futur1)
step.model.test2<-na.approx(step.model.test2)
step.model.test1<-na.approx(step.model.test1)

length(ychap.kernel)+length(step.model.test2)
length(step.model.test)
nrow(data1)
step.model.test <- c(ychap.kernel,step.model.test2)
submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- step.model.test
submit$Appliances <- na.approx(submit$Appliances)

write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)
