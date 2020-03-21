rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory
library(zoo)
source("rmse.R")
library(caret)
library(corrplot)
library(mctest) # pour faire les test de colinearite

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
## Les NA non interpolable de data1 sont sur visbility uniquement. On le remplace par la moyenne.
data_num1$Visibility[which(is.na(data_num1[,23]))] <- mean(data_num1$Visibility[-which(is.na(data_num1[,23]))])
data1<-data.frame(data_num1,data1[factors]) 

data <- rbind(data_train[-1],subset(data1,select=-Id))

### On fait le test de multi colinéarité sur le train seulement
### Ce n'est pas possible de le faire ailleurs

# names(data)
# omcdiag(data0[,c(seq(4,14,2),seq(15,19,2),26)],data0$Appliances)
# imcdiag(data0[,c(seq(4,14,2),seq(15,21,2))],data0$Appliances)
# omcdiag(data0[,c(seq(5,11,2),seq(16,20,2))],data0$Appliances)
# imcdiag(data0[,c(seq(5,11,2),seq(16,20,2))],data0$Appliances)
# round(pcor(data0[,c(seq(4,14,2),seq(15,19,2),26)],method = 'pearson')$estimate,3)
# 


names(data[,c(seq(2,12,2),seq(13,19,2))])
T.pr.com <- prcomp(data[,c(seq(2,12,2),seq(13,19,2))], scale. = F)
names(T.pr.com)
head(round(T.pr.com$x,2))
biplot(T.pr.com,scale=0)
T.std_dev <- T.pr.com$sdev
T.pr_var <- T.std_dev^2 
T.prop_varex <- T.pr_var/sum(T.pr_var)
plot(cumsum(T.prop_varex), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained",type = "b")
## On prend 2 PC 

T_imaginaire0 <-as.matrix(data_train[,c(seq(3,13,2),seq(14,20,2))]) %*% as.matrix(T.pr.com$rotation[,1:2])
data_train <- subset(data_train,select=-c(seq(3,13,2),seq(14,20,2)))
data_train <- data.frame(data_train,T_imaginaire0)

names(data_train)[length(data_train)-1] <- "T_imaginaire1"
names(data_train)[length(data_train)] <- "T_imaginaire2"
names(data_train)


T_imaginaire1 <-as.matrix(data1[,c(seq(2,12,2),seq(13,19,2))]) %*% as.matrix(T.pr.com$rotation[,1:2])
data1 <- subset(data1,select=-c(seq(2,12,2),seq(13,19,2)))
data1 <- data.frame(data1,T_imaginaire1)

names(data1)[length(data1)-1] <- "T_imaginaire1"
names(data1)[length(data1)] <- "T_imaginaire2"
names(data1)



names(data[,c(seq(3,9,2),seq(14,19,2))])
RH.pr.com <- prcomp(data[,c(seq(3,9,2),seq(14,19,2))], scale. = F)
names(RH.pr.com)
head(round(RH.pr.com$x,2))
biplot(RH.pr.com,scale=0)
RH.std_dev <- RH.pr.com$sdev
RH.pr_var <- RH.std_dev^2 
RH.prop_varex <- RH.pr_var/sum(T.pr_var)
plot(cumsum(RH.prop_varex), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained",type = "b")
## On prend 1 PC encore cette fois 

RH_imaginaire0 <-as.matrix(data_train[,c(c(3:6),c(8:10))]) %*% as.matrix(RH.pr.com$rotation[,1])
data_train <- subset(data_train,select=-c(c(3:6),c(8:10)))
data_train <- data.frame(data_train,RH_imaginaire0)

names(data_train)[length(data_train)] <- "RH_imaginaire"
names(data_train)



RH_imaginaire1 <-as.matrix(data1[,c(c(2:5),c(7:9))]) %*% as.matrix(RH.pr.com$rotation[,1])
data1 <- subset(data1,select=-c(c(2:5),c(7:9)))
data1 <- data.frame(data1,RH_imaginaire1)

names(data1)[length(data1)] <- "RH_imaginaire"
names(data1)










omcdiag(data0[,c(seq(4,14,2),seq(15,19,2),26)],data0$Appliances)
imcdiag(data0[,c(seq(4,14,2),seq(15,21,2),26)],data0$Appliances)
omcdiag(data0[,c(seq(5,11,2),seq(16,20,2))],data0$Appliances)
imcdiag(data0[,c(seq(5,11,2),seq(16,20,2))],data0$Appliances)
round(pcor(data0[,c(seq(4,14,2),seq(15,19,2),26)],method = 'pearson')$estimate,3)




M <- cor(data0[,c(seq(4,14,2),seq(15,21,2),26)])
corrplot(M)

omcdiag(data_train[,c(8,16,17)],data_train$Appliances)
imcdiag(data_train[,c(8,16,17)],data_train$Appliances)
omcdiag(data0[,c(seq(5,11,2),seq(16,20,2))],data0$Appliances)
imcdiag(data0[,c(seq(5,11,2),seq(16,20,2))],data0$Appliances)
round(pcor(data0[,c(seq(4,14,2),seq(15,19,2),26)],method = 'pearson')$estimate,3)









#################################################################
###################   XGBOOST     ###############################
#################################################################


l<- length(subset(data1,as.POSIXct(data1$date)>as.POSIXct(tail(data_train$date,1))))
data_prediction_test<- tail(data_train,l)
data_prediction_train <- head(data_train,length(data_train$date)-l)
Appliances_train <- data_prediction_train$Appliances
Appliances_test <- data_prediction_test$Appliances
data_prediction_train <- subset(data_prediction_train,select=-Appliances)
data_prediction_train <- subset(data_prediction_train,select=-Appliances)

data_prediction_test<- as.matrix(data_prediction_test, rownames.force=NA)
data_prediction_train<- as.matrix(data_prediction_train, rownames.force=NA)
 data_prediction_test <- as(data_prediction_test, "sparseMatrix")
 data_prediction_train <- as(data_prediction_train, "sparseMatrix")

X_train = xgb.DMatrix(data_prediction_train)
X_test =xgb.DMatrix(data_prediction_test)


xgb_trcontrol = trainControl(method = "cv", number = 5, allowParallel = TRUE, 
                             verboseIter = FALSE, returnData = FALSE)

xgbGrid <- expand.grid(nrounds = c(100,200),  max_depth = c(3, 5, 10, 15, 20),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),## valeurs par défaut : 
                       eta = 0.1,
                       gamma=0,min_child_weight = 1,subsample = 1)

set.seed(0)
xgb_model = train(X_train, Appliances_train, trControl = xgb_trcontrol, tuneGrid = xgbGrid, 
                  method = "xgbTree")
rmse()