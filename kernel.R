rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory
library(zoo)
source("rmse.R")
library(snpar)
library(pbapply)
library(lubridate)
library(xts)
library(npreg)
library(kernlab)

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


# Cette ligne sert a ne recuperer que la partie interpolation sur le test
data_local_test<- subset(data1,as.POSIXct(data1$date)<as.POSIXct(tail(data_train$date,1)))

# On recupere un echantion indexé par s sur la partie interpolation.
data_local_train <- subset(data_train,as.POSIXct(data_train$date)<as.POSIXct(tail(data_local_test$date,1)))
n <- nrow(data_local_train)
proportion <- 1-nrow(data_local_test)/nrow(data_local_train)
set.seed(2)
s <- sample(c(1:n), size=floor(n*proportion))
s<-sort(s)
n-length(s) ==nrow(data_local_test)
Appliances_test <-data_local_train$Appliances[-s]

DATEHEURE_test <- (as.numeric(as.POSIXct(data_local_train[-s,]$date))-as.numeric(as.POSIXct(data_local_train[-s,]$date[1])))/3600
# maintenant on concatene une partie a trou avec autant de trou que represente test avec une partie continue
# cette denriere correspond a la queue de train qui ne se croise pas avec test
data_local_train <- rbind(data_local_train[s,],
                          subset(data_train,as.POSIXct(data0$date)>as.POSIXct(tail(data_local_train$date,1))))
DATEHEURE_train<-(as.numeric(as.POSIXct(data_local_train$date))-as.numeric(as.POSIXct(data_local_train$date[1])))/3600
DATE_test <- (as.numeric(as.POSIXct(data_non_futur0$date))-as.numeric(as.POSIXct(data_local_train$date[1])))/3600

#####################################################################################################################
####### Methode Noyaux ----- ROMIN
#####################################################################################################################
h = 1   # h en heures

noyaudata = function(i)
{
  deltatemps = DATEHEURE_train-DATE_test[i]
  L = dnorm(deltatemps,0,sd=sqrt(h/2))/sum(dnorm(deltatemps,0,sd=sqrt(h/2)))
  L[L<1.e-5]=0
  L <- as.vector(L) %*% as.vector(data_local_train$Appliances)
          
  return(L)
}

ychap.kernel = c(1:length(DATE_test))


ychap.kernel <- pblapply(c(1:length(DATE_test)),noyaudata)


data_local_test<- subset(data1,as.POSIXct(data1$date)>as.POSIXct(tail(data_train$date,1)))

RF <- randomForest(Appliances~., data =data_train[,-14],ntree = 270,replace=FALSE, importance = TRUE,
                   keep.forest=TRUE)

plot(RF)
step.model.test2 <- predict(RF,data_prediction_test)

length(ychap.kernel)+length(step.model.test2)
nrow(data1)

rmse(unlist(ychap.kernel),Appliances_test)
n-length(s)
length(unlist(ychap.kernel))
length(DATEHEURE_test)

#####################################################################################################################
####### Methode Noyaux ----- 
#####################################################################################################################

# On transforme la date en échelle

kernel <- npreg-internals(txdat = DATEHEURE_train, tydat =data_local_train$Appliances,exdat=Appliances_test)
kernel <-ksvm(x=DATEHEURE_train, data=NULL,y = data_local_train$Appliances, kpar = list(length = 4, lambda = 0.5),
              scaled = FALSE, kernel="stringdot")
