rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory

source("rmse.R")
library(readr)
library(tidyverse)
library(lubridate)


#data <- read_delim("train.csv", col_names = TRUE, delim = ",")
#data_test<- read_delim("test.csv", col_names = TRUE, delim = ",")
data0<-read.csv(file="train.csv", sep=",", dec='.')
data_test<-read.csv(file="test.csv", sep=",", dec='.')


#On utilise na.approx qui est dans me package zoo pour remplacer les NA par interpolation
# On enleve d'abord les variables factor pour faire du calcul
factors <-which(sapply(data0, is.factor))
nb_fact = length(factors) # Nombre de variables FACTOR prise dans data_train
data_num <- subset( data0, select = -factors)
data_num<-data.frame(na.approx(data_num))

data_train<-data.frame(data0[factors[1]],data_num,data0[factors[-1]]) 
names(data_train)


######################################
######################################
###### Methode stepwise regression linéaire lm
#####################################""
#########################################
#data0<-data_num

fitMod <- function(eq, subset)
{
  reg <- lm(eq, data=data0[subset,])
  return(reg)
}

cov <- head(names(data_train)[-c(1,2)], 30) 
n <- nrow(data0)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.7))
length(s)

l="Appliances ~"
eq_list =list()

score = 1000
for(i in c(2:length(cov)))
{
  for(k in c(1:length(cov)))
  {
    eq_list[[k]] <-  paste0(l, paste0(cov[[k]], collapse='+'))
    eq_list[[k]]
  }
  reg_list <- lapply(eq_list, fitMod, subset=s)
  reg_list_forecast <- lapply(reg_list, predict, newdata=data0[s,])
  
  rmse(reg_list_forecast[[1]],data0[s,]$Appliances)
  rmse_list <- lapply(reg_list_forecast, rmse, data0[s,]$Appliances)
  j = which.min(rmse_list) ## On repère l'indice qui minimise l'erreur
  ### D'abord on vérifie qu'il y a un gain à rajouter la jème variable
  l
  if (rmse(reg_list_forecast[[j]],data0[s,]$Appliances) <score )
  {
    score = rmse(reg_list_forecast[[j]],data0[s,]$Appliances)
    l <- paste0(l, paste0(cov[j], collapse='+')) ## On rajoute le terme qui minimise à notre equation
    l <-paste0(l, '+')
  }
  cov[-j]
  
}
score

l<-strtrim(l,(nchar(l)-1))

Appliance.lm = lm(l,data0)
summary(Appliance.lm)
step.model.test<-predict(Appliance.lm, data_test)
which(is.na(step.model.test))


submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- step.model.test

write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)

