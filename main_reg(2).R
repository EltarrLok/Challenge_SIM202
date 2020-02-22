rm(list=objects())
library(tidyverse)
library(lubridate)
library(ranger)

Data0 <- read.csv(file="train.csv", sep=",", dec='.')
Data1 <- read.csv(file="test.csv", sep=",", dec='.')

# Data <- readRDS("Data/Data.rds")
# Data_target <- readRDS("Data/test.RDS")

Data <-rbind(Data0[,-2], Data1[,-ncol(Data1)])
dim(Data)

Data_WTHNA <- Data[-which(is.na(Data$RH_6)), ]
RF_NA <- ranger(RH_6 ~ RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_7 + RH_8, data=Data_WTHNA)
Data$RH_6[which(is.na(Data$RH_6))] <- predict(RF_NA,data=Data[which(is.na(Data$RH_6)),])$predictions

Data_WTHNA <- Data[-which(is.na(Data$Visibility)), ]
RF_NA <- ranger(Visibility ~ Tdewpoint + Windspeed + T_out + Instant, data=Data_WTHNA)
Data$Visibility[which(is.na(Data$Visibility))] <- predict(RF_NA, data=Data[which(is.na(Data$Visibility)),])$predictions

Data0[,-2] <- Data[1:nrow(Data0),]
Data1[,-ncol(Data1)] <- Data[(nrow(Data0)+1):nrow(Data),]



#####useful function
source("R/rmse.R")
names(Data0)

n <- nrow(Data0)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.1))
length(s)

reg0 <- lm(Appliances ~ T_out + InstantF + Day_of_week, data=Data0[-s,])
length(reg0$coefficients)
summary(reg0)
####erreur échantillon apprentissage
rmse(y=Data0$Appliances[-s], ychap=reg0$fitted.values)

####erreur échantillon test
reg0.forecast <- predict(reg0, newdata=Data0[s,])
rmse(y=Data0$Appliances[s], ychap=reg0.forecast[s])


plot(reg0)

a <- 1
b <- 144
plot(Data0$Appliances[a:b], type='l')
lines(reg0$fitted.values[a:b], col='red')

###stepwise selection

cov <- head(names(Data0)[-c(1,2)], 32) ####un peu long (instantF)
cov <- head(names(Data0)[-c(1,2)], 30) 
eq <- paste0("Appliances ~", paste0(cov, collapse='+'))
eq
full.model <- lm(eq, data = Data0[-s,])
summary(full.model)

#######################################################################################
#############################Stepwise regression model
#######################################################################################

library(MASS)
step.model <- stepAIC(full.model, direction = "both", trace = TRUE, 
                      data=Data0[-s,])

summary(step.model)
names(step.model)
step.model$anova

step.model.forecast <- predict(step.model, newdata=Data0[s,])
rmse(y=Data0$Appliances[s], ychap=step.model.forecast)

a <- 1
b <- 144
plot(Data0$Appliances[a:b], type='l')
lines(step.model$fitted.values[a:b], col='red')

#######################################################################################
#############################forward selection, test set
#######################################################################################

cov <- head(names(Data0)[-c(1,2)], 30)
eq_list <- list()
eq_list[[1]] <-  paste0("Appliances ~", paste0(cov[1], collapse='+'))
for(i in c(2:length(cov)))
{
  eq_list[[i]] <-  paste0("Appliances ~", paste0(cov[1:i], collapse='+'))
}

fitMod <- function(eq, subset)
{
  reg <- lm(eq, data=Data0[-subset,])
  return(reg)
}


reg_list <- lapply(eq_list, fitMod, subset=s)
length(reg_list)
reg_list_forecast <- lapply(reg_list, predict, newdata=Data0[s,])
rmse_list <- lapply(reg_list_forecast, rmse, y=Data0[s,]$Appliances)

plot(unlist(rmse_list))
which.min(rmse_list)

reg_list[[23]]


#########leaps
# install.packages("leaps")
# library(leaps)
# cov <- head(names(Data0)[-c(1,2)], 32) ####un peu long (instantF)
# eq <- paste0("Appliances ~", paste0(cov, collapse='+'))
# eq
# full.model <- lm(eq, data = Data0[-s,])
# 
# models <- regsubsets(eq, data = Data0[-s,], nvmax = 5, method = "seqrep")
# 

###########blowise CV



###########Exemple de soumission 



step.model.test <- predict(step.model, newdata=Data1)
which(is.na(step.model.test))

submit <- read.csv(file="Data/sample_submission.csv", sep=",", dec=".")
submit$Appliances <- step.model.test

write.table(submit, file="Data/submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)








