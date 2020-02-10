rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory


source("rmse.R")
library(readr)
library(tidyverse)
library(lubridate)
library(ranger)

#data <- read_delim("train.csv", col_names = TRUE, delim = ",")
#data_test<- read_delim("test.csv", col_names = TRUE, delim = ",")
data0<-read.csv(file="train.csv", sep=",", dec='.')
data_test<-read.csv(file="test.csv", sep=",", dec='.')

# Pas besoin de mettre une date car il y en a deja une
# Tracer variables en fonction d'une autre, voir correlation etc


################################
##############################
############" Methode variable bien correllées
###################################
####################################

Applience = data$Appliances#[1:7000]
summary(data)
train<-as.data.frame(data[,1], drop=false)
train<-data$T5[1:7000]
lm(Applience~as.data.frame(data[,1], drop=false))



#Appliance.lm = lm(data$Appliances[1:7000]~data$lights[1:7000] + data$T2[1:7000] + data$T6[1:7000] + data$RH_8[1:7000] + data$T_out[1:7000] + data$RH_out[1:7000] + data$NSM[1:7000] + data$Windspeed[1:7000])
Appliance.lm = lm(data$Appliances~data$lights + data$T2 + data$T6+ data$RH_8 + data$T_out + data$RH_out + data$NSM + data$Windspeed)
Appliance.lm = lm(Appliances~lights + T2 + T6+ RH_8 + T_out + RH_out + NSM + Windspeed)


summary(Appliance.lm)
step.model.test<-predict(Appliance.lm, data_test)
which(is.na(step.model.test))


submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- step.model.test

dim(submit)
head(submit)
submit$Appliances <- step.model.test
str(submit)

write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)

######################################
######################################
###### Methode stepwise
#####################################""
#########################################

cov <- head(names(data0)[-c(1,2)], 30) 
n <- nrow(data0)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.5))
length(s)

l="Appliances ~"
eq_list =list()
eq_list[[1]] <-  paste0(l, paste0(cov[1], collapse='+'))

####### On génère une liste de formule
for(i in c(1:length(cov)))
{
  eq_list[[i]] <-  paste0(l, paste0(cov[[i]], collapse='+'))
  eq_list[[i]]
}

####### On génère une liste de regression linéaire
fitMod <- function(eq, subset)
{
  reg <- lm(eq, data=data0[-subset,])
  return(reg)
}
reg_list <- lapply(eq_list, fitMod, subset=s)

####### On prédit à partir de des regressions linéaires 
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
l


####### Algorrithme Stepwise propre

cov <- head(names(data0)[-c(1,2)], 30) 
n <- nrow(data0)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.5))
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

l<-strtrim(l,(nchar(l)-1))

Appliance.lm = lm(l,data0)
summary(Appliance.lm)
step.model.test<-predict(Appliance.lm, data_test)
which(is.na(step.model.test))


submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- step.model.test

write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)


#str(data)
#head(data)
summary(data)
attach(data)
cons =data$Appliances
plot(data$date, data$Appliances, type = 'l')

######################################
########## Histogram  ###########
######################################

#histogram avec 40 marches
hist(cons,breaks =40,main="Frequence des consomations",xlab="Quantités Consommées",
     ylab="Frequence",col="green",proba=T)
h<-hist(cons,breaks =seq(min(cons),max(cons),by=10),plot =FALSE)
h$counts
#s = max(h$counts) ### Si on prend le max atteint en terme de repetition
#s=m(cons)   ### Si on prend l'occurence maximale.
m=min(cons)+which.max(h$counts)*10
# hist du min au max avec pas de 10
hist(cons,breaks =seq(min(cons),max(cons),by=10),main="Frequence des consomations",
     xlab="Quantités Consommées", ylab="Frequence",col="green",proba=T)
#histo du min au max avec pas de 10 tronqué à 200
hist(cons,breaks =seq(min(cons),max(cons),by=10),main="Frequence des consomations",
     xlab="Quantités Consommées", ylab="Frequence",col="green",proba=F, 
     xlim=c(0,200),ylim=c(0,3000))


curve(dnorm(x,mean=m,sd =s**(1/2)), add =T, col = "red")
curve(dnorm(x,mean=mean(cons),sd =sd(cons)), add =T, col = "red")


######################################
########## Corelation  ###########
######################################
library(corrplot)
round(cor(df),4) # MAtrice de correlation ie de(s) covariance(s) 
corrplot(cor(df),method = "number",type="upper")

#pairs(~ x1 + x3, data = data, labels = c("var1", "var3"),  
#         main = "This is a nice pairs plot in R")) #faire un scatterplot en enlevant la variable x2


#plot(data$date<"2016-02-01", data$T1-data$T2, type = 'h')


#plot(data$date, data$T1-data$T2, type = 'h')

#selectByDate(
#  data,
#  start = "1/1/2016",
#  end = "01/02/2016",
#  year = 2016,
#  month = 1,
#  day = "weekday",
#  hour = 1
#)


# CONSTRUIR UN MODELE DE REGRESSION LINEAIRE
n<-length(data$Appliances)/2

set.seed(100)
#rnorm(13)
mysample <- data[sample(1:nrow(data), n, replace=T),] 
print(mysample)

s<-sample(data)

s
cov<-head(names(data)[-c(1,2)],30)



