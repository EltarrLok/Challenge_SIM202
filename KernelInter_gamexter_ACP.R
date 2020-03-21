rm(list = objects())
graphics.off()
#Cours/SIM202/
library(readr)
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory
data0 <- read.csv("train.csv")
View(data0)
test <- read.csv("test.csv")
View(test)

source("~/Cours/SIM202/R/rmse.R")
source("~/Cours/SIM202/R/erreur_validcroise_gam.R")


library(readr)
library(mgcv) # modélisation non linéaire
library(glmnet)
library(tidyverse)
library(lubridate)
library(ranger)
library(MASS)
library(zoo)
library(pbapply)
library(pls)
library(rgrs)
library(questionr)
library(dygraphs)
library(xts)
library(lubridate)
library(Matrix)
library(randomForest)

####################
######## G?n?ration de la variable DATEHEURE et DATEHEURETEST
####################

# date de fin interpolation : 2016-05-19 23:50:00 correspond ? la 4654 donn?e test


dateenheure = function(date)
{
  return (time_length(interval(start = ymd_hms("2016-01-11 17:00:00"), end = ymd_hms(date)), unit = "hour"))
}

DATE = data0$date

DATEHEURE = pblapply(DATE, dateenheure)
DATEHEURE = as.numeric(DATEHEURE)

AppTime = data.frame(DATEHEURE, data0$Appliances)



DATETEST = test[1:4654,]$date

DATEHEURETEST = pblapply(DATETEST, dateenheure)
DATEHEURETEST = as.numeric(DATEHEURETEST)



###############################################
########### Partie I : Interpolation par kernel
###############################################




#### noyau gaussien je test sur les donn?es

n = nrow(data0)

Appliances.xts <- xts(x = data0[0:n,]$Appliances,order.by = as.POSIXct(x = data0[0:n,]$date, origin = "2016-01-11 17:00:00"))

h = 0.01   # h en heures

noyaudata = function(i)
{
  deltatemps = DATEHEURE[0:n]-DATEHEURE[i]
  L = dnorm(deltatemps,0,sd=sqrt(h/2))/sum(dnorm(deltatemps,0,sd=sqrt(h/2)))
  L[L<1.e-5]=0
  L <- as(L, "sparseVector")%*% (data0$Appliances)
  return(L)
}

ychap.kernel = c(1:n)

ychap.kernel <- pblapply(c(1:n),noyaudata)

ychap.kernel<-xts(ychap.kernel,order.by=as.POSIXct(x = data0$date, origin = "2016-01-11 17:00:00"))


A = cbind(ychap.kernel,Appliances.xts)
names(A) = c("ychap.kernel", "Appliances.xts")
dygraph(A)

rmse(ychap.kernel,Appliances.xts)


###### J'interpole par rapport aux donn?es test


n = length(data0$date)

Appliances.xts <- xts(x = data0$Appliances,order.by = as.POSIXct(x = data0$date, origin = "2016-01-11 17:00:00"))

h = 0.05   # h en heures
x = c(1:n)

noyautest = function(i)
{
  deltatemps = DATEHEURE-DATEHEURETEST[i]
  L = dnorm(deltatemps,0,sd=sqrt(h/2))/sum(dnorm(deltatemps,0,sd=sqrt(h/2)))
  l = length(L)
  for(k in c(1:l))
  {
    if (L[k] < 0.00001)
    {
      L[k] = 0
    }
  }
  return(L)
}

App.inter = c(1:4654)

for (i in c(1:4654))
{
  print(i/4654*100)
  Coeff = as(noyautest(i), "sparseVector")
  App.inter[i] = Coeff %*% as.vector(data0$Appliances)
}




##########################################################
###################### Partie II : Extrapolation ? l'aide des autres variables
##########################################################

test.extra = test[4655:5771,]



############# Pr?paration des donn?es

data_Temp = subset(data0, select = c(T1, T2, T3, T4, T5, T6, T7, T8, T9, T_out, Tdewpoint))
data_RH = subset(data0, select = c(RH_1,RH_2,RH_3,RH_4,RH_5,RH_7,RH_8,RH_9,RH_out))


any(is.na(data_Temp))
any(is.na(data_RH))

cor(data0$Tdewpoint,data_Temp)

## D?composition sur composentes principales

pca_Temp = prcomp(data_Temp, scale = FALSE)
data_Temp = as.data.frame(pca_Temp$x)


pca_RH = prcomp(data_RH, scale = FALSE)
data_RH = as.data.frame(pca_RH$x)



######## Autres donn?es utiles

autredata_utile = subset(data0, select = c(lights, Press_mm_hg, Windspeed, Visibility, BE_load_actual_entsoe_transparency, BE_wind_onshore_generation_actual))
data_date = subset(data0, select = c(NSM, Posan))

######## Variables utiles

data1 = data.frame(data0$Appliances, data_date, data_Temp, data_RH, autredata_utile)


### conclusion avec RH_6 : NSM, PC1, PC3, PC6, PC7, PC8, PC11, PC1.1, PC2.1, PC4.1, PC5.1, lights, Windspeed, BE_load_actual_entsoe_transparency



#####################################
####### Model gam
#####################################

gfinal <- gam(data0.Appliances ~ s(NSM,k=5) + s(PC1,k = 5) + s(PC3,k = 5) + s(PC6, k= 5) + s(PC7, k=5) + s(PC8, k=5) + s(PC11, k=5) + s(PC1.1, k=5) + s(PC2.1, k=5) + s(PC5.1, k=5) + s(PC4.1, k=5) + s(lights, k=5) + s(Windspeed, k=5) + s(BE_load_actual_entsoe_transparency, k=5), data = data1, method = "GCV.Cp")



#############################################
####### M?me traitement pour les donn?es tests en extrapolation
#############################################


test_date = subset(test.extra, select = c(NSM, Posan))



#######Tests temp?ratures et en humidit?

test_Temp = subset(test.extra, select = c(T1, T2, T3, T4, T5, T6, T7, T8, T9, T_out, Tdewpoint))
test_RH = subset(test.extra, select = c(RH_1,RH_2,RH_3,RH_4,RH_5,RH_7,RH_8,RH_9,RH_out))


test_Temp = data.frame(na.approx(test_Temp))
any(is.na(test_Temp))

test_RH = data.frame(na.approx(test_RH))
any(is.na(test_RH))



test_Temp_pca = data.frame(predict(object = pca_Temp, newdata = test_Temp))

test_RH_pca = data.frame(predict(object = pca_RH, newdata = test_RH))
any(is.na(test_RH_pca))


####### Autres tests utiles

autretest_utile = subset(test.extra, select = c(lights, Press_mm_hg, Windspeed, Visibility, BE_load_actual_entsoe_transparency, BE_wind_onshore_generation_actual))

######## Variables utiles

test1 = data.frame(test_date, test_Temp_pca, test_RH_pca, autretest_utile)

############# App.extra

App.extra<-predict(gfinal, test1)
which(is.na(App.extra))


#########
## Autre partie II :
#########
m = mean(data0$Appliances)
App.extra = rep(m, 1117)

#############################################
####### Fusion des pr?visions et soumission
#############################################
App.inter = data.frame(App.inter)
App.extra = data.frame(App.extra)

names(App.inter) = c("Appliances")
names(App.extra) = c("Appliances")

kernel.inter_gam.acp.extra = rbind(App.inter,App.extra)
which(is.na(kernel.inter_gam.acp.extra))
kernel.inter_gam.acp.extra = as.numeric(unlist(kernel.inter_gam.acp.extra))

setwd("Cours/SIM202/")
submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- kernel.inter_gam.acp.extra

write.table(submit, file="submission_kernel.inter_gam.acp.extra2.csv", quote=F, sep=",", dec='.',row.names = F)










