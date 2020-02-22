rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory

source("rmse.R")
library(readr)
library(mgcv) # modélisation non linéaire
library(tidyverse)
library(lubridate)
library(ranger)
library(zoo)
library(polycor)

#data <- read_delim("train.csv", col_names = TRUE, delim = ",")
#data_test<- read_delim("test.csv", col_names = TRUE, delim = ",")
data0<-read.csv(file="train.csv", sep=",", dec='.')
data_test<-read.csv(file="test.csv", sep=",", dec='.')


cor(data0$Appliances,as.numeric(data0$WeekStatus))
cor(data0$Appliances,as.numeric(data0$Day_of_week))
cor(data0$Appliances,as.numeric(data0$DayType))
cor(data0$Appliances,as.numeric(data0$InstantF))

# On se débarasse des variables qui sont des facteurs. On les voit avec la commande str()
str(data0)
data_num <- subset( data0, select = -c(date,WeekStatus,Day_of_week,DayType, InstantF))



# On cherche les coeffictions fortement correlés à Appliencies
# on peut observer qu'il y a beaucoup de NA
cor(data_num)[,1]
which(is.na(cor(data_num)[,1]))


# Pour visibility on voit que les valeurs sont assez continues et ne change pas énormément. On remplace les NA par 
#la moyenne de valeurs autour. Rayon =2
for (i in is.na(data0$Visibility)){
  data0$Visibility[i]<-(data0$Visibility[i+1]+data0$Visibility[i-1])/2
}
# On voit que ça a bien fonctionneé
which(is.na(data0$Visibility))


#On utilise na.approx qui est dans me package zoo pour remplacer les NA par interpolation
data_num<-data.frame(na.approx(data_num))


cor(data_num)[1,2:ncol(data_num)]
#il persiste une correaltion sous forme de NA. On la retire
data_num <- subset( data_num, select = -BE_wind_onshore_capacity)

####
##  Dans cette partie on retire les variables qui sont correlé à moins de 20% de la plus corrélée
####

max_corel<-max(abs(cor(data_num)[1,2:ncol(data_num)]))
liste_petit<-list()
for (i in c(ncol(data_num):2)){
  if (abs(cor(data_num)[1,i])< 0.7*max_corel)
  {
    data_num <- subset( data_num, select =-i)
  }
}

ncol(data_num)

data_train<-data.frame(data_num,data0$InstantF)
names(data_num)
names(data_train)
#names(data_train)[1]<-'date'
#names(data_train)[length(data_train)-3]<-'WeekStatus'
#names(data_train)[length(data_train)-2]<-'Day_of_week'
#names(data_train)[length(data_train)-1]<-'DayType'
names(data_train)[length(data_train)]<-'InstantF'
fact = 1 # Nombre de variables FACTOR prise dans data_train

which(is.na(data_train)) 
l <- 'Appliances ~Day_of_week'
g <- as.formula(l)

g0<-gam(g, data=data_train[s,])
summary(g0)
plot(g0,page=1)
par(new=TRUE)
plot(data0$T1,data0$Appliances)

######################################
######################################
###### Methode stepwise régréssion linéaire gam
#####################################""
#########################################
#data0<-data_num

fitMod_gam <- function(eq, subset)
{
  reg <- gam(eq, data=data0[subset,],method='REML')
  return(reg)
}

cov <- head(names(data_train)[-1], 30) 
n <- nrow(data0)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.7))
length(s)

l='Appliances ~'
eq_list =list()

score = 1000
for(i in c(2:length(cov)))
{
  
  for(k in c(1:(length(cov)-fact)))
  {
    eq_list[[k]] <-  paste0(l, 's(',paste0(cov[[k]],')'))
  }
  
  for(k in c((length(cov)+1-fact):length(cov)))
  {
    eq_list[[k]] <-  paste0(l,paste0(cov[[k]]))
  }
  
  g<-lapply(eq_list, as.formula)
  print(i)
  reg_list <- lapply(g, fitMod_gam, subset=s)
  #print(i)
  reg_list_forecast <- lapply(reg_list, predict, newdata=data0[s,])
  
  rmse(reg_list_forecast[[1]],data0[s,]$Appliances)
  rmse_list <- lapply(reg_list_forecast, rmse, data0[s,]$Appliances)
  j = which.min(rmse_list) ## On repère l'indice qui minimise l'erreur
  ### D'abord on vérifie qu'il y a un gain à rajouter la jème variable
  if (rmse(reg_list_forecast[[j]],data0[s,]$Appliances) <score )
  {
    score = rmse(reg_list_forecast[[j]],data0[s,]$Appliances)
    if (j<=(length(cov)-fact))
    {
    l <- paste0(l, 's(',paste0(cov[j],')')) ## On rajoute le terme qui minimise à notre equation
    l <-paste0(l, '+')
    }
    if (j>(length(cov)-fact))
    {
      l <- paste0(l,paste0(cov[j])) ## On rajoute le terme qui minimise à notre equation
      l <-paste0(l, '+')
    }
  }
  cov[-j]
  
}

gam(data0$Appliances~data0$T1)


l<-strtrim(l,(nchar(l)-1))
l<-"Appliances ~s(NSM)+s(T3)+s(T2)+s(T_out)+s(BE_load_forecast_entsoe_transparency)+s(RH_1)+s(RH_2)+s(RH_8)+s(T1)+s(RH_9)+s(BE_load_actual_entsoe_transparency)+s(RH_7)+s(Heure)+s(Windspeed)+s(T6)+s(RH_out)+s(Instant)"
g<-"Appliances ~s(NSM)+s(T3)+s(T2)+s(T_out)+s(BE_load_forecast_entsoe_transparency)+s(RH_1)+s(RH_2)+s(RH_8)+s(T1)+s(RH_9)+s(BE_load_actual_entsoe_transparency)+s(RH_7)+s(Heure)+s(Windspeed)+s(T6)+s(RH_out)+Instant+ WeekStatus+Day_of_week+DayType+InstantF"
g<-Appliances~InstantF+s(BE_load_forecast_entsoe_transparency)+s(BE_load_actual_entsoe_transparency)+s(lights)
g<-as.formula(g)
new_reg<-fitMod_gam(g,s)
names(data0)

best_vecteur <- predict(new_reg,data0[s,])

score = rmse(best_vecteur,data0[s,]$Appliances)

Appliance.lm = lm(l,data0)
summary(new_reg)
step.model.test<-predict(new_reg, data_test)
which(is.na(step.model.test))


submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- step.model.test

write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)








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



