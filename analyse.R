rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory

source("rmse.R")
library(readr)
#library(mgcv) # modélisation non linéaire
library(tidyverse)
library(lubridate)
#library(gam)
library(ranger)
library(zoo)
library(rpart)
library(rpart.plot)
#library(polycor)

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

g1 <- gam(Appliances~s(T1),data=data0)
g2 <- gam(Appliances~s(T2),data=data0)
g3 <- gam(Appliances~s(T3),data=data0)
g4 <- gam(Appliances~s(T4),data=data0)
par(mfrow=c(1,3))
plot(g1,se = TRUE)
plot(g2,se = TRUE)
plot(g3,se = TRUE)
plot(g4,se = TRUE)


rp <- rpart(Appliances~ InstantF, data = data0[s,],model=TRUE)
rpart.plot(rp,type=3,clip.right.labs=FALSE)

plot(data0$T1,data0$Appliances)

# On cherche les coeffictions fortement correlés à Appliencies
# on peut observer qu'il y a beaucoup de NA
cor(data_num)[,1]
which(is.na(cor(data_num)[,1]))


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

l <- 'Appliances ~T1'
#names(data_train)
g <- as.formula(l)
g0<-gam(g, data=data_train[s,])
summary(g0)
plot(g0,page=1)
par(new=TRUE)
plot(data0$T1,data0$Appliances)

step(glm(Appliances~1,family=gaussian,data_train), scope=~lights+T1+RH_1+T2+RH_2+T3+RH_3+T4+RH_4+T5+RH_5+T6+RH_6+T7+RH_7+T8+RH_8+T9+RH_9+T_out
                  +Press_mm_hg+RH_out+Windspeed+Visibility+Tdewpoint+rv1+rv2+NSM+Instant+Month+Posan+Heure+BE_load_actual_entsoe_transparency
                  +BE_load_forecast_entsoe_transparency+BE_wind_onshore_capacity+BE_wind_onshore_generation_actual+BE_wind_onshore_profile
                  +WeekStatus+Day_of_week+DayType+InstantF,direction = "forward")
step(lm(Appliances~1,data_train), scope=~lights+T1+RH_1+T2+RH_2+T3+RH_3+T4+RH_4+T5+RH_5+T6+RH_6+T7+RH_7+T8+RH_8+T9+RH_9+T_out
     +Press_mm_hg+RH_out+Windspeed+Visibility+Tdewpoint+rv1+rv2+NSM+Instant+Month+Posan+Heure+BE_load_actual_entsoe_transparency
     +BE_load_forecast_entsoe_transparency+BE_wind_onshore_capacity+BE_wind_onshore_generation_actual+BE_wind_onshore_profile
     +WeekStatus+Day_of_week+DayType+InstantF,direction = "forward")
Gam.object <- gam(Appliances,data=data0)
step.Gam(Appliances~1, scope=~lights+T1+RH_1+T2+RH_2+T3+RH_3+T4+RH_4,direction = "forward")

names(data_train)


######################################
######################################
###### Methode stepwise régréssion linéaire gam
#####################################""
#########################################
#data0<-data_num
cov <- names(data_train)[-1] 
n <- nrow(data0)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.7))
length(s)

l='Appliances ~'
eq_list =list()
for(k in c(1:length(cov)))
{
  eq_list[[k]] <-  paste0(l,paste0(cov[[k]]))
}
g<-lapply(eq_list, as.formula)
reg_list <- lapply(g, fitMod_gam, subset=s)
reg_list_forecast <- lapply(reg_list, predict, newdata=data0[s,])
rmse_list <- lapply(reg_list_forecast, rmse, data0[s,]$Appliances)
rmse_list <- rmse_list[-which(is.na(rmse_list))]

min_rmse<-min(unlist(rmse_list))
liste_petit<-list()
for (i in c(ncol(data_num):2)){
  if (unlist(rmse_list)[i]> 1.05*min_rmse)
  {
    data_train <- subset( data_train, select =-i)
  }
}

ncol(data_train)

fitMod_gam <- function(eq, subset)
{
  reg <- gam(eq, data=data0[subset,])
  return(reg)
}


cov <- head(names(data_train)[-1]) 
n <- nrow(data0)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.7))
length(s)

l='Appliances ~'
eq_list =list()

score = 1000
for(i in c(1:length(cov)))
{
  
  #for(k in c(1:(length(cov)-nb_fact)))
  #{
   # eq_list[[k]] <-  paste0(l, 's(',paste0(cov[[k]],')'))
  #}
  
  #for(k in c((length(cov)+1-nb_fact):length(cov)))
  #{
   # eq_list[[k]] <-  paste0(l,paste0(cov[[k]]))
  #}
  
  for(k in c(1:length(cov)))
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
    if (j<=(length(cov)-nb_fact))
    {
    l <- paste0(l, 's(',paste0(cov[j],')')) ## On rajoute le terme qui minimise à notre equation
    l <-paste0(l, '+')
    }
    if (j>(length(cov)-nb_fact))
    {
      l <- paste0(l,paste0(cov[j])) ## On rajoute le terme qui minimise à notre equation
      l <-paste0(l, '+')
    }
  }
  cov[-j]
  
}

gam(data0$Appliances~data0$T1)


l<-strtrim(l,(nchar(l)-1))
g <- as.formula(l)
Appliance.gam<-glm(g, data = data0)

step.model.test<-predict.lm(Appliance.gam, data_test)
which(is.na(step.model.test))



l<-"Appliances ~s(NSM)+s(T3)+s(T2)+s(T_out)+s(BE_load_forecast_entsoe_transparency)+s(RH_1)+s(RH_2)+s(RH_8)+s(T1)+s(RH_9)+s(BE_load_actual_entsoe_transparency)+s(RH_7)+s(Heure)+s(Windspeed)+s(T6)+s(RH_out)+s(Instant)"
g<-"Appliances ~s(NSM)+s(T3)+s(T2)+s(T_out)+s(BE_load_forecast_entsoe_transparency)+s(RH_1)+s(RH_2)+s(RH_8)+s(T1)+s(RH_9)+s(BE_load_actual_entsoe_transparency)+s(RH_7)+s(Heure)+s(Windspeed)+s(T6)+s(RH_out)+Instant+ WeekStatus+Day_of_week+DayType+InstantF"
g<-Appliances~InstantF+s(BE_load_forecast_entsoe_transparency)+s(BE_load_actual_entsoe_transparency)+s(lights)
g<-as.formula(g)

### résultat par step(glm())
gl<-'Appliances ~ InstantF + lights + T3 + T9 + T8 +T1 + Day_of_week + T6 + RH_3 + RH_4 + RH_7 + RH_2 + T2 + RH_1 + RH_out + RH_8 + RH_5
      + Windspeed + BE_wind_onshore_generation_actual + RH_6 + T7'
Appliance.glm<-glm(gl, data = data_train)

step.model.test<-predict.lm(Appliance.glm, data_test)
which(is.na(step.model.test))




#resultat par step(lm())
l<-'Appliances ~ InstantF + lights + T3 + T9 + T8 +T1 + Day_of_week + T6 + RH_3 + RH_4 + RH_7 + RH_2 + T2 + RH_1 
+ RH_out+ RH_8 + RH_5 + Windspeed + BE_wind_onshore_generation_actual + RH_6 + T7'
Appliance.lm <- lm(l, data0)
step.model.test<-predict.lm(Appliance.lm, data_test)
which(is.na(step.model.test))

Appliance.lm = lm(l,data0)
summary(Appliance.lm)
step.model.test<-predict(Appliance.lm, data_test)


submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- step.model.test

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



