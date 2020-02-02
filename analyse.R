rm(list = objects())
graphics.off()
setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory

library(readr)

data <- read_delim("train.csv", col_names = TRUE, delim = ",")
# Pas besoin de mettre une date car il y en a deja une
# Tracer variables en fonction d'une autre, voir correlation etc

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
s = max(h$counts)
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







#plot(data$date<"2016-02-01", data$T1-data$T2, type = 'h')


#plot(data$date, data$T1-data$T2, type = 'h')

#selectByDate(
  data,
  start = "1/1/2016",
  end = "01/02/2016",
  year = 2016,
  month = 1,
  day = "weekday",
  hour = 1
)


# CONSTRUIR UN MODELE DE REGRESSION LINEAIRE
n<-length(data$Appliances)/2

set.seed(100)
#rnorm(13)
mysample <- data[sample(1:nrow(data), n, replace=T),] 
print(mysample)

s<-sample(data)

s
cov<-head(names(data)[-c(1,2)],30)



