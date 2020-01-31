rm(list = objects())

setwd("/home/lokmen/Documents/ENSTA/SIM202/building-appliances/")   # set working directory

library(readr)

data <- read_delim("train.csv", col_names = TRUE, delim = ",")
# Pas besoin de mettre une date car il y en a deja une
# Tracer variables en fonction d'une autre, voir correlation etc

#str(data)
#head(data)
#summary(data)

#plot(data$date, data$Appliances, type = 'l')
#hist(data$Appliances)


#plot(data$periode, data$T1-data$T2, type = 'h')


#plot(data$date, data$T1-data$T2, type = 'h')

selectByDate(
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



