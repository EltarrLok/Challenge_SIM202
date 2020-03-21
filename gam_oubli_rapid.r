
######################################
######################################
###### Methode forward gam oubli rapide
#####################################
#########################################

#data_num <- subset( data0, select = -c(date,WeekStatus,Day_of_week,DayType, InstantF))

fitMod_gam <- function(eq, subset)
{
  reg <- gam(eq, data=data0[subset,])
  print(eq)
  return(reg)
}

cov <- names(data_train)[-c(1:2)] 
n <- nrow(data0)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.7))
length(s)

l='Appliances ~'
eq_list =list()

score = 1000

for(i in c(2:length(cov)))
{
  for(k in c(1:length(cov))){
    
    eq_list[[k]] <-  paste0(l,paste0(cov[[k]]))
  }
  
  g<-lapply(eq_list, as.formula)
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