library(tidyverse)
dat.precip <- read_csv(file = "agacis.csv")

#data cleaning from lecture
dat.precip.long <- dat.precip |>    
  dplyr::select(-Annual) |>                   # Remove annual column 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr,   # pivot the column data into one col
                        May, Jun, Jul, Aug, 
                        Sep, Oct, Nov, Dec), 
               values_to = "Precipitation",   # store the values in Precipitation
               names_to = "Month") |>         # store the months in Month
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
                                   TRUE                 ~ Precipitation))|>
  mutate(Precipitation = as.numeric(Precipitation))

#part a
#log likelihood function for Gamma
llgamma <- function(par, data, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  ll <- sum(log(dgamma(x=data, shape=alpha, rate=beta)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

gamma.MLEs <- optim(fn = llgamma,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)
gamma.MLEs$par

#Part b
#log likelihood function for Log normal
lllognorm <- function(par, data, neg=F){
  mu <- par[1]
  sigma <- par[2]
  
  ll <- sum(log(dlnorm(x=data, meanlog = mu, sdlog = sigma)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

lognorm.MLEs <- optim(fn = lllognorm,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)
lognorm.MLEs$par


ll.weibull = -2166.496 #log-likelihood for Weibull
ll.gamma = -gamma.MLEs$value #log-likelihood for Gamma
ll.lognorm = -lognorm.MLEs$value #log-likelihood for Lognorm
#part c
(weibull.gamma = exp(ll.weibull - ll.gamma)) #likelihood ratio for Weibull and Gamma

#part d
(weibull.lognorm = exp(ll.weibull - ll.lognorm)) #likelihood ratio for Weibull and Lognorm

#part e
(gamma.lognorm = exp(ll.gamma - ll.lognorm)) #likelihood ratio for Gamma and Lognorm

