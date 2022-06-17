## DATOS Y LIBRERIAS -----
library(pbugs)
library(splines)
source("Scripts/1-datos.R")

## MODELO 3.2: Modelo 3 con k=8 -----
modelo_nocormo_nocons7 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[])
    + beta.hospi*hospi[i] + beta.dias*dias[i] 
    + b.sexo[sexo[i]] + b.nivel[nivel[i]] + b.vacuna[vacuna[i]]  
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:8) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat()
  b.vacuna[2]~dflat()
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
}

base<-ns(x=datos1516$yoa,df=8,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(8,0,0.5))

param<- c("beta0", "pi","b.sexo","b.vacuna","b.nivel",
          "beta.dias","beta.hospi","beta.edad")

rmodelo7k7 <- pbugs(data = dat, inits = iniciales, parameters.to.save = param, model.file = modelo_nocormo_nocons7,n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo7k7,
     file = "Resultados/Seleccion/modelo_nocormo_nocons7_460.RData")
rm(rmodelo7k7,param,dat,iniciales)

## MODELO 3.3: Modelo 3 con k=4 -----
modelo_nocormo_nocons3 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + beta.hospi*hospi[i] + beta.dias*dias[i] 
    + b.sexo[sexo[i]] + b.nivel[nivel[i]] + b.vacuna[vacuna[i]]  
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:4) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat()
  b.vacuna[2]~dflat()
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
}

base<-ns(x=datos1516$yoa,df=4,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(4,0,0.5))

param<- c("beta0", "pi","b.sexo","b.vacuna","b.nivel",
          "beta.dias","beta.hospi","beta.edad")

rmodelo7k3<- pbugs(data = dat, inits = iniciales, 
                   parameters.to.save = param, 
                   model.file = modelo_nocormo_nocons3,
                   n.iter=20000,n.burnin=2000,n.thin=20)

load(rmodelo7k3,
     file = "Resultados/Seleccion/modelo_nocormo_nocons3_390.RData")
rm(rmodelo7k3,param,dat,iniciales)

## MODELO 4.2: Modelo 4 con k=8 ----
modelo_nivdihosvac7 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] +  
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]]  
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:8) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.vacuna[2]~dflat()
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## RESTRICCIONES: 
  b.vacuna[1]<-0
  b.nivel[1]<-0
}

base<-ns(x=datos1516$yoa,df=8,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base,
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(8,0,0.5))

param<- c("beta0", "pi","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad")

rmodelo8k7 <- pbugs(data = dat, inits = iniciales, 
                    parameters.to.save = param, 
                    model.file = modelo_nivdihosvac7
                    ,n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo8k7,
     file = "Resultados/Seleccion/modelo_hos_di_niv_vac7_670.RData")
rm(rmodelo8k7,iniciales,param,dat)

## MODELO 4.3: Modelo 4 con k=4 ----
modelo_nivdihosvac3 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] +  
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]]  
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:4) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.vacuna[2]~dflat()
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## RESTRICCIONES: 
  b.vacuna[1]<-0
  b.nivel[1]<-0
}

base<-ns(x=datos1516$yoa,df=4,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base,
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(4,0,0.5))

param<- c("beta0", "pi","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad")

rmodelo8k3 <- pbugs(data = dat, inits = iniciales, 
                    parameters.to.save = param, 
                    model.file = modelo_nivdihosvac3,
                    n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo8k3,
     file="Resultados/Seleccion/modelo_hos_di_niv_vac3_530.RData")
rm(rmodelo8k3,dat,iniciales,param)

## MODELO 8.2: Modelo 8 con k=8 ----
modelo_nocormo_nocons_sexvac7 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + 
      b.sex.vac[sexo[i],vacuna[i]]
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  for (j in 1:8) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat()
  b.vacuna[2]~dflat()
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## PREVIAS INTERACCIONES: 
  b.sex.vac[2,2]~dflat()
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  
  b.sex.vac[1,1]<-0
  b.sex.vac[1,2]<-0
  b.sex.vac[2,1]<-0
  
}

base<-ns(x=datos1516$yoa,df=8,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(8,0,0.5),
                            b.sex.vac=matrix(c(NA,NA,NA,
                                               rnorm(1,0,0.5)),
                                             ncol = 2))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad", "b.sex.vac","pi")

rmodelo10k7 <- pbugs(data = dat, inits = iniciales, 
                    parameters.to.save = param, 
                    model.file = modelo_nocormo_nocons_sexvac7,
                    n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo10k7,
     file = "Resultados/Seleccion/modelo_nocormo_nocons_sexvac7_420.RData")
rm(rmodelo10k7,param,dat,iniciales)

## MODELO 8.3: Modelo 8 con k=4 ----
modelo_nocormo_nocons_sexvac3 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + 
      b.sex.vac[sexo[i],vacuna[i]]
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  for (j in 1:4) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat()
  b.vacuna[2]~dflat()
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## PREVIAS INTERACCIONES: 
  b.sex.vac[2,2]~dflat()
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  
  b.sex.vac[1,1]<-0
  b.sex.vac[1,2]<-0
  b.sex.vac[2,1]<-0
  
}

base<-ns(x=datos1516$yoa,df=4,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(4,0,0.5),
                            b.sex.vac=matrix(c(NA,NA,NA,
                                               rnorm(1,0,0.5)),
                                             ncol = 2))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad", "b.sex.vac","pi")

rmodelo10k3 <- pbugs(data = dat, inits = iniciales, 
                    parameters.to.save = param, 
                    model.file = modelo_nocormo_nocons_sexvac3,
                    n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo10k3,
     file = "Resultados/Seleccion/modelo_nocormo_nocons_sexvac3_260.RData")
rm(rmodelo10k3,param,dat,iniciales)

