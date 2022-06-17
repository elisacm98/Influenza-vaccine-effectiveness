## DATOS Y LIBRERIAS -----
library(pbugs)
library(splines)
source("Scripts/1-datos.R")

## MODELO 1: Metemos TODAS las variables EXCEPTO HOSPITAL ----
modelo_todas5 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.consultas[consultas[i]+1] + 
      b.cormo[cormo[i]+1] + b.vacuna[vacuna[i]]  
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  for (j in 1:6) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat()
  b.vacuna[2]~dflat()
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  for (j in 2:5){
    b.consultas[j]~dflat()
  }
  for (j in 2:4){
    b.cormo[j]~dflat()
  }
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  b.consultas[1]<-0
  b.cormo[1]<-0
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),nivel=as.numeric(soc),
                          consultas=as.numeric(as.vector(gp)),
                          cormo=as.numeric(as.vector(riskR)),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            b.consultas=c(NA,rnorm(4,0,0.5)),
                            b.cormo=c(NA,rnorm(3,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5))

param<- c("beta0", "pi","b.sexo","b.vacuna","b.nivel","b.consultas",
          "b.cormo","beta.dias","beta.hospi","beta.edad")

rmodelo1<- pbugs(data = dat, inits = iniciales,
                 parameters.to.save = param, 
                 model.file = modelo_todas5,
                 n.iter=10000,n.burnin=1000,n.thin=10)

save(rmodelo1,
     file="Resultados/Seleccion/modelo_todas_180.RData")
rm(dat,iniciales,param,rmodelo1)

## MODELO 2: Eliminamos cormorbilidades del MODELO 1 ----
modelo_nocormo5 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] 
    + b.sexo[sexo[i]] + b.nivel[nivel[i]] + 
      b.consultas[consultas[i]+1] + b.vacuna[vacuna[i]]  
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  for (j in 1:6) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat()
  b.vacuna[2]~dflat()
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  for (j in 2:5){
    b.consultas[j]~dflat()
  }
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  b.consultas[1]<-0
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          consultas=as.numeric(as.vector(gp)),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            b.consultas=c(NA,rnorm(4,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5))

param<- c("beta0", "pi","b.sexo","b.vacuna","b.nivel",
          "b.consultas","beta.dias","beta.hospi","beta.edad")

rmodelo5<- pbugs(data = dat, inits = iniciales,
                 parameters.to.save = param,
                 model.file = modelo_nocormo5,
                 n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo5,
     file="Resultados/Seleccion/modelo_nocormo_440.RData")
rm(dat,iniciales,param,rmodelo5)

## MODELO 3: Eliminamos consultas del MODELO 2 ----
modelo_nocormo_nocons5 <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) +
      beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]]  
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:6) {
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

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

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
                            beta.edad=rnorm(6,0,0.5))

param<- c("beta0", "pi","b.sexo","b.vacuna","b.nivel",
          "beta.dias","beta.hospi","beta.edad")

rmodelo7 <- pbugs(data = dat, inits = iniciales, 
                  parameters.to.save = param, 
                  model.file = modelo_nocormo_nocons5,
                  n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo7,
     file="Resultados/Seleccion/modelo_nocormo_nocons_210.RData")
rm(rmodelo7,param,dat,iniciales)

## MODELO 4: Eliminamos sexo del MODELO 3 ----
modelo_nivdihosvac <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] +  b.nivel[nivel[i]] + 
      b.vacuna[vacuna[i]]  
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:6) {
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

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base,
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5))

param<- c("beta0", "pi","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad")

rmodelo8 <- pbugs(data = dat, inits = iniciales, 
                  parameters.to.save = param, 
                  model.file = modelo_nivdihosvac,
                  n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo8,
     file="Resultados/Seleccion/modelo_hos_di_niv_vac_640.RData")
rm(dat,iniciales,param,rmodelo8)

## MODELO 5: Metemos interaccion (con cat.) en el MODELO 3 ----
modelo_nocormo_nocons_int <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i]+ b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + 
      b.sex.vac[sexo[i],vacuna[i]] + b.niv.vac[nivel[i],vacuna[i]]
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat() 
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  for (j in 1:6) {
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
  
  
  for (j in 2:6) {
    b.niv.vac[j,2]~dflat()
  }
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  
  b.sex.vac[1,1]<-0
  b.sex.vac[1,2]<-0
  b.sex.vac[2,1]<-0
  
  for (j in 1:6) {
    b.niv.vac[j,1]<-0
  }
  
  b.niv.vac[1,2]<-0
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

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
                            beta.edad=rnorm(6,0,0.5),
                            b.sex.vac=matrix(c(NA,NA,NA,
                                               rnorm(1,0,0.5)),
                                             ncol = 2),
                            b.niv.vac=matrix(c(rep(NA,7),
                                               rnorm(5,0,0.5)),
                                             ncol = 2))


param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad", "b.sex.vac","b.niv.vac","pi")

rmodelo9 <- pbugs(data = dat, inits = iniciales, 
                  parameters.to.save = param, 
                  model.file = modelo_nocormo_nocons_int, 
                  n.iter=30000,n.burnin=3000,n.thin=30)

save(rmodelo9,
     file="Resultados/Seleccion/modelo_nocormo_nocons_int_380.RData")
rm(rmodelo9,param,dat,iniciales,resultados)

## MODELO 6: Consideramos solo inter sex:vac en MODELO 5 ----
modelo_nocormo_nocons_sexvac <- function() {
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
  
  for (j in 1:6) {
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

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

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
                            beta.edad=rnorm(6,0,0.5),
                            b.sex.vac=matrix(c(NA,NA,NA,
                                               rnorm(1,0,0.5)),
                                             ncol = 2))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad", "b.sex.vac","pi")

rmodelo10<- pbugs(data = dat, inits = iniciales, 
                  parameters.to.save = param, 
                  model.file = modelo_nocormo_nocons_sexvac,
                  n.iter=20000,n.burnin=2000,n.thin=20)

save(rmodelo,
     file="Resultados/Seleccion/modelo_nocormo_nocons_sexvac_510.RData")
rm(rmodelo10,param,dat,iniciales)

## MODELO 7: Metemos interaccion (todas) en el MODELO 3 ----
datos1516$hospi.vac<-datos1516$hhm ##Creamos var. interaccion con hospi. 
for(i in 1:length(datos1516$hhm)){
  if(datos1516$r2015sfv[i]==0){
    datos1516$hospi.vac[i]<-0
  }
}
datos1516$dias.vac<-datos1516$lablag ##Creamos var. interaccion con dias 
for(i in 1:length(datos1516$lablag)){
  if(datos1516$r2015sfv[i]==0){
    datos1516$dias.vac[i]<-0
  }
}

modelo_nocormo_nocons_inttodas <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + 
      b.sex.vac[sexo[i],vacuna[i]] + b.niv.vac[nivel[i],vacuna[i]] + 
      b.hospi.vac*hospi.vac[i] + b.dias.vac*dias.vac[i]
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat()
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:6) {
    beta.edad[j] ~ dflat()  ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat() 
  b.vacuna[2]~dflat()
  
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## PREVIAS INTERACCIONES: 
  b.sex.vac[2,2]~dflat()
  
  
  for (j in 2:6) {
    b.niv.vac[j,2]~dflat()
  }
  
  b.hospi.vac~dflat()
  b.dias.vac~dflat()
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  
  b.sex.vac[1,1]<-0
  b.sex.vac[1,2]<-0
  b.sex.vac[2,1]<-0
  
  for (j in 1:6) {
    b.niv.vac[j,1]<-0
  }
  
  b.niv.vac[1,2]<-0
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          dias.vac=as.numeric(dias.vac),
                          hospi.vac=as.numeric(hospi.vac),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5),
                            b.sex.vac=matrix(c(NA,NA,NA,
                                               rnorm(1,0,0.5)),
                                             ncol = 2),
                            b.niv.vac=matrix(c(rep(NA,7),
                                               rnorm(5,0,0.5)),
                                             ncol = 2),
                            b.hospi.vac=c(rnorm(1,0,0.5)),
                            b.dias.vac=c(rnorm(1,0,0.5)))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad", "b.sex.vac","b.niv.vac",
          "b.hospi.vac","b.dias.vac","pi")

rmodelo11<- pbugs(data = dat, inits = iniciales, 
                  parameters.to.save = param, 
                  model.file = modelo_nocormo_nocons_inttodas,
                  n.iter=30000,n.burnin=3000,n.thin=30)

save(rmodelo11,
     file="Resultados/Seleccion/modelo_nocormo_nocons_int_todas_460.RData")
rm(rmodelo11,param,dat,iniciales,datos1516$hospi.vac)


## MODELO 8: Eliminamos hospi:vac del MODELO 7 ----
modelo_nocormo_nocons_nohospivac <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + 
      b.sex.vac[sexo[i],vacuna[i]] + b.niv.vac[nivel[i],vacuna[i]] + 
      b.dias.vac*dias.vac[i]
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat()
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:6) {
    beta.edad[j] ~ dflat()  ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat() 
  b.vacuna[2]~dflat()
  
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## PREVIAS INTERACCIONES: 
  b.sex.vac[2,2]~dflat()
  
  
  for (j in 2:6) {
    b.niv.vac[j,2]~dflat()
  }
  
  b.dias.vac~dflat()
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  
  b.sex.vac[1,1]<-0
  b.sex.vac[1,2]<-0
  b.sex.vac[2,1]<-0
  
  for (j in 1:6) {
    b.niv.vac[j,1]<-0
  }
  
  b.niv.vac[1,2]<-0
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          dias.vac=as.numeric(dias.vac),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5),
                            b.sex.vac=matrix(c(NA,NA,NA,
                                               rnorm(1,0,0.5)),
                                             ncol = 2),
                            b.niv.vac=matrix(c(rep(NA,7),
                                               rnorm(5,0,0.5)),
                                             ncol = 2),
                            b.dias.vac=c(rnorm(1,0,0.5)))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad", "b.sex.vac","b.niv.vac",
          "b.dias.vac","pi")

rmodelo12 <- pbugs(data = dat, inits = iniciales, 
                   parameters.to.save = param, 
                   model.file = modelo_nocormo_nocons_nohospivac,
                   n.iter=30000,n.burnin=3000,n.thin=30)

save(rmodelo12,
     file = "Resultados/Seleccion/modelo_nocormo_nocons_nohospivac_220.RData")
rm(rmodelo12,param,dat,iniciales,datos1516$hospi.vac)



## MODELO 9: Eliminamos nivel:vac del modelo 8 ----
modelo_nocormo_nocons_sexvac_diasvac <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + 
      b.sex.vac[sexo[i],vacuna[i]] + 
      b.dias.vac*dias.vac[i]
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat()
  beta.hospi ~ dflat() 
  beta.dias ~ dflat()
  
  for (j in 1:6) {
    beta.edad[j] ~ dflat()  ## PREVIAS SPLINES
  }
  
  ## PREVIAS DISCRETAS:  
  b.sexo[2]~dflat() 
  b.vacuna[2]~dflat()
  
  for (j in 2:6){
    b.nivel[j]~dflat()
  }
  
  ## PREVIAS INTERACCIONES: 
  b.sex.vac[2,2]~dflat()
  
  b.dias.vac~dflat()
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  
  b.sex.vac[1,1]<-0
  b.sex.vac[1,2]<-0
  b.sex.vac[2,1]<-0
  
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          dias.vac=as.numeric(dias.vac),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5),
                            b.sex.vac=matrix(c(NA,NA,NA,
                                               rnorm(1,0,0.5)),
                                             ncol = 2),
                            b.dias.vac=c(rnorm(1,0,0.5)))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad", "b.sex.vac",
          "b.dias.vac","pi")

rmodelo13 <- pbugs(data = dat, inits = iniciales, 
                   parameters.to.save = param, 
                   model.file = modelo_nocormo_nocons_nohospivac,
                   n.iter=30000,n.burnin=3000,n.thin=30)

save(rmodelo13,
     file = "Resultados/Seleccion/modelo_nocormo_nocons_sexvac_diasvac_480.RData")
rm(rmodelo13,param,dat,iniciales,datos1516$dias.vac)


## COMPROBACION DE LOS DF DEL SUAVIZADO ------
source("Scripts/Seleccion/nodos_suavizado.R")
c(rmodelo7$DIC,rmodelo7k7$DIC,rmodelo7k3$DIC) ## k=5 
c(rmodelo8$DIC,rmodelo8k7$DIC,rmodelo8k3$DIC) ## k=5 
c(rmodelo10$DIC,rmodelo10k7$DIC,rmodelo10k3$DIC) ## k=5

## Comparamos los suavizados de manera grafica (modelo 7):
base1<-ns(x=datos1516$yoa,df=8,intercept = F) 
plot(datos1516$yoa,rmodelo7k7$mean$beta.edad%*%t(base1),main="Distintos suavizados edad modelo 7")
load("Resultados/Seleccion/modelo_nocormo_nocons_210.RData")
base2<-ns(x=datos1516$yoa,df=6,intercept = F) 
points(datos1516$yoa,rmodelo7$mean$beta.edad%*%t(base2),col="red")
base3<-ns(x=datos1516$yoa,df=4,intercept=F)
points(x=datos1516$yoa,rmodelo7k3$mean$beta.edad%*%t(base3),col="green")

## Modelo 8:
plot(datos1516$yoa,rmodelo8k7$mean$beta.edad%*%t(base1),main="Distintos suavizados edad modelo 8")
load("Resultados/Seleccion/modelo_hos_di_niv_vac_640.RData")
points(datos1516$yoa,rmodelo8$mean$beta.edad%*%t(base2),col="red")
points(x=datos1516$yoa,rmodelo8k3$mean$beta.edad%*%t(base3),col="green")

## Modelo 10:
plot(datos1516$yoa,rmodelo10k7$mean$beta.edad%*%t(base1),main="Distintos suavizados edad modelo 10")
load("Resultados/Seleccion/modelo_nocormo_nocons_sexvac_510.RData")
points(datos1516$yoa,rmodelo10$mean$beta.edad%*%t(base2),col="red")
points(x=datos1516$yoa,rmodelo10k3$mean$beta.edad%*%t(base3),col="green")


## SELECCIONAMOS MEJOR MODELO HASTA AHORA: -----
source("Scripts/3.2-seleccion_modelos.R")

