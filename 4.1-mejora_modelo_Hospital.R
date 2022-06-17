## DATOS Y LIBRERIAS -----
library(pbugs)
library(splines)

source("Scripts/1-datos.R")

## MEJOR MODELO: GRIPE~HOSPI+DIAS+SEXO+NIVEL+VACUNA+s(EDAD) ------
modelo <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + 
      b.sexo[sexo[i]] + b.nivel[nivel[i]] + b.vacuna[vacuna[i]]  
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
                            beta.edad=rnorm(6,0,0.5))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad", "pi")

resultados <- pbugs(data = dat, inits = iniciales, 
                    parameters.to.save = param, model.file = modelo, 
                    n.iter=35000,n.burnin=3500,n.thin=35)
save(resultados,
     file="Resultados/Hospital/modelo_simple_670.RData")

### LA EDAD SUAVIZADA RESULTA SIGNIFICATIVA ??? 
edad<-resultados$sims.list$beta.edad
cbind(resultados$mean$beta.edad,
      c(mean(edad[,1]<0),
        mean(edad[,2]>0),
        mean(edad[,3]>0),
        mean(edad[,4]<0),
        mean(edad[,5]>0),
        mean(edad[,6]<0))) ##SI

rm(resultados,param,dat,iniciales)
## ¿¿Y SI METEMOS HOSPITAL COMO EFECTO FIJO??  ----
modelo <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) 
    + beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] 
    + b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + b.hospital[hospital[i]] 
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
  for (j in 2:4){
    b.hospital[j]~dflat()
  }
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  b.hospital[1]<-0
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          hospital=as.numeric(hospital),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5),
                            b.hospital=c(NA,rnorm(3,0,0.5)))

param<- c("beta0", "pi","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad","b.hospital")

rhfijos <- pbugs(data = dat, inits = iniciales, 
                 parameters.to.save = param,model.file = modelo, 
                 n.iter=20000,n.burnin=2000,n.thin=20)
save(resultados,
     file="Resultados/Hospital/modelo_simple_fijos_510.RData")

## Veamos el efecto del hospital:
hospital<-rhfijos$sims.list$b.hospital
cbind(rhfijos$mean$b.hospital,
      c(mean(hospital[,1]>0),
        apply(hospital[,2:3]<0,2,mean))) ## No parece haber un efecto hospital

rm(rhfijos,param,iniciales,dat)

## ¿¿ HOSPITAL COMO EFECTO ALEATORIO??-----
modelo <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) 
    + beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] 
    + b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + ea[hospital[i]] 
  }
  
  for (i in 1:4) {
    ea[i] ~dnorm(0,tau.hospital)
  }
  
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat()
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  ## EFECTO ALEATORIO HOSPITAL:
  tau.hospital <- pow(sd.hospital,-2)
  sd.hospital ~ dunif(0,100)
  
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
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          hospital=as.numeric(hospital),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5),
                            ea=rnorm(4,0,0.5),
                            sd.hospital = runif(1,0,10))

param<- c("beta0", "pi","b.sexo","b.vacuna",
          "b.nivel","beta.dias","beta.hospi",
          "beta.edad","ea", "sd.hospital")

rhale <- pbugs(data = dat, inits = iniciales, 
               parameters.to.save = param, model.file = modelo, 
               n.iter=30000,n.burnin=3000,n.thin=30)

save(rhale,
     file="Resultados/Hospital/modelo_simple_aleatorios.400.RData")
## COMPARACION MODELOS ----

load("Resultados/Hospital/modelo_simple_670.RData")
cbind(resultados$DIC,rhale$DIC) ## El modelo con menor DIC es el que NO TIENE HOSPITAL

## Veamos el efecto del hospital:
hospital<-resultadoseah$sims.list$ea
cbind(resultadoseah$mean$ea,
      c(apply(hospital[,1:2]>0,2,mean),
        apply(hospital[,3:4]<0,2,mean))) ## No parece haber un efecto hospital

