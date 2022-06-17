## DATOS Y LIBRERIAS -----
library(pbugs)
library(splines)

source("Scripts/1-datos.R")

## DISTRIBUCION PREDICTIVA -----
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
  
  ## DISTRIBUCION PREDICTIVA:
  for (i in 1:N) {
    gripe.pred[i] ~ dbern(pi[i])
    residuos[i]<-gripe.pred[i]-gripe[i]
  }
  
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
          "beta.hospi","beta.edad", "pi","gripe.pred","residuos")

rpred<- pbugs(data = dat, inits = iniciales, 
              parameters.to.save = param, model.file = modelo,
              n.iter=25000,n.burnin=2500,n.thin=25)

save(rpred,
     file="Resultados/Semana/modelo_predictiva_310.RData")
## ESTUDIO AUTOCORRELACION RESIDUOS ----
load("Resultados/Semana/modelo_predictiva_310.RData")
residuos<-rpred$mean$residuos
residuost<-aggregate(residuos, 
                     list(semana= datos1516$epi_week[-2625]), 
                     mean)
colnames(residuost)[2]<-c("resi")

png("Resultados/Semana/Autocorrelacion_nosemana.png")
par(mfrow=c(2,1),mai=c(0.9,0.9,0.7,0.2))
acf(residuost$resi,main="ACF modelo sin semana")
pacf(residuost$resi,main="PACF modelo sin semana")
dev.off()

## ¿¿Y SI METEMOS LA SEMANA COMO EFECTO FIJO?? ----
modelo <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + 
      b.sexo[sexo[i]] + b.nivel[nivel[i]] + 
      b.vacuna[vacuna[i]] + b.semana[semana[i]]
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
  for (j in 2:25) {
    b.semana[j]~dnorm(0,0.01)
  }
  
  ## RESTRICCIONES: 
  b.sexo[1]<-0
  b.vacuna[1]<-0
  b.nivel[1]<-0
  b.semana[1]<-0
  
  ## DISTRIBUCION PREDICTIVA:
  for (i in 1:N) {
    gripe.pred[i] ~ dbern(pi[i])
    residuos[i]<-gripe.pred[i]-gripe[i]
  }
  
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv), 
                          semana=as.numeric(epi_week),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.semana=c(NA,rnorm(24,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad","b.semana","pi","gripe.pred",
          "residuos")

rsfijos<- pbugs(data = dat, inits = iniciales, 
                parameters.to.save = param, model.file = modelo,
                n.iter=35000,n.burnin=3500,n.thin=35)
traceplot(resul,var.pattern = "^b.semana",not.converged = T)
traceplot(resul,var.pattern = "^pi",not.converged = T)

save(rsfijos,
     file = "Resultados/Semana/modelo_semana_fijos_130.RData")

## ESTUDIO AUTOCORRELACION RESIDUOS ----
load("Resultados/Semana/modelo_semana_fijos_130.RData")
residuos<-fijos$mean$residuos
residuost<-aggregate(residuos, 
                     list(semana= datos1516$epi_week), 
                     mean)
colnames(residuost)[2]<-c("resi")

png("Autocorrelacion_semanafijo.png")
par(mfrow=c(2,1),mai=c(0.9,0.9,0.7,0.2))
acf(residuost$resi,main="ACF modelo efectos fijos")
pacf(residuost$resi,main="PACF modelo efectos fijos")
dev.off()

## AJUSTE MODELOS:
cbind(rpred$DIC,rsfijos$DIC)

## ¿¿Y SI METEMOS LA SEMANA COMO EFECTO ALEATORIO??  ----
modelo <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- beta0 + inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + b.sexo[sexo[i]] + 
      b.nivel[nivel[i]] + b.vacuna[vacuna[i]] + ea[semana[i]]
  }
  
  for (i in 1:25) {
    ea[i] ~dnorm(0,tau.semana)
  }
  
  ## PREVIAS CONTINUAS:
  beta0 ~ dflat()
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  for (j in 1:6) {
    beta.edad[j] ~ dflat() ## PREVIAS SPLINES
  }
  
  ## EFECTO ALEATORIO SEMANA EPIDEMIOLÓGICA:
  tau.semana <- pow(sd.semana,-2)
  sd.semana ~ dunif(0,100)
  
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
  
  ## DISTRIBUCION PREDICTIVA:
  for (i in 1:N) {
    gripe.pred[i] ~ dbern(pi[i])
    residuos[i]<-gripe.pred[i]-gripe[i]
  }
  
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv), 
                          semana=as.numeric(epi_week),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            ea=c(rnorm(25,0,0.5)),
                            sd.semana=runif(1,0,5),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5))

param<- c("beta0","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad","ea","sd.semana","pi",
          "gripe.pred","residuos")

rsale<- pbugs(data = dat, inits = iniciales, 
              parameters.to.save = param, model.file = modelo,
              n.iter=35000,n.burnin=3500,n.thin=35)
traceplot(rsale,var.pattern = "^gripe.pred", not.converged = TRUE)

save(rsale,
     file="Resultados/Semana/modelo_semana_aleatorios_250.RData")

## ESTUDIO AUTOCORRELACION RESIDUOS ----
load("Resultados/Semana/modelo_semana_aleatorios_250.RData")
residuos<-rsale$mean$residuos
residuost<-aggregate(residuos, 
                     list(semana= datos1516$epi_week), mean)
colnames(residuost)[2]<-c("resi")

png("Resultados/Semana/Autocorrelacion_semanaale.png")
par(mfrow=c(2,1),mai=c(0.9,0.9,0.7,0.2))
acf(residuost$resi,main="ACF modelo efectos aleatorios")
pacf(residuost$resi,main="PACF modelo efectos aleatorios")
dev.off()

## ¿¿Y SI METEMOS LA SEMANA COMO EFECTO ALEATORIO TEMPORAL??  ----
modelo <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + 
      b.sexo[sexo[i]] + b.nivel[nivel[i]] + 
      b.vacuna[vacuna[i]]+et[semana[i]] 
  }
  
  ## EFECTO TEMPORAL:
  et[1] ~ dflat()
  
  for (i in 2:25) {
    et[i] ~ dnorm(media.et[i], tau.et)
    media.et[i] <-beta0.et+ beta1.et * et[i - 1]
  }
  
  ## PREVIAS CONTINUAS:
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  ## PREVIAS EFECTO TEMPORAL:
  tau.et <- pow(sd.et, -2)
  sd.et ~ dunif(0, 100)
  beta0.et ~ dflat()
  beta1.et ~ dflat()
  
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
  
  ## DISTRIBUCION PREDICTIVA:
  for (i in 1:N) {
    gripe.pred[i] ~ dbern(pi[i])
    residuos[i]<-gripe.pred[i]-gripe[i]
  }
  
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv), 
                          semana=as.numeric(epi_week),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0.et=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5),
                            et=c(rnorm(25,0,0.5)),
                            sd.et = runif(1, 0, 5), 
                            beta1.et = rnorm(1,0,0.5))

param<- c("beta0.et","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad","sd.et","et","beta1.et",
          "pi","gripe.pred","residuos")

rstemp<- pbugs(data = dat, inits = iniciales, 
               parameters.to.save = param, model.file = modelo,
               n.iter=25000,n.burnin=2500,n.thin=25)
traceplot(rstemp,var.pattern = "^et")
traceplot(rstemp,var.pattern = "^beta0.et")
traceplot(rstemp,var.pattern = "^beta1.et")
traceplot(rstemp,var.pattern = "^sd.et")

save(rstemp,
     file="Resultados/Semana/modelo_semana_ar1_320.RData")
## ESTUDIO AUTOCORRELACION RESIDUOS ----
load("Resultados/Semana/modelo_semana_ar1_320.RData")
residuos<-rstemp$mean$residuos
residuost<-aggregate(residuos, 
                     list(semana= datos1516$epi_week), mean)
colnames(residuost)[2]<-c("resi")

png("Resultados/Semana/Autocorrelacion_semanatemporal.png")
par(mfrow=c(2,1),mai=c(0.9,0.9,0.7,0.2))
acf(residuost$resi,main="ACF modelo efecto temporal")
pacf(residuost$resi,main="PACF modelo efecto temporal")
dev.off()

## AJUSTE MODELOS:
cbind(rpred$DIC,rsale$DIC,rstemp$DIC) ##El modelo que tiene en cuenta la dependencia temporal de las semanas es el que presenta un mejor ajuste


## ¿¿Y SI METEMOS LA SEMANA COMO EFECTO ALEATORIO TEMPORAL?? (AR2)  ----
modelo <- function() {
  for (i in 1:N) {
    gripe[i] ~ dbern(pi[i])
    logit(pi[i]) <- inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + 
      b.sexo[sexo[i]] + b.nivel[nivel[i]] + 
      b.vacuna[vacuna[i]]+et[semana[i]] 
  }
  
  ## EFECTO TEMPORAL:
  et[1] ~ dflat()
  et[2] ~ dflat() 
  
  for (i in 3:25) {
    et[i] ~ dnorm(media.et[i], tau.et)
    media.et[i] <-beta0.et+ beta1.et * et[i - 1] + beta2.et*et[i-2]
  }
  
  ## PREVIAS CONTINUAS:
  beta.hospi ~ dflat()
  beta.dias ~ dflat()
  
  ## PREVIAS EFECTO TEMPORAL:
  tau.et <- pow(sd.et, -2)
  sd.et ~ dunif(0, 100)
  beta0.et ~ dflat()
  beta1.et ~ dflat()
  beta2.et ~ dflat()
  
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
  
  ## DISTRIBUCION PREDICTIVA:
  for (i in 1:N) {
    gripe.pred[i] ~ dbern(pi[i])
    residuos[i]<-gripe.pred[i]-gripe[i]
  }
  
}

base<-ns(x=datos1516$yoa,df=6,intercept = F) ##Def. base splines

dat<- with(datos1516,list(gripe = as.numeric(as.vector(flu)), 
                          base.edad=base, 
                          sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv), 
                          semana=as.numeric(epi_week),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          N = length(flu)))

iniciales<- function() list(beta0.et=rnorm(1,0,0.5),
                            b.sexo=c(NA,rnorm(1,0,0.5)),
                            b.vacuna=c(NA,rnorm(1,0,0.5)),
                            b.nivel=c(NA,rnorm(5,0,0.5)),
                            beta.hospi=rnorm(1,0,0.5),
                            beta.dias=rnorm(1,0,0.5),
                            beta.edad=rnorm(6,0,0.5),
                            et=c(rnorm(25,0,0.5)),
                            sd.et = runif(1, 0, 5), 
                            beta1.et = rnorm(1,0,0.5),
                            beta2.et = rnorm(1,0,0.5))

param<- c("beta0.et","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad","sd.et","et","beta1.et",
          "beta2.et","pi","gripe.pred","residuos")

rstemp2<- pbugs(data = dat, inits = iniciales, parameters.to.save = param,
                model.file = modelo,n.iter=25000,n.burnin=2500,n.thin=25)
traceplot(rstemp,var.pattern = "^et")
traceplot(rstemp,var.pattern = "^beta0.et")
traceplot(rstemp,var.pattern = "^beta1.et")
traceplot(rstemp,var.pattern = "^sd.et")

save(rstemp2,file="Resultados/Semana/modelo_semana_ar2_320.RData")
cbind(rstemp$DIC, rstemp2$DIC)