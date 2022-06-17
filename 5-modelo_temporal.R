## DATOS Y LIBRERIAS -----
library(pbugs)
library(splines)

source("Scripts/1-datos.R")

## Modificamos el tiempo de los no vacunados
indice<-is.na(datos1516$daysvac)
datos1516$daysvac[indice]<-0

## Comprobamos que la variable daysvac es correcta:
sum(datos1516$daysvac[datos1516$r2015sfv==0]!=0) 
##Hay 1 individuo que tiene mal el dato de vacuna o el dato de tiempo
which(datos1516$daysvac[datos1516$r2015sfv==0]!=0)
datos1516<-datos1516[-which(datos1516$daysvac!=0 & 
                              datos1516$r2015sfv==0),]


## MODELO NO SEMANA VS MODELO TEMPORAL --------
## COMPARAMOS PARTE PARAMETRICA ----##
load("Resultados/Semana/modelo_predictiva_310.RData")
load("Resultados/Semana/modelo_semana_ar1_320.RData")
comparacion<-array(data=0,dim=c(9,4,2),
                   dimnames = list(rep("",9),
                                   c("Estimador","IC inf",
                                     "IC sup","Prob."),
                                   c("Modelo sin semana",
                                     "Modelo con semana")))

rownames(comparacion)<-c("num. hospi.","dias","sexo",
                         "nivel soc2","nivel soc3",
                         "nivel soc4","nivel soc5",
                         "nivel soc6","vacuna")

## MODELO SIN SEMANA
comparacion[,1,1]<-c(rpred$mean$beta.hospi,rpred$mean$beta.dias,
                     rpred$mean$b.sexo,rpred$mean$b.nivel,
                     rpred$mean$b.vacuna)
comparacion[,2,1]<-c(rpred$summary[10:9,3],rpred$summary[2,3],
                     rpred$summary[4:8,3],rpred$summary[3,3])
comparacion[,3,1]<-c(rpred$summary[10:9,7],rpred$summary[2,7],
                     rpred$summary[4:8,7],rpred$summary[3,7])
comparacion[,4,1]<-c(mean(rpred$sims.list$beta.hospi<0),
                     mean(rpred$sims.list$beta.dias<0),
                     mean(rpred$sims.list$b.sexo>0),
                     apply(rpred$sims.list$b.nivel>0,2,mean),
                     mean(rpred$sims.list$b.vacuna<0))

## MODELO CON SEMANA
comparacion[,1,2]<-c(rstemp$mean$beta.hospi,rstemp$mean$beta.dias,
                     rstemp$mean$b.sexo,rstemp$mean$b.nivel,
                     rstemp$mean$b.vacuna)
comparacion[,2,2]<-c(rstemp$summary[10:9,3],rstemp$summary[2,3],
                     rstemp$summary[4:8,3],rstemp$summary[3,3])
comparacion[,3,2]<-c(rstemp$summary[10:9,7],rstemp$summary[2,7],
                     rstemp$summary[4:8,7],rstemp$summary[3,7])
comparacion[,4,2]<-c(mean(rstemp$sims.list$beta.hospi<0),
                     mean(rstemp$sims.list$beta.dias<0),
                     mean(rstemp$sims.list$b.sexo>0),
                     apply(rstemp$sims.list$b.nivel>0,2,mean),
                     mean(rstemp$sims.list$b.vacuna<0))

## El efecto de la vacuna disminuye en el segundo modelo. El nivel socio económico tb tiene un efecto distinto. El resto de efectos son mas o menos similares. 

## COMPARAMAOS VARIABLE SUAVIZADA ----###
base<-ns(x=datos1516$yoa,df=6,intercept = F) 
plot(datos1516$yoa,rpred$mean$beta.edad%*%t(base),
     xlab="Edad",ylab="Edad suavizada")
points(datos1516$yoa,rstemp$mean$beta.edad%*%t(base),col="red")

## COMPROBAMOS EFECTO VACUNA A LO LARGO DEL TIEMPO (MOD. LINEAL) -------

modelo<-function(){
  for (i in 1:N) {
    gripe[i]~dbern(pi[i])
    logit(pi[i])<-inprod2(base.edad[i,],beta.edad[]) + 
      beta.hospi*hospi[i] + beta.dias*dias[i] + 
      b.sexo[sexo[i]] + b.nivel[nivel[i]] + et[semana[i]] + 
      (b.vacuna[vacuna[i]] + b.tiempo.vac*tiempo[i])
  }
  
  ## EFECTO TEMPORAL DE LA SEMANA:
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
  
  b.tiempo.vac~dflat()
  
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
                          base.edad=base, sexo=as.numeric(sex),
                          vacuna=as.numeric(r2015sfv), 
                          semana=as.numeric(epi_week),
                          hospi=as.numeric(hhm),
                          dias=as.numeric(lablag),
                          nivel=as.numeric(soc),
                          tiempo=daysvac,
                          media.tiempo=media.tiempo,
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
                            b.tiempo.vac=0.2)

param<- c("beta0.et","b.sexo","b.vacuna","b.nivel","beta.dias",
          "beta.hospi","beta.edad","sd.et","et","beta1.et",
          "t.mitad","pi","gripe.pred","residuos","tiempo0")

rstiempo<- pbugs(data = dat, inits = iniciales, 
                 parameters.to.save = param, model.file = modelo,
                 n.iter=20000,n.burnin=2000,n.thin=20)##,debug=T)

save(rstiempo,
     file="Resultados/Semana/modelo_semana_tiempo_290.RData")

## MODELO SIN TIEMPO VACUNA VS MODELO TIEMPO VACUNA --------
## COMPARAMOS PARTE PARAMETRICA ----##
load("Resultados/Semana/modelo_semana_tiempo_290.RData")
load("Resultados/Semana/modelo_semana_ar1_320.RData")
comparacion_vac<-array(data=0,dim=c(9,4,2),
                       dimnames = list(rep("",9),
                                       c("Estimador","IC inf",
                                         "IC sup","Prob."),
                                       c("Modelo sin tiempo vacuna",
                                         "Modelo con tiempo vacuna")))

rownames(comparacion)<-c("num. hospi.","dias","sexo",
                         "nivel soc2","nivel soc3",
                         "nivel soc4","nivel soc5",
                         "nivel soc6","vacuna")

## MODELO SIN TIEMPO
comparacion_vac[,,1]<-comparacion[,,2]

## MODELO CON TIEMPO
comparacion_vac[,1,2]<-c(rstiempo$mean$beta.hospi,
                         rstiempo$mean$beta.dias,
                         rstiempo$mean$b.sexo,
                         rstiempo$mean$b.nivel,
                         rstiempo$mean$b.vacuna)
comparacion_vac[,2,2]<-c(rstiempo$summary[10:9,3],
                         rstiempo$summary[2,3],
                         rstiempo$summary[4:8,3],
                         rstiempo$summary[3,3])
comparacion_vac[,3,2]<-c(rstiempo$summary[10:9,7],
                         rstiempo$summary[2,7],
                         rstiempo$summary[4:8,7],
                         rstiempo$summary[3,7])
comparacion_vac[,4,2]<-c(mean(rstiempo$sims.list$beta.hospi<0),
                         mean(rstiempo$sims.list$beta.dias<0),
                         mean(rstiempo$sims.list$b.sexo>0),
                         apply(rstiempo$sims.list$b.nivel>0,2,mean),
                         mean(rstiempo$sims.list$b.vacuna<0))

## Los efectos sin similares en ambos modelos. Solo destaca el factor vacuna, pues hay un aumento considerable de su efecto y, además, la significatividad aumenta. 

## COMPARAMAOS VARIABLE SUAVIZADA ----###
base<-ns(x=datos1516$yoa,df=6,intercept = F) 
plot(datos1516$yoa,rstemp$mean$beta.edad%*%t(base),
     xlab="Edad",ylab="Edad suavizada")
points(datos1516$yoa,rstiempo$mean$beta.edad%*%t(base),col="red")