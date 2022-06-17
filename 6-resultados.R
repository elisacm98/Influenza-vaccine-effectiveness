### ---- CARGA DATOS +LIBRERIAS ----------------
load("Resultados/Semana/modelo_semana_tiempo_290.RData")
load("Resultados/Semana/modelo_predictiva_310.RData")
library(dplyr)
library(ggplot2)
library(splines)
source("Scripts/1-datos.R")

### ---- TABLA MODELO NO SEMANA VS MODELO DECAIMIENTO -----------
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

## MODELO DECAIMIENTO
comparacion[,1,2]<-c(rstiempo$mean$beta.hospi,rstiempo$mean$beta.dias,
                     rstiempo$mean$b.sexo,rstiempo$mean$b.nivel,
                     rstiempo$mean$b.vacuna)
comparacion[,2,2]<-c(rstiempo$summary[10:9,3],rstiempo$summary[2,3],
                     rstiempo$summary[4:8,3],rstiempo$summary[3,3])
comparacion[,3,2]<-c(rstiempo$summary[10:9,7],rstiempo$summary[2,7],
                     rstiempo$summary[4:8,7],rstiempo$summary[3,7])
comparacion[,4,2]<-c(mean(rstiempo$sims.list$beta.hospi<0),
                     mean(rstiempo$sims.list$beta.dias<0),
                     mean(rstiempo$sims.list$b.sexo>0),
                     apply(rstiempo$sims.list$b.nivel>0,2,mean),
                     mean(rstiempo$sims.list$b.vacuna<0))
## Prob asociada a los meses transcurridos:
mean(rstiempo$sims.list$b.tiempo.vac>0)

### ---- ESTIMACION PARTE SUAVIZADA ---------------
## Comparación de edad en ambos modelos:
anyos<-sort(datos1516$yoa)
base<-ns(x=anyos,df=6,intercept = F)

dat<-data.frame("x"=anyos,
                "y1"=(rstiempo$mean$beta.edad%*%t(base))[1,],
                "y2"=(rpred$mean$beta.edad%*%t(base))[1,])

png("Graficas_latex/Resultados_modelo/compraracion_edad.png",
    width=580,
    height = 460)
ggplot(dat, aes(x=anyos)) + 
  geom_line(aes(x = anyos, y = y2,linetype="dashed")) + 
  labs(#title="Suavización variable edad",
    x="Edad",y="Edad suavizada") +
  scale_x_continuous(breaks = seq(0, 102, 10)) + 
  scale_y_continuous(breaks=NULL) + 
  geom_line(aes(x = anyos, y = y1,linetype="solid")) +
  scale_linetype_manual("", values=c("dashed","solid"),
                        labels = c("Modelo (3.1)",
                                   "Modelo (3.4)")) 
dev.off()

## Estimación edad junto con el intervalo de credibilidad:
anyos<-sort(datos1516$yoa)
base<-ns(x=anyos,df=6,intercept = F) 

dat<-data.frame("x"=anyos,"y"=(rstiempo$mean$beta.edad%*%t(base))[1,])
IC<-apply(rstiempo$sims.list$beta.edad,2,quantile,
          probs=c(0.025,0.975))
IC<-data.frame(x=anyos,IC1=(IC[1,]%*%t(base))[1,],
               IC2=(IC[2,]%*%t(base))[1,])

png("Graficas_latex/Resultados_modelo/edad_temporal.png",
    width=580,
    height = 460)
ggplot(dat, aes(x=anyos)) + 
  geom_line(aes(x = anyos, y = y,colour="black")) + 
  labs(#title="Suavización variable edad",
    x="Edad",y="Edad suavizada") +
  scale_x_continuous(breaks = seq(0, 102, 10)) + 
  scale_y_continuous(breaks=NULL) + 
  geom_line(aes(x = anyos, y = IC$IC2,colour="red")) + 
  geom_line(aes(x = anyos, y = IC$IC1,colour="red")) + 
  scale_colour_manual("", 
                      labels = c("Estimación","IC 95%"),
                      values = c("black","red","red"))
dev.off()
### ---- EFECTO COVARIABLES ----------
exp(c(rstiempo$mean$beta.hospi,rstiempo$mean$beta.dias,
      rstiempo$mean$b.sexo,rstiempo$mean$b.nivel,
      rstiempo$mean$b.vacuna,rstiempo$mean$b.tiempo.vac))
### ---- EFECTO SEMANA EPIDEMIOLÓGICA ---------------
IC<-apply(rstiempo$sims.list$et,2,quantile,
          probs=c(0.025,0.975))
png("Graficas_latex/Resultados_modelo/efecto_semana.png",
    width=580,
    height = 460)
ggplot() + geom_line(aes(x=1:25,y=rstiempo$mean$et,colour="black")) +
  labs(#title="Efecto de la semana epidemiológica",
    x="Semana epidemiológica",
    y=expression("logit("*pi[i]*")")) +
  ##scale_x_continuous(breaks = c(seq(24, 52, 1),seq(1,17,1))) + 
  scale_x_continuous(breaks = seq(1, 25, 1),labels=c(45:52,1:17)) +
  scale_y_continuous(breaks=seq(-7.25,0.75,0.5)) +
  geom_line(aes(x = 1:25, y = IC[1,],colour="red")) +
  geom_line(aes(x = 1:25, y = IC[2,],colour="red")) + 
  scale_colour_manual("", labels = c("Estimación","IC 95%"),
                      values = c("black","red","red"))
  
dev.off()

## Representamos ahora los odds:
png("Resultados/Resultados_modelo/efecto_semana_odds.png",
    width=580,
    height = 460)
ggplot() + geom_line(aes(x=1:25,y=exp(rstiempo$mean$et))) +
  labs(x="Semana epidemiológica",
       y="Efecto sobre los odds a favor de ser ingresado por gripe") +
  scale_x_continuous(breaks = seq(1, 25, 1),labels=c(45:52,1:17))
dev.off()

## Representamos ahora la probabilidad:
prob_semana<-exp(rstiempo$mean$et)/(1+exp(rstiempo$mean$et))

png("Graficas_latex/Resultados_modelo/efecto_semana_prob.png",
    width=580,
    height = 460)
ggplot() + geom_line(aes(x=1:25,y=prob_semana)) +
  labs(x="Semana epidemiológica",
       y="Probabilidad de ser ingresado por gripe") +
  scale_x_continuous(breaks = seq(1, 25, 1),labels=c(45:52,1:17)) + 
  scale_y_continuous(breaks=seq(0,0.4,0.05)) 
dev.off()
### ---- GRÁFICA DECAIMIENTO VACUNAL------
png("Graficas_latex/Resultados_modelo/decaimiento_vacunal.png",
    width=580,
    height = 460)

ggplot() + geom_line(aes(x=0:5,y=rstiempo$mean$b.vacuna +
                           rstiempo$mean$b.tiempo.vac*(0:5))) +
  labs(title=" ",
       x="Tiempo en meses",
       y=expression("logit("*pi[i]*")")) +
  scale_x_continuous(breaks = seq(0, 5, 0.5)) + 
  scale_y_continuous(breaks=seq(-1,2,0.25))
dev.off()
### ---- EFECTIVIDAD VACUNAL -----------------
ggplot() + 
  geom_line(aes(x=seq(0, 4, 0.5),
                y=(1-exp(rstiempo$mean$b.vacuna +
                           rstiempo$mean$b.tiempo.vac*seq(0, 4, 0.5)))*100)) +
  labs(title=" ",
       x="Tiempo en meses",
       y="Efectividad vacunal") +
  scale_x_continuous(breaks = seq(0, 4, 0.5)) + 
  scale_y_continuous(breaks=seq(0,65,5))