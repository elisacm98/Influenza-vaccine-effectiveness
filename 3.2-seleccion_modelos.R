## -- MATRIZ ----
selec<-array(data=0,dim=c(26,2,20),
             dimnames = list(rep("",30),
                             c("Estimador","Prob."),
                             c(rep(" ",20))))

rownames(selec)<-c("intercept","num. hospi.","hospi.vac2","dias",
                   "dias.vac2","sexo","sex.vac2","nivel soc2",
                   "nivel soc3","nivel soc4","nivel soc5",
                   "nivel soc6","niv.vac2","niv.vac3","niv.vac4",
                   "niv.vac5","niv.vac6","consultas2","consultas3",
                   "consultas4","consultas5","cormo2","cormo3",
                   "cormo4","vacuna","DIC")

source("Scripts/1-datos.R")
library(splines)

## -- MODELO 1: Todas las variables ------
load("Resultados/Seleccion/modelo_todas_180.RData")
intercept<-rmodelo1$sims.list$beta0
sexo<-rmodelo1$sims.list$b.sexo
v.hospital<-rmodelo1$sims.list$beta.hospi
dias<-rmodelo1$sims.list$beta.dias
nivel<-rmodelo1$sims.list$b.nivel
consultas<-rmodelo1$sims.list$b.consultas
cormo<-rmodelo1$sims.list$b.cormo
vacuna<-rmodelo1$sims.list$b.vacuna

selec[,1,1]<-c(rmodelo1$mean$beta0,rmodelo1$mean$beta.hospi,NA,
               rmodelo1$mean$beta.dias,NA,rmodelo1$mean$b.sexo,NA,
               rmodelo1$mean$b.nivel[1:5],rep(NA,5),
               rmodelo1$mean$b.consultas[1:4],
               rmodelo1$mean$b.cormo[1:3],rmodelo1$mean$b.vacuna,
               rmodelo1$DIC)

selec[,2,1]<-c(mean(intercept<0),mean(v.hospital<0),NA,
               mean(dias<0),NA,mean(sexo>0),NA,
               apply(nivel>0, 2, mean),rep(NA,5),
               apply(consultas<0, 2,mean),
               apply(cormo<0, 2, mean),mean(vacuna<0),NA)

indice<-selec[,2,1]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,1]<-1-selec[i,2,1]
  }
}

## -- ES NECESARIO SUAVIZAR?? ----##
base<-ns(x=datos1516$yoa,df=6,intercept = F) 
plot(datos1516$yoa,rmodelo1$mean$beta.edad%*%t(base))

rm(rmodelo1,intercept,sexo,v.hospital,dias,vacuna,nivel,consultas,
   cormo,indice)
dimnames(selec)[[3]][1]<-c("Mod. todas")

## -- MODELO 2: No cormo ------
load("Resultados/Seleccion/modelo_nocormo_440.RData")
intercept<-rmodelo5$sims.list$beta0
sexo<-rmodelo5$sims.list$b.sexo
v.hospital<-rmodelo5$sims.list$beta.hospi
dias<-rmodelo5$sims.list$beta.dias
nivel<-rmodelo5$sims.list$b.nivel
consultas<-rmodelo5$sims.list$b.consultas
vacuna<-rmodelo5$sims.list$b.vacuna

selec[,1,2]<-c(rmodelo5$mean$beta0,rmodelo5$mean$beta.hospi,NA,
               rmodelo5$mean$beta.dias,NA,rmodelo5$mean$b.sexo,NA,
               rmodelo5$mean$b.nivel[1:5],rep(NA,5),
               rmodelo5$mean$b.consultas[1:4],rep(NA,3),
               rmodelo5$mean$b.vacuna,rmodelo5$DIC)

selec[,2,2]<-c(mean(intercept<0),mean(v.hospital<0),NA,
               mean(dias<0),NA,mean(sexo>0),NA,
               apply(nivel>0, 2, mean),rep(NA,5),
               apply(consultas<0, 2, mean),rep(NA,3),
               mean(vacuna<0),NA)

indice<-selec[,2,2]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,2]<-1-selec[i,2,2]
  }
}


## -- ES NECESARIO SUAVIZAR?? ----##
plot(datos1516$yoa,rmodelo5$mean$beta.edad%*%t(base))

rm(rmodelo5,sexo,v.hospital,dias,vacuna,nivel,consultas,indice,intercept)
dimnames(selec)[[3]][2]<-c("Mod. sin cormo")

## -- MODELO 3: No cormo, no cons ------
load("Resultados/Seleccion/modelo_nocormo_nocons_210.RData")
intercept<-rmodelo7$sims.list$beta0
sexo<-rmodelo7$sims.list$b.sexo
v.hospital<-rmodelo7$sims.list$beta.hospi
dias<-rmodelo7$sims.list$beta.dias
nivel<-rmodelo7$sims.list$b.nivel
vacuna<-rmodelo7$sims.list$b.vacuna

selec[,1,3]<-c(rmodelo7$mean$beta0,rmodelo7$mean$beta.hospi,NA,
               rmodelo7$mean$beta.dias,NA,rmodelo7$mean$b.sexo,NA,
               rmodelo7$mean$b.nivel[1:5],rep(NA,5),rep(NA,4),
               rep(NA,3),rmodelo7$mean$b.vacuna,
               rmodelo7$DIC)

selec[,2,3]<-c(mean(intercept<0),mean(v.hospital<0),NA,
               mean(dias<0),NA,mean(sexo>0),NA,
               apply(nivel>0, 2, mean),rep(NA,5),rep(NA,4),
               rep(NA,3),mean(vacuna<0),NA)

indice<-selec[,2,3]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,3]<-1-selec[i,2,3]
  }
}

## -- ES NECESARIO SUAVIZAR?? ----##
plot(datos1516$yoa,rmodelo7$mean$beta.edad%*%t(base))

rm(rmodelo7,sexo,v.hospital,dias,vacuna,nivel,indice,intercept)
dimnames(selec)[[3]][3]<-c("Mod. sin cormo ni consu")

## -- MODELO 4: No cormo, no consu ni sexo ------
load("Resultados/Seleccion/modelo_hos_di_niv_vac_640.RData")
intercept<-rmodelo8$sims.list$beta0
v.hospital<-rmodelo8$sims.list$beta.hospi
dias<-rmodelo8$sims.list$beta.dias
nivel<-rmodelo8$sims.list$b.nivel
vacuna<-rmodelo8$sims.list$b.vacuna

selec[,1,4]<-c(rmodelo8$mean$beta0,rmodelo8$mean$beta.hospi,NA,
               rmodelo8$mean$beta.dias,NA,NA,NA,
               rmodelo8$mean$b.nivel[1:5],rep(NA,5),rep(NA,4),
               rep(NA,3),rmodelo8$mean$b.vacuna,rmodelo8$DIC)

selec[,2,4]<-c(mean(intercept<0),mean(v.hospital<0),NA,
               mean(dias<0),NA,NA,NA,apply(nivel>0, 2, mean),
               rep(NA,5),rep(NA,4),rep(NA,3),mean(vacuna<0),NA)

indice<-selec[,2,4]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,4]<-1-selec[i,2,4]
  }
}


## -- ES NECESARIO SUAVIZAR?? ----##
plot(datos1516$yoa,rmodelo8$mean$beta.edad%*%t(base))

dimnames(selec)[[3]][4]<-c("Mod. dias+hos+niv+vac")
rm(rmodelo8,v.hospital,dias,vacuna,nivel,indice,intercept)


## -- MODELO 5: No cormo, no consu + inter discretas ------
load("Resultados/Seleccion/modelo_nocormo_nocons_int_380.RData")
intercept<-rmodelo9$sims.list$beta0
sexo<-rmodelo9$sims.list$b.sexo
v.hospital<-rmodelo9$sims.list$beta.hospi
dias<-rmodelo9$sims.list$beta.dias
nivel<-rmodelo9$sims.list$b.nivel
vacuna<-rmodelo9$sims.list$b.vacuna
sex.vac<-rmodelo9$sims.list$b.sex.vac
niv.vac<-rmodelo9$sims.list$b.niv.vac

selec[,1,5]<-c(rmodelo9$mean$beta0,rmodelo9$mean$beta.hospi,NA,
               rmodelo9$mean$beta.dias,NA,rmodelo9$mean$b.sexo,
               rmodelo9$mean$b.sex.vac,rmodelo9$mean$b.nivel[1:5],
               rmodelo9$mean$b.niv.vac,rep(NA,4),rep(NA,3),
               rmodelo9$mean$b.vacuna,rmodelo9$DIC)

selec[,2,5]<-c(mean(intercept<0),mean(v.hospital<0),NA,
               mean(dias<0),NA,mean(sexo>0),mean(sex.vac<0),
               apply(nivel>0, 2, mean),apply(niv.vac<0,2,mean),
               rep(NA,4),rep(NA,3),mean(vacuna<0),NA)

indice<-selec[,2,5]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,5]<-1-selec[i,2,5]
  }
}

## -- ES NECESARIO SUAVIZAR?? ----##
plot(datos1516$yoa,rmodelo9$mean$beta.edad%*%t(base))

rm(rmodelo9,sexo,v.hospital,dias,vacuna,nivel,indice)
dimnames(selec)[[3]][5]<-c("Mod. sin cormo ni consu +sex.vac+niv.vac")


## -- MODELO 6: No cormo, no consu + vac.sex ----
load("Resultados/Seleccion/modelo_nocormo_nocons_sexvac_510.RData")
intercept<-rmodelo10$sims.list$beta0
sexo<-rmodelo10$sims.list$b.sexo
v.hospital<-rmodelo10$sims.list$beta.hospi
dias<-rmodelo10$sims.list$beta.dias
nivel<-rmodelo10$sims.list$b.nivel
vacuna<-rmodelo10$sims.list$b.vacuna
sex.vac<-rmodelo10$sims.list$b.sex.vac

selec[,1,6]<-c(rmodelo10$mean$beta0,rmodelo10$mean$beta.hospi,
                NA,rmodelo10$mean$beta.dias,NA,rmodelo10$mean$b.sexo,
                rmodelo10$mean$b.sex.vac,rmodelo10$mean$b.nivel[1:5],
                rep(NA,5),rep(NA,4),rep(NA,3),
                rmodelo10$mean$b.vacuna,rmodelo10$DIC)

selec[,2,6]<-c(mean(intercept<0),mean(v.hospital<0),NA,
                mean(dias<0),NA,mean(sexo>0),mean(sex.vac<0),
                apply(nivel>0, 2, mean),rep(NA,5),rep(NA,4),
                rep(NA,3),mean(vacuna<0),NA)

indice<-selec[,2,6]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,6]<-1-selec[i,2,6]
  }
}

## -- ES NECESARIO SUAVIZAR?? ----##
plot(datos1516$yoa,rmodelo10$mean$beta.edad%*%t(base))

rm(rmodelo10,sexo,v.hospital,dias,vacuna,nivel,indice,sex.vac)
dimnames(selec)[[3]][6]<-c("Mod. sin cormo ni consu +sex.vac")

## -- MODELO 7: No cormo, no consu + inter ------
load("Resultados/Seleccion/modelo_nocormo_nocons_int_todas_460.RData")
intercept<-rmodelo11$sims.list$beta0
sexo<-rmodelo11$sims.list$b.sexo
v.hospital<-rmodelo11$sims.list$beta.hospi
dias<-rmodelo11$sims.list$beta.dias
nivel<-rmodelo11$sims.list$b.nivel
vacuna<-rmodelo11$sims.list$b.vacuna
sex.vac<-rmodelo11$sims.list$b.sex.vac
niv.vac<-rmodelo11$sims.list$b.niv.vac
hospi.vac<-rmodelo11$sims.list$b.hospi.vac
dias.vac<-rmodelo11$sims.list$b.dias.vac

selec[,1,7]<-c(rmodelo11$mean$beta0,rmodelo11$mean$beta.hospi,
                rmodelo11$mean$b.hospi.vac,rmodelo11$mean$beta.dias,
                rmodelo11$mean$b.dias.vac,rmodelo11$mean$b.sexo,
                rmodelo11$mean$b.sex.vac,rmodelo11$mean$b.nivel[1:5],
                rmodelo11$mean$b.niv.vac,rep(NA,4),rep(NA,3),
                rmodelo11$mean$b.vacuna,rmodelo11$DIC)

selec[,2,7]<-c(mean(intercept<0),mean(v.hospital<0),
                mean(hospi.vac<0),mean(dias<0),mean(dias.vac<0),
                mean(sexo>0),mean(sex.vac<0),apply(nivel>0, 2, mean),
                apply(niv.vac<0,2,mean),rep(NA,4),rep(NA,3),
                mean(vacuna<0),NA)

indice<-selec[,2,7]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,7]<-1-selec[i,2,7]
  }
}

## -- ES NECESARIO SUAVIZAR?? ----##
plot(datos1516$yoa,rmodelo11$mean$beta.edad%*%t(base))

rm(rmodelo11,sexo,v.hospital,dias,vacuna,nivel,indice,sex.vac,
   niv.vac,hospi.vac,dias.vac)
dimnames(selec)[[3]][7]<-c("Mod. sin cormo ni consu +interacciones")



## -- MODELO 8: No cormo, no consu ni hospi vac ------
load("Resultados/Seleccion/modelo_nocormo_nocons_nohospivac_220.RData")
intercept<-rmodelo12$sims.list$beta0
sexo<-rmodelo12$sims.list$b.sexo
v.hospital<-rmodelo12$sims.list$beta.hospi
dias<-rmodelo12$sims.list$beta.dias
nivel<-rmodelo12$sims.list$b.nivel
vacuna<-rmodelo12$sims.list$b.vacuna
sex.vac<-rmodelo12$sims.list$b.sex.vac
niv.vac<-rmodelo12$sims.list$b.niv.vac
dias.vac<-rmodelo12$sims.list$b.dias.vac

selec[,1,8]<-c(rmodelo12$mean$beta0,rmodelo12$mean$beta.hospi,NA,
                rmodelo12$mean$beta.dias,rmodelo12$mean$b.dias.vac,
                rmodelo12$mean$b.sexo,rmodelo12$mean$b.sex.vac,
                rmodelo12$mean$b.nivel[1:5],rmodelo12$mean$b.niv.vac,
                rep(NA,4),rep(NA,3),rmodelo12$mean$b.vacuna,rmodelo12$DIC)

selec[,2,8]<-c(mean(intercept<0),mean(v.hospital<0),
                NA,mean(dias<0),mean(dias.vac<0),
                mean(sexo>0),mean(sex.vac<0),apply(nivel>0, 2, mean),
                apply(niv.vac<0,2,mean),rep(NA,4),rep(NA,3),
                mean(vacuna<0),NA)

indice<-selec[,2,8]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,8]<-1-selec[i,2,8]
  }
}

## -- ES NECESARIO SUAVIZAR?? ----##
plot(datos1516$yoa,rmodelo12$mean$beta.edad%*%t(base))

rm(rmodelo12,sexo,v.hospital,dias,vacuna,nivel,indice,
   sex.vac,niv.vac,hospi.vac,dias.vac)
dimnames(selec)[[3]][8]<-c("Mod. sin cormo ni consu ni hospi:vacuna")



## -- MODELO 9: No cormo, no consu + dias:vacuna + sexo:vacuna------
load("Resultados/Seleccion/modelo_nocormo_nocons_sexvac_diasvac_480.RData")
intercept<-rmodelo13$sims.list$beta0
sexo<-rmodelo13$sims.list$b.sexo
v.hospital<-rmodelo13$sims.list$beta.hospi
dias<-rmodelo13$sims.list$beta.dias
nivel<-rmodelo13$sims.list$b.nivel
vacuna<-rmodelo13$sims.list$b.vacuna
sex.vac<-rmodelo13$sims.list$b.sex.vac
dias.vac<-rmodelo13$sims.list$b.dias.vac

selec[,1,9]<-c(rmodelo13$mean$beta0,rmodelo13$mean$beta.hospi,NA,
                rmodelo13$mean$beta.dias,rmodelo13$mean$b.dias.vac,
                rmodelo13$mean$b.sexo,rmodelo13$mean$b.sex.vac,
                rmodelo13$mean$b.nivel[1:5],rep(NA,5),
                rep(NA,4),rep(NA,3),rmodelo13$mean$b.vacuna,rmodelo13$DIC)

selec[,2,9]<-c(mean(intercept<0),mean(v.hospital<0),
                NA,mean(dias<0),mean(dias.vac<0),
                mean(sexo>0),mean(sex.vac<0),apply(nivel>0, 2, mean),
                rep(NA,5),rep(NA,4),rep(NA,3),
                mean(vacuna<0),NA)

indice<-selec[,2,9]
indice[is.na(indice)]<-11

for (i in 1:nrow(selec)) {
  if (indice[i]<0.5){
    selec[i,2,9]<-1-selec[i,2,9]
  }
}

## -- ES NECESARIO SUAVIZAR?? ----##
plot(datos1516$yoa,rmodelo13$mean$beta.edad%*%t(base))

rm(rmodelo13,sexo,v.hospital,dias,vacuna,nivel,
   indice,sex.vac,niv.vac,hospi.vac,dias.vac)
dimnames(selec)[[3]][9]<-c("Mod. sin cormo ni consu + dias:vac + sex:vac")





print("MEJOR MODELO: GRIPE~HOSPI+DIAS+SEXO+NIVEL+VACUNA+s(EDAD)")

## COMPROBACION DE LOS DF DEL SUAVIZADO ------
load("Resultados/Seleccion/modelo_nocormo_nocons7_460.RData")
load("Resultados/Seleccion/modelo_nocormo_nocons3_390.RData")
load("Resultados/Seleccion/modelo_hos_di_niv_vac3_530.RData")
load("Resultados/Seleccion/modelo_hos_di_niv_vac7_670.RData")
load("Resultados/Seleccion/modelo_nocormo_nocons_sexvac7_420.RData")
load("Resultados/Seleccion/modelo_nocormo_nocons_sexvac3_260.RData")


c(rmodelo7$DIC,rmodelo7k7$DIC,rmodelo7k3$DIC) ## k=5 
c(rmodelo8$DIC,rmodelo8k7$DIC,rmodelo8k3$DIC) ## k=5 
c(rmodelo10$DIC,rmodelo10k7$DIC,rmodelo10k3$DIC) ## k=5

## Comparamos los suavizados de manera grafica (modelo 3):
base1<-ns(x=datos1516$yoa,df=8,intercept = F) 
plot(datos1516$yoa,rmodelo7k7$mean$beta.edad%*%t(base1),
     main="Distintos suavizados edad modelo 3")
load("Resultados/Seleccion/modelo_nocormo_nocons_210.RData")
base2<-ns(x=datos1516$yoa,df=6,intercept = F) 
points(datos1516$yoa,rmodelo7$mean$beta.edad%*%t(base2),col="red")
base3<-ns(x=datos1516$yoa,df=4,intercept=F)
points(x=datos1516$yoa,rmodelo7k3$mean$beta.edad%*%t(base3),col="green")

## Modelo 4:
plot(datos1516$yoa,rmodelo8k7$mean$beta.edad%*%t(base1),
     main="Distintos suavizados edad modelo 4")
load("Resultados/Seleccion/modelo_hos_di_niv_vac_640.RData")
points(datos1516$yoa,rmodelo8$mean$beta.edad%*%t(base2),col="red")
points(x=datos1516$yoa,rmodelo8k3$mean$beta.edad%*%t(base3),col="green")

## Modelo 8:
plot(datos1516$yoa,rmodelo10k7$mean$beta.edad%*%t(base1),
     main="Distintos suavizados edad modelo 8")
load("Resultados/Seleccion/modelo_nocormo_nocons_sexvac_510.RData")
points(datos1516$yoa,rmodelo10$mean$beta.edad%*%t(base2),col="red")
points(x=datos1516$yoa,rmodelo10k3$mean$beta.edad%*%t(base3),col="green")
