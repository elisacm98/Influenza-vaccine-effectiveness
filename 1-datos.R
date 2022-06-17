library(haven)
library(lubridate)
library(gtsummary)
datos<-read_dta("Datos/Datos_Elisa_1516_1617.dta") 

## Guardamos la edad en un único vector:
datos$yoa<-replace(datos$yoa,datos$yoa==9999,
                   interval(datos$dob[datos$moa<9999],
                            datos$dohosp[datos$moa<9999])
                   %/% months(1) / 12)

datos$sex<-datos$female ## Sexo en distintas variables

datos1<-replace(datos,datos==9999,NA)  ## Modificamos todos
datos1<-replace(datos1,datos==99, NA)  ## No Aplica  a NA
datos1$yoa[is.na(datos1$yoa)]<-99   ### Hay sujetos con 99 años

## ----------- SELECCIONAMOS SUJETOS + TEMPORADA -------------------
sum(datos1$ico==0,na.rm=T)  ## CONSETIMIENTO
sum(is.na(datos1$ico))  ## Hay sujetos que no consienten y con NA

indice<-datos1$ico
indice[is.na(indice)]<-11
datos1<-datos1[indice==1,] ### Escogemos sólo los que consienten

sum(datos1$ccres==0,na.rm = TRUE) ## CRITERIOS RESIDENCIA
sum(is.na(datos1$ccres))          ## No hay NA
datos1<-datos1[datos1$ccres!=0,]

sum(datos1$c5cri==0,na.rm = TRUE) ### ¿CUMPLEN CRITERIOS?
sum(is.na(datos1$c5cri))          ## Hay NAs
indice<-datos1$c5cri
indice[is.na(indice)]<-11
datos1<-datos1[indice==1,] ## Cogemos sólo los que cumplen criterios

sum(datos1$ccss==0,na.rm = TRUE) ## ¿CRITERIOS CASO PROBABLE?
indice<-datos1$ccss
sum(is.na(indice))               ## Hay NAs (menores de 5 años)
indice[is.na(indice)]<-11
datos1<-datos1[indice!=0,]  

sum(datos1$cclt5==0,na.rm = TRUE) ## ¿Menos de a ños CUMPLEN CRITERIOS?
indice<-datos1$cclt5
sum(is.na(indice)) 
indice[is.na(indice)]<-11
datos1<-datos1[indice!=0,]

sum(is.na(datos1$pcr)) ## Tenemos individuos con resultado indeterminado
indice<-datos1$pcr
indice[is.na(indice)]<-11
datos1<-datos1[indice!=11,]  ## Eliminamos resultados indeterminados

datos<-datos1; rm(indice)

### ---------- TRANSFORMAMOS VARIABLES DEL ESTUDIO ---------------
datos$sex<-as.factor(datos$sex)  ## variable SEXO
datos$r2015sfv<-as.factor(datos$r2015sfv) ## variable VACUNACION
datos$hospital<-as.factor(datos$hospital) ## variable HOSPITAL
datos$soc<-as.factor(datos$soc)           ## NIVEL SOCIOECONOMICO
datos$gp<-as.factor(datos$gp)             ## VISITAS ATENCION PRIMARIA

for(i in 1:length(datos$risk)){
  if(datos$risk[i]==0){datos$riskR[i]=0}
  if(datos$risk[i]==1){datos$riskR[i]=1}
  if(datos$risk[i]==2){datos$riskR[i]=2}
  if(datos$risk[i]>=3){datos$riskR[i]=3}
}
rm(i)

datos$riskR<-as.factor(datos$riskR) ##Var. NUM. COMORBILIDADES

datos$epi_week<-as.factor(datos$epi_week) ##SEMANA EPIDEMIOLOGICA

## ----- TEMPORADA 15/16 --------
datos1516<-datos[datos$season=="15/16",]
## Corregimos datos erroneos en vacunacion:
sum(is.na(datos1516$r2015sfv)) ## Los NA corresponden a NO vacunados
datos1516$r2015sfv<-replace(datos1516$r2015sfv,is.na(datos1516$r2015sfv),0)

## MESES ENTRE VACUNA Y SINTOMAS:
datos1516[,ncol(datos1516)+1]<-interval(datos1516$do2015sfv,
                                        datos1516$doini)%/% days(1)
colnames(datos1516)[ncol(datos1516)]<-c("daysvac") 
datos1516$daysvac<-datos1516$daysvac/30

## Renombramos la variable DIAS ENTRE SINTOMAS Y MUESTRA:
colnames(datos1516)[colnames(datos1516)=="lablag"]<-c("dayssin") 

rm(datos,datos1)
