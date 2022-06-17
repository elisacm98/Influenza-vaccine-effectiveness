### LIBRERIAS + DATOS:
library(ggplot2)
library(RColorBrewer)
library(gtsummary)

datos<-read_dta("Datos/Datos_Elisa_1516_1617.dta") 

datos1516_completo<-datos[datos$season=="15/16",]; rm(datos)

#### ------------ GRAFICA INCLUSION-EXCLUSION ------------------
frecs<-c(sum(table(datos1516_completo$flu)),
         table(datos1516_completo$ccres)[1]+
           table(datos1516_completo$resi6)[1],
         table(datos1516_completo$ccss)[1],
         table(datos1516_completo$cclt5)[1],
         table(datos1516_completo$ico)[1],
         table(datos1516_completo$inst)[2],
         table(datos1516_completo$prevhosp)[2])

prop<-(frecs/nrow(datos1516_completo))*100

png("Graficas_latex/Inclusion-exclusion/datos_excluidos.png",
    width = 604,
    height = 604)
opar<-par(no.readonly = TRUE)
par(mar=c(5,5,4,6))
barplot(prop,
        names.arg=rep('',length(frecs)),
        main=" ",
        col=c(brewer.pal(7,"Set2")),
        ylab="Frecuencia relativa",
        ylim=c(0,40))

texto<-c("Individuos con muestra","No residen",
         "Sin síntomas (>5años)","Sin síntomas (<5años)",
         "No consienten", "Institucionalizado",
         "Ingresados en el último mes")

legend(x = "topright",inset = c(-0.1, 0),legend= texto,
       pch=rep(19,length(texto)),
       col=c(brewer.pal(7,"Set2")),
       xpd=TRUE,
       cex=1.4)
on.exit(par(opar))
dev.off()

#### ------------ ETIQUETAS VARIABLES ------------------
source("Scripts/1-datos.R")

levels(datos1516$sex)<-c("Hombres","Mujeres")

levels(datos1516$r2015sfv)<-c("No","Sí")

datos1516$flu<-as.factor(datos1516$flu)
levels(datos1516$flu)<-c("No","Sí") 

levels(datos1516$soc)<-c("Técnicos superiores", 
                         "Técnicos medios", 
                         "Administrativos",
                         "Trab. man. cualificados", 
                         "Trab. man. semicualificados",
                         "Trabajadores no cualificados")
levels(datos1516$hospital)<-c("H. G. Castellón", "La Fe", 
                              " Dr. Peset", "H. G. Alicante")

levels(datos1516$riskR)<-c("Nignuna", "Una", "Dos", "Más de dos")

levels(datos1516$gp)<-c("Nignuna", "Una", "Dos", "Tres", "Más de tres")

#### ------------ DESCRIPTIVA GRÁFICA COVARIABLES ---------------------
ggplot(data=datos1516, aes(sex,fill=sex)) + 
  geom_bar() + ylab("") +
  xlab("Sexo") + labs(fill= 'Sexo')    

ggplot(data=datos1516, aes(soc,fill=soc)) + 
  geom_bar() + ylab("") +
  xlab("Nivel socioeconómico") + labs(fill= 'Nivel socioeconómico') 

ggplot(data=datos1516, aes(riskR,fill=riskR)) + 
  geom_bar() + ylab("") +
  xlab("Número de comorbilidades") + labs(fill= 'Número de comorbilidades') 

ggplot(data=datos1516, aes(gp,fill=gp)) + 
  geom_bar() + ylab("") +
  xlab("Número de visitas a atención primaria") + 
  labs(fill= 'Número de visitas a atención primaria') 

ggplot(data=datos1516, aes(r2015sfv,fill=r2015sfv)) + 
  geom_bar() + ylab("") +
  xlab("Vacunación") + labs(fill= 'Vacunación') 

ggplot(data=datos1516, aes(flu,fill=flu)) + 
  geom_bar() + ylab("") +
  xlab("Gripe") + labs(fill= 'Gripe') 

ggplot(data=datos1516, aes(hospital,fill=hospital)) + 
  geom_bar() + ylab("") +
  xlab("Hospital") + labs(fill= 'Hospital')  

ggplot(data =datos1516, aes(y=yoa)) + 
  geom_boxplot(fill=4) + ylab("Edad") 

ggplot(data =datos1516, aes(y=dayssin)) + 
  geom_boxplot(fill=4) + ylab("Días con síntomas") 

ggplot(data =datos1516, aes(y=hhm)) + 
  geom_boxplot(fill=4) + ylab("Hospitalizaciones") 

ggplot(data =datos1516, aes(y=daysvac)) + 
  geom_boxplot(fill=4) + ylab("Meses vacunado")

#### ------------ GRÁFICA COMORBILIDADES ----------------------------
comor<-data.frame(num=c(sum(datos1516$acvd),
                        sum(datos1516$acevd),
                        sum(datos1516$aperifarthero),
                        sum(datos1516$asthma),
                        sum(datos1516$abronchitis),
                        sum(datos1516$adiab),
                        sum(datos1516$aendroc),
                        sum(datos1516$anemia),
                        sum(datos1516$ahepdis),
                        sum(datos1516$rf),
                        sum(datos1516$aimmun),
                        sum(datos1516$anmd),
                        sum(datos1516$aneo),
                        sum(datos1516$autoin),
                        sum(datos1516$adement)),
                  etiquetas=c("Enfermedad cardíaca", 
                              "Enfermedad cerebrovascular",
                              "Arteriopatía periférica","Asma",
                              "Bronquitis o EPOC","Diabetes",
                              "Enfermedad sistema endocrino distinta a diabetes",
                              "Anemia",
                              "Enfermedad hepática crónica",
                              "Alteración función renal", 
                              " Inmunodeficiencias",
                              "Enfermedad neuromuscular",
                              "Enfermedad neoplásica", 
                              "Enfermedad crónica autoinmune",
                              "Demencia senil"))

opar<-par(no.readonly = TRUE)
par(mar=c(5,5,4,11))
barplot(comor$num/nrow(datos1516)*100,
        names.arg=rep('',length(comor$num)),
        main=" ",
        col=c(brewer.pal(7,"Set2"),brewer.pal(8,"Set1")),
        ylab="Frecuencia relativa",
        ylim=c(0,40))

legend(x = "topright",inset = c(-0.7, 0),legend= comor$etiquetas,
       pch=rep(19,length(comor$etiquetas)),
       col=c(brewer.pal(7,"Set2"),brewer.pal(8,"Set1")),
       xpd=TRUE,
       cex=0.9)
on.exit(par(opar))

#### ------------ DESCRIPTIVA NUMÉRICA COVARIABLES --------------------
t.predictorg <- datos1516 %>%
  select(r2015sfv,sex,yoa,dayssin,hhm,gp,soc,riskR,flu,daysvac) %>%
  tbl_summary(label = list(r2015sfv~ "Vacunados",
                           sex~"Sexo",
                           yoa~"Edad",
                           dayssin~"Días con síntomas",
                           daysvac~"Meses vacunado",
                           hhm~"Hospitalizaciones",
                           gp~"Visitas atención primaria",
                           soc~"Nivel socioeonómico",
                           riskR~"Cormobilidades"),
              type=all_continuous()~"continuous2",
              statistic = list(all_continuous()~c("{median} ({p25}, {p75})",
                                                  "{min}","{max}", 
                                                  "{mean} ({sd})")))

#### ------------ DESCRIPTIVA GRÁFICA VACUNACIÓN ------------------------
ggplot(data=datos1516, aes(x=sex,fill=r2015sfv)) + 
  geom_bar(position="dodge") + ylab("") + 
  xlab("Sexo") + labs(fill= 'Vacunación') 

ggplot(data=datos1516, aes(x=soc,fill=r2015sfv)) +  
  geom_bar(position="dodge") + ylab("") +         
  xlab("Nivel socioeconómico") + labs(fill= 'Vacunación')

ggplot(data=datos1516, aes(x=hospital,fill=r2015sfv)) +
  geom_bar(position="dodge") + ylab("") +           
  xlab("Hospital") + labs(fill= 'Vacunación') 

ggplot(data=datos1516, aes(x=riskR,fill=r2015sfv)) +    
  geom_bar(position="dodge") + ylab("") +
  xlab("Número de comorbilidades") + labs(fill= 'Vacunación') 

ggplot(data=datos1516, aes(x=riskR,fill=r2015sfv)) +
  geom_bar(position="dodge") + ylab("") +      
  xlab("Número de comorbilidades") + labs(fill= 'Vacunación') 

ggplot(data =datos1516, aes(y=yoa,fill=r2015sfv)) +
  geom_boxplot() + ylab("Edad") +              
  labs(fill= 'Vacunación')

ggplot(data =datos1516, aes(y=dayssin,fill=r2015sfv)) + 
  geom_boxplot() + ylab("Días con síntomas") +        
  labs(fill= 'Vacunación')

ggplot(data =datos1516, aes(y=hhm,fill=r2015sfv)) +  
  geom_boxplot() + ylab("Hospitalizaciones") +       
  labs(fill= 'Vacunación')

#### ------------ DESCRIPTIVA NUMÉRICA VACUNACIÓN -------------------
t.predictorvacu <- datos1516 %>%
  select(r2015sfv,sex,yoa,dayssin,hhm,gp,soc,riskR,hospital) %>%
  tbl_summary(label = list(sex~"Sexo",
                           yoa~"Edad",
                           dayssin~"Días con síntomas",
                           hhm~"Hospitalizaciones",
                           gp~"Visitas atención primaria",
                           soc~"Nivel socioeonómico",
                           riskR~"Cormobilidades",
                           hospital~"Hospital"),
              by=r2015sfv,
              type=all_continuous()~"continuous2",
              statistic = list(all_continuous()~c("{median} ({p25}, {p75})",
                                                  "{min}","{max}", 
                                                  "{mean} ({sd})")))
#### ------------ DESCRIPTIVA GRÁFICA GRIPE ---------------------
ggplot(data=datos1516, aes(x=sex,fill=flu)) + 
  geom_bar(position="dodge") + ylab("") + 
  xlab("Sexo") + labs(fill= 'Gripe')  

ggplot(data=datos1516, aes(r2015sfv,fill=flu)) +  
  geom_bar(position="dodge") + ylab("") +       
  xlab("Vacunación") + labs(fill= 'Gripe')

ggplot(data=datos1516, aes(x=soc,fill=flu)) +     
  geom_bar(position="dodge") + ylab("") +             
  xlab("Nivel socioeconómico") + labs(fill= 'Gripe')

ggplot(data=datos1516, aes(x=hospital,fill=flu)) +    
  geom_bar(position="dodge") + ylab("") +             
  xlab("Hospital") + labs(fill= 'Gripe') 

ggplot(data=datos1516, aes(x=riskR,fill=flu)) +    
  geom_bar(position="dodge") + ylab("") +         
  xlab("Número de comorbilidades") + labs(fill= 'Gripe')

ggplot(data=datos1516, aes(x=gp,fill=flu)) +  
  geom_bar(position="dodge") + ylab("") +       
  xlab("Número de visitas a atención primaria") + labs(fill= 'Gripe')  

ggplot(data = datos1516, aes(y=yoa,fill=flu)) +   
  geom_boxplot() +  ylab("Edad") +              
  labs(fill = "Gripe")

ggplot(data = datos1516, aes(y=dayssin,fill=flu)) +       
  geom_boxplot() +  ylab("Días con síntomas") +         
  labs(fill = "Gripe")

ggplot(data = datos1516, aes(y=daysvac,fill=flu)) +       
  geom_boxplot() +  ylab("Meses vacunado") +         
  labs(fill = "Gripe")

ggplot(data = datos1516, aes(y=hhm,fill=flu)) +   
  geom_boxplot() +  ylab("Hospitalizaciones") +       
  labs(fill = "Gripe")
#### ------------ DESCRIPTIVA NUMÉRICA GRIPE ------------------------
t.predictorflu <- datos1516 %>%
  select(r2015sfv,sex,yoa,dayssin,hhm,gp,soc,riskR,hospital,flu,daysvac) %>%
  tbl_summary(label = list(r2015sfv~ "Vacunados",
                           sex~"Sexo",
                           yoa~"Edad",
                           dayssin~"Días con síntomas",
                           daysvac~"Meses vacunado",
                           hhm~"Hospitalizaciones",
                           gp~"Visitas atención primaria",
                           soc~"Nivel socioeonómico",
                           riskR~"Cormobilidades",
                           hospital~"Hospital"),
              by=flu,
              type=all_continuous()~"continuous2",
              statistic = list(all_continuous()~c("{median} ({p25}, {p75})",
                                                  "{min}","{max}", 
                                                  "{mean} ({sd})")))
#### ------------ SEMANA EPI + COLINEALIDAD
datos1516$semanas <-factor(
  datos1516$epi_week, 
  levels = levels(datos1516$epi_week)[c(18:26,1:17)]) 

## Representamos los casos y loS controles cada semana:
ggplot(data=datos1516, aes(x=semanas,fill=flu)) +     
  geom_bar(position="dodge") + ylab("") +             
  xlab("Semana epidemiológica") + labs(fill= 'Gripe')

covariables<-datos1516 %>%
  select(flu,sex,yoa,hhm,dayssin,gp,soc,riskR,r2015sfv,hospital)
covariables<-data.matrix(covariables)
colnames(covariables)<-c("gripe","sexo","edad","num. hospi",
                         "días","visitas","n.socio",
                         "comor","vacuna","hospital")

## Matriz sin meses vacunado:
matcor<-cor(covariables,use='complete.obs')
corrplot(matcor, method = "ellipse", type = "lower", 
         diag = T,tl.cex=0.7,tl.col=1)
corrplot(matcor, method = "number", type = "upper", 
         diag = FALSE,add = T,tl.cex=0.7,tl.col=1)

covariables<-datos1516 %>%
  select(flu,sex,yoa,hhm,dayssin,gp,soc,riskR,r2015sfv,hospital,daysvac)
matcor<-cor(covariables,use='complete.obs')

## Matriz con meses vacunado:
corrplot(matcor, method = "ellipse", type = "lower", 
         diag = T,tl.cex=0.7,tl.col=1)
corrplot(matcor, method = "number", type = "upper", 
         diag = FALSE,add = T,tl.cex=0.7,tl.col=1)
rm(covariables)