
#########################################################################################
#########################################################################################
################################## MODELO DE FUGA #######################################
#########################################################################################
#########################################################################################
{
library(data.table)
library(caTools)
library(bit64)

setwd("C:/Users/FelipeHernandez/Desktop/Predictable Media/Plan Vital - Fuga/Ultimos modelos")

##################################################################################
#################### Llamado de BBDD y entrenamiento y prueba ####################
##################################################################################

fuga=fread("churn.csv", sep=";", header = T)
names(fuga)
colnames(fuga)[14]<-"fugado"
colnames(fuga)[8]<-"Antiguedad"
View(fuga)


fuga$fugado<-factor(fuga$fugado, levels = c(0,1)) #cambia la variable a factor (dicotomica)
library(ggplot2)
bp <- ggplot(fuga, aes(fugado, tipo_fondo, fill = fugado)) + scale_y_continuous(name = "Tipo Fondo",breaks = seq(0, 5, 1),limits=c(0, 5)) + scale_x_discrete(name = "0=No Fugado/1=Fugado") + ggtitle("Nivel de fuga por Tipo de Fondo")
bp + geom_boxplot() + stat_boxplot(geom ='errorbar') 
library(plotly)
plot_ly(fuga, x = ~ genero, color = ~fugado, type = "box",colors = c("#4271AE", "#1F3552"))

#### ORDENO LA BASE DEJO FUGA PRIMERO
fuga=fuga[, c(14,1:13,15,16)]
fuga$region<-as.factor(fuga$region)
fuga$usa_web<-as.factor(fuga$usa_web)
fuga$visit_web<-as.factor(ifelse(fuga$visit_web>0,1,0))
fuga$visit_web<-as.factor(fuga$visit_web)
fuga$visit_sucursal<-as.factor(fuga$visit_sucursal)
fuga$usa_app <-as.factor(fuga$usa_app)
fuga$empleador_paga<-as.factor(fuga$empleador_paga)
fuga$genero<-as.factor(fuga$genero)
fuga$tipo_reclamo<-as.factor(fuga$tipo_reclamo)
fuga$producto_CAV<-as.factor(fuga$producto_CAV)
fuga$producto_CCV<-as.factor(fuga$producto_CCV)

fuga$visit_web<-NULL
#base completa con id persona
base_completa<-fuga

#base para entrenar el modelo sin id persona
fuga$id_persona<-NULL #mientras entreno el modelo no necesito el id despues cargo denuevo la dta

set.seed(69)
split = sample.split(fuga$fugado, SplitRatio = 0.75) #divide la data 75% training
training_set = subset(fuga, split == TRUE)
test_set = subset(fuga, split == FALSE)

################### Arbol de desición  
sapply(fuga, function(x) sum(is.na(x))) #Valores faltantes

######### UNBALALANCED SAMPLE - Cuando la data esta desbalanceada ahi te envio un paper
library(ROSE)
library(caret)
#### ROSE proporcionan una mejor estimación de los datos originales- muestreo sintético
data.rose <- ROSE(fugado ~ ., data = training_set, seed = 1)$data
table(data.rose$fugado); prop.table(table(data.rose$fugado))
training_set=data.rose
rm(data.rose)


##################################################################################
############################# REGRESIÓN LOGÍSTICA ################################
##################################################################################

glm_logit = glm(formula = fugado ~. ,
                family=binomial(link="logit"), data = training_set)
summary(glm_logit)
library(pscl)
pR2(glm_logit) #pseudo r cuadrado
library(car)
vif(glm_logit) #multicolinealidad
require(MASS)
#exp(cbind(coef(glm_logit), confint(glm_logit)))
options(width=80)
exp(coef(glm_logit)) # odds
#chisq.test(training_set$FORMA_PAGO_CO,training_set$FUGADO)
prob_pred = predict(glm_logit, type = 'response', newdata = test_set[,-1])
y_pred = ifelse(prob_pred > 0.5 , 1, 0)
library(e1071)
c_glm <- confusionMatrix(factor(y_pred),test_set$fugado, positive = "1")      #  matriz de confusion
cat(" Accuracy de test : ",c_glm[[3]][1],"\n") 
print(c_glm)  

library(InformationValue)
plotROC(test_set$fugado, prob_pred)
detach("package:InformationValue", unload=TRUE)


####### PREDECIR TODA LA DATA - # sin incluir la variable fuga ni el id de la persona
######################################################################################
predecir_base = predict(glm_logit, type = 'response', newdata = base_completa[,c(-1,-2)])
###asignar pbb_fuga a toda la base

base_completa=cbind(base_completa,predecir_base)

}

dim(base_completa)
View(base_completa)
#ordenar base
base_completa=base_completa[, c(2,1,16,3,4,5,6,7,8,9,10,11,12,13,14,15)]
colnames(base_completa)[3]<-"pbb_fuga"

dim(base_completa)

base_completa <- subset(base_completa, fugado == 0)

dim(base_completa)
View(base_completa)



write.table(base_completa, "Plan_vital_pbb_fuga.csv", row.names = F, sep=";")


split = sample.split(base_completa$pbb_fuga, SplitRatio = 0.68235) #muestra que deja la base en 1000 customers
p3 = subset(base_completa, split == TRUE)
dim(p3)

write.table(p3, "Plan_vital_pbb_fuga_sample.csv", row.names = F, sep=";")
