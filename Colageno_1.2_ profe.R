# Este codigo contempla mas de 2 variables en Csour
# si los P values son menores a 0.05. se dice que hay diferencias
# en el caso de anova en las medias de los datos.
# source("C:/Users/bjxtr/Desktop/biosensores code/Colageno.R")

################################ Librerias #####################################
library(ggplot2)

install.packages("statsr")
library(statsr)

####################### carga el archivo por ventana en examinar ###############
#tiene que estar en formato csv

Data <- read.csv(file.choose(), header = T, sep=";")
#elegir "Sin_Mai_ni_ Isaac.csv" para ver mejor los boxplots
#se pueden cargar cualquier datos csv

########################## Factoriza los datos ################################@
Data$t <- as.factor(Data$t)
Data$Csour <- as.factor(Data$Csour)
Data$Rt <- as.factor(Data$Rt)
######################### Profe ################################################
m1 <- aov(A450 ~ Rt, data=Data) # se consideran todos los Csour
summary(m1)
TukeyHSD(m1)
boxplot(A450 ~ Rt, data=Data)


#Rt70
#Se elije porque tiene el unico valor por debajo de 0.05. Ademas se elige
# 70 por el hecho que es mas diferente a los comparados con 20 vs otros ratios
Data_Rt70 = subset(Data, Rt=="70")
m1_Rt70 <- aov(A450 ~ t, data=Data_Rt70)
anova(m1_Rt70)
boxplot(A450 ~ t, data=Data_Rt70)# se plotean los datos solo para el ratio 70
########################### fin profe ##########################################


#mejores resultados 
#$$$$$$$$$$$$$$$$$$$$$ Solo de FS #######################################
Data_FS = subset(Data, Csour=="FS")
m2 <- aov(A450 ~ Rt, data=Data_FS) # se consideran solos los datos para FS
summary(m2)
TukeyHSD(m2)
boxplot(A450 ~ Rt, data=Data_FS)

#Rt30
#Se elije porque tiene un valor por debajo de 0.05. Ademas se elige
# 30 por el hecho que es mas diferente a los comparados vs otros ratios


# Revisar. Se supone que solo se eligen los datos para Rt=30 y Csour=FS
Data_selected_ratio = subset(Data_FS, Rt=="30")
m1_selected_ratio <- aov(A450 ~ t, data=Data_selected_ratio)
summary(m1_selected_ratio)
anova(m1_selected_ratio)
# porque en este anova me faltan datos para que se ploteen con varianza

boxplot(A450 ~ t, data=Data_selected_ratio)# se plotean los datos solo para el selected_ratio
