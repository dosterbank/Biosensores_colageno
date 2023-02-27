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


#Rt20
#Aun no se claramente porque se elige este ratio. la profe elije 10
Data_Rt10 = subset(Data, Rt=="20")
m1_Rt10 <- aov(A450 ~ t, data=Data_Rt10)
anova(m1_Rt10)
boxplot(A450 ~ t, data=Data_Rt10)
########################### fin profe ##########################################



############# Modelo anova todos los datos de Ab at 405nm ######################
modelo_anova <- anova(aov(A_405 ~ Rt, data=Data))
modelo_anova
#otros modelos para el tukey
res_aov_1 <- aov(A_405 ~ Rt, data=Data)
res_aov_2 <- aov(A_405 ~ Csour, data=Data)

##################################### Tukey ####################################

# Tukey compara por pares y entrega los P values
# se pueden ver cuales son iguales cuango son mayores a 0.05
# es decir que los valores cercanos a 1 dicen que los experimentos 
# son muy parecidos en resultados
TukeyHSD(res_aov_2, which = "Csour")#tukey de todos los Csour
TukeyHSD(res_aov_1, which = "Rt")
boxplot(A_405 ~ Csour, data=Data)
boxplot(A_405 ~ Csour*Rt, data=Data)
boxplot(A_405 ~ Rt, data=Data)

################################### Subset #####################################

#Tipo de extracto. En mi caso es Fs. asumiendo que es el mejor
# mejor en terminos de estabilidad y facil extraccion. 
# como no hicimos tiempos largos, solo de 40 min. es posible que no se 
# observe la estabilidad en el tiempo de las nanoparticulas(grafico lineal pendiente 0)
# inestabilidad (lineal creciente)

Data_Fs = subset(Data, Csour=="FS") #cambiar Fs por el Csour deseado
res_aov_Fs <- aov(A_405 ~ Rt, data=Data_Fs)
anova(res_aov_Fs) #ANOVA SOLO del subset Fs

TukeyHSD(res_aov_Fs, which = "Rt") #tukey solo de los ratios de Fs
boxplot(A_405 ~ t+Rt, data=Data_Fs)
################################ Conclusiones ##################################
#De la prueba de anova general se observa valor menor a .05. Por lo tanto, 
#hay diferencias en las medias de los grupos de datos considerando como referencia
#a la absorbancia de 405nm y como factores solo al Ratio

#De la prueba de Tukey se observa que hay diferencias entre pares de datos tanto
#para Rt como para Csour. tambien hay algunos pares de datos que son iguales tanto
#para Rt como para Csour.

#Del subset en mi caso FS, se puede observar que existen diferencias cuando se hace el anova
# considerando  como referencia a la absorbancia de 405nm y como factores solo al Ratio

# De igual manera se pueden ver diferencias y similitudes en tukey solo de Fs 
#consideranco como factor el Ratio unicamente. 

# De lo antes mencionado se puede elegir un ratio que sea similar y que sea facil de replicar


############## box mejorado con separaciones cada 10 min ################
# se asigna los valores de Data a vectores del mismo nombre
A_405 <- Data$A_405
Rt <- Data$Rt
t <- Data$t


# Define the data frame
Data_Fs <- data.frame(
  A_405,
  t,
  Rt 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (t), y = A_405, fill = (Rt))) +
  ggtitle("Fish scales with N3 treatment boxplot") +
  geom_boxplot() +
  scale_fill_manual(values = c("gray70", "blue","gray50","gray50","gray50","gray50","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5,3.5,4.5), linetype = "dotted", color = "black") +
  theme_classic()

####################### Elegir mejor Ratio de Fs ##########
#"80-20"  0.008000 -0.048195842 0.064195842 0.9998656
# Segun mi eleccion el mejor ratio es 20, ya que es muy similar al ratio de 80
# y estan separados entre si. 
Data_Rt_20 = subset(Data, Rt=="20") #cambiar Rt por el Rt deseado
res_aov_Rt_20 <- aov(A_405 ~ t, data=Data_Rt_20)
anova(res_aov_Rt_20) #ANOVA SOLO del subset Rt= 20 como factor el tiempo


# en teoria cuando el P value es mayor a 0.05 se dice que ya no hay cambios en la abs en funcion del tiempo para el Rt 20
# Por lo tanto la prueba de tuley seria innecessaria.
# en mi caso es 0.08 por lo que no hay cambios y no se deberia hacer tukey a mi criterio

TukeyHSD(res_aov_Rt_20, which = "t") #tukey solo de los ratios de 20 en funcion del tiempo




############## box mejorado Ratio=20 con separaciones cada 10 min ################
# se asigna los valores de Data a vectores del mismo nombre
A_405_20 <- Data_Rt_20$A_405
Rt20 <- Data_Rt_20$Rt
t20 <- Data_Rt_20$t


# Define the data frame
Data_Fs <- data.frame(
  A_405_20,
  t20,
  Rt20 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (t20), y = A_405_20, fill = (Rt20))) +
  ggtitle("Fish scales with N3 treatment Only Ratio=20 and time 10min to 40 min") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","gray50","gray50","gray50","gray50","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5,3.5,4.5), linetype = "dotted", color = "black") +
  theme_classic()

