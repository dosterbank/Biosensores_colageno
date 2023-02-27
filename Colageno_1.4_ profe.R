# Este codigo contempla mas de 2 variables en Csour
# si los P values son menores a 0.05. se dice que hay diferencias
# en el caso de anova en las medias de los datos.
# 

################################ Librerias #####################################
library(ggplot2)

install.packages("statsr")
library(statsr)

####################### carga el archivo por ventana en examinar ###############
#tiene que estar en formato csv

Data <- read.csv(file.choose(), header = T, sep=";")
#elegir "Knockout4.csv" para ver mejor los boxplots
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
################################################################################
A_405_20 <- Data$A450
Rt20 <- Data$Rt
t20 <- Data$t


# Define the data frame
Data_Fs <- data.frame(
  A_405_20,
  t20,
  Rt20 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (t20), y = A_405_20, fill = (Rt20))) +
  ggtitle("todos los Csour todos los ratios") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","gray50","gray50","gray50","gray50","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5,3.5,4.5), linetype = "dotted", color = "black") +
  theme_classic()


################################################################################
################################################################################
A_405_20 <- Data$A450
Rt20 <- Data$Csour
t20 <- Data$t


# Define the data frame
Data_Fs <- data.frame(
  A_405_20,
  t20,
  Rt20 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (t20), y = A_405_20, fill = (Rt20))) +
  ggtitle("todos los Csour vs tiempo ") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","yellow","red","orange","green","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5), linetype = "dotted", color = "black") +
  theme_classic()

#fish scales es mas estable vs CC 
################################################################################


#Rt70
#Se elije porque tiene el unico valor por debajo de 0.05. Ademas se elige
# 70 por el hecho que es mas diferente a los comparados con 20 vs otros ratios
Data_Rt70 = subset(Data, Rt=="70")
m1_Rt70 <- aov(A450 ~ t, data=Data_Rt70)
anova(m1_Rt70)
boxplot(A450 ~ t, data=Data_Rt70)# se plotean los datos solo para el ratio 70
########################### fin profe ##########################################
A_405_20 <- Data_Rt70$A450
Rt20 <- Data_Rt70$Rt
t20 <- Data_Rt70$t


# Define the data frame
Data_Fs <- data.frame(
  A_405_20,
  t20,
  Rt20 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (t20), y = A_405_20, fill = (Rt20))) +
  ggtitle("todos los Csour para el ratio 70") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","gray50","gray50","gray50","gray50","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5,3.5,4.5), linetype = "dotted", color = "black") +
  theme_classic()




################################################################################

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
# al tomar Fs de mio y maithe , ya sale p values
#no hay diferencias en los experimentos para Fs con ratio 30


boxplot(A450 ~ t, data=Data_selected_ratio)# se plotean los datos solo para el selected_ratio
############## box mejorado Ratio=20 con separaciones cada 10 min ################
# se asigna los valores de Data a vectores del mismo nombre
A_405_20 <- Data_selected_ratio$A450
Rt20 <- Data_selected_ratio$Rt
t20 <- Data_selected_ratio$t


# Define the data frame
Data_Fs <- data.frame(
  A_405_20,
  t20,
  Rt20 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (t20), y = A_405_20, fill = (Rt20))) +
  ggtitle("para Rt=30 y Csour=FS") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","gray50","gray50","gray50","gray50","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5,3.5,4.5), linetype = "dotted", color = "black") +
  theme_classic()

