# Este codigo contempla mas de 2 variables en Csour
# si los P values son menores a 0.05. se dice que hay diferencias
# en el caso de anova en las medias de los datos.
# 
rm(list = ls())
################################ Librerias #####################################
library(ggplot2)

install.packages("statsr")
library(statsr)

####################### carga el archivo por ventana en examinar ###############
#tiene que estar en formato csv

Data <- read.csv(file.choose(), header = T, sep=";")

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
#################################improved boxplot#######################################
A_450_all <- Data$A450
All_Ratio <- Data$Rt
All_time <- Data$t


# Define the data frame
Data_Fs <- data.frame(
  A_450_all,
  All_time,
  All_Ratio 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (All_time), y = A_450_all, fill = (All_Ratio))) +
  ggtitle("todos los Csour todos los ratios") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","yellow","cyan","green","gray50","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = "dotted", color = "black") +
  theme_classic()


################################################################################
################################################################################
Absorbance_450nm <- Data$A450
Colagen_source <- Data$Csour
Time_all <- Data$t


# Define the data frame
Data_Fs <- data.frame(
  Absorbance_450nm,
  Time_all,
  Colagen_source 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (Time_all), y = Absorbance_450nm, fill = (Colagen_source))) +
  ggtitle("All colagen sources vs time") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","yellow","red","orange","green","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = "dotted", color = "black") +
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

Absorbance_450nm_Ratio_70 <- Data_Rt70$A450
Rt_70 <- Data_Rt70$Rt
Time <- Data_Rt70$t


# Define the data frame
Data_Fs <- data.frame(
  Absorbance_450nm_Ratio_70,
  Time,
  Rt_70 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (Time), y = Absorbance_450nm_Ratio_70, fill = (Rt_70))) +
  ggtitle("All colagen sources for ratio 70") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","yellow","orange","white","red","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = "dotted", color = "black") +
  theme_classic()




################################################################################
rm(list = ls())
#mejores resultados en tukey y anova 
#$$$$$$$$$$$$$$$$$$$$$ Solo de FS #######################################
# Warning: Borrar las variables de workspace y cargar el archivo "Sin_Mai_ni_ Isaac.csv
# este archivo solo tiene el FS N3. mas adelante se hará con FS N3 y N5
Data <- read.csv(file.choose(), header = T, sep=";")

########################## Factoriza los datos ################################@
Data$t <- as.factor(Data$t)
Data$Csour <- as.factor(Data$Csour)
Data$Rt <- as.factor(Data$Rt)

Data_FS = subset(Data, Csour=="FS")
m2 <- aov(A450 ~ Rt, data=Data_FS) # se consideran solos los datos para FS
summary(m2)
TukeyHSD(m2)
boxplot(A450 ~ Rt, data=Data_FS)


################################ A450 ~ Rt plot ################################
Absorbance_450nm_Fs<- Data_FS$A450
All_ratios_Fs<- Data_FS$Rt



# Define the data frame
Data_Fs <- data.frame(
  Absorbance_450nm_Fs,
  All_ratios_Fs 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (All_ratios_Fs), y = Absorbance_450nm_Fs, fill = "magenta")) +
  ggtitle("Fs N3 for all ratios") +
  geom_boxplot() +
  scale_fill_manual(values = c("yellow","orange","white","red","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = "dotted", color = "black") +
  theme_classic()

#Rt30
#Se elije porque tiene un valor por debajo de 0.05. Ademas se elige
# 30 por el hecho que es mas diferente a los comparados vs otros ratios


################################################################################
########################## Final del codigo ####################################



# Revisar. Se supone que solo se eligen los datos para Rt=30 y Csour=FS
Data_selected_ratio = subset(Data_FS, Rt=="30")
m1_selected_ratio <- aov(A450 ~ t, data=Data_selected_ratio)
summary(m1_selected_ratio)
anova(m1_selected_ratio)
# porque en este anova me faltan datos para que se ploteen con varianza
# al tomar Fs de mio y maithe , ya sale p values
#no hay diferencias en los experimentos para Fs con ratio 30


boxplot(A450 ~ t, data=Data_selected_ratio)# se plotean los datos solo para el selected_ratio


################################ FS N3 y N5 ####################################

rm(list = ls())
# Warning: Borrar las variables de workspace y cargar el archivo "Knockout4"
Data <- read.csv(file.choose(), header = T, sep=";")

########################## Factoriza los datos ################################@
Data$t <- as.factor(Data$t)
Data$Csour <- as.factor(Data$Csour)
Data$Rt <- as.factor(Data$Rt)
#######################

Data_FS_N3_N5 = subset(Data, Csour=="FS")
m2 <- aov(A450 ~ Rt, data=Data_FS_N3_N5) # N3 y N5 Fish scales
summary(m2)
#TukeyHSD(m2)
#######################################################################

selected_ratio = subset(Data_FS_N3_N5, Rt=="30")
m1_selected_ratio <- aov(A450 ~ t, data=selected_ratio)
summary(m1_selected_ratio)
anova(m1_selected_ratio)# hace lo mismo que la linea anterior
#TukeyHSD(m1_selected_ratio)

# al tomar Fs de N3 y N5 , ya sale p values
# Sin embargo, no hay diferencias en los experimentos para Fs con ratio 30 
# por lo que se pueden usar ambas soluciones extractoras N3 y N5 para extraer colageno de Fs sin que hayan diferencias significativas en los resultados 


boxplot(A450 ~ t, data=selected_ratio)# se plotean los datos solo para el selected_ratio




############## box mejorado N3 N5 Fs ################


Absorbance_450nm_Fs_N3_N5 <- selected_ratio$A450
Rt_30 <- selected_ratio$Rt
Time <- selected_ratio$t


# Define the data frame
Data_Fs <- data.frame(
  Absorbance_450nm_Fs_N3_N5,
  Time,
  Rt_30 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (Time), y = Absorbance_450nm_Fs_N3_N5, fill = (Rt_30))) +
  ggtitle("FS N3 y N5 ratio 30 ") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","yellow","orange","white","red","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = "dotted", color = "black") +
  theme_classic()


###############################################################################
###################### FS N3 y N5 time 20-40 min ##############################
rm(list = ls())

# Warning: Borrar las variables de workspace y cargar el archivo "Knockout_20_40.csv"
Data <- read.csv(file.choose(), header = T, sep=";")

########################## Factoriza los datos ################################@
Data$t <- as.factor(Data$t)
Data$Csour <- as.factor(Data$Csour)
Data$Rt <- as.factor(Data$Rt)
#######################

Data_FS_N3_N5 = subset(Data, Csour=="FS")
m2 <- aov(A450 ~ Rt, data=Data_FS_N3_N5) # N3 y N5 Fish scales
summary(m2)
#TukeyHSD(m2)
#######################################################################

selected_ratio = subset(Data_FS_N3_N5, Rt=="30")
m1_selected_ratio <- aov(A450 ~ t, data=selected_ratio)
summary(m1_selected_ratio)
anova(m1_selected_ratio)# hace lo mismo que la linea anterior
#TukeyHSD(m1_selected_ratio)

# al tomar Fs de N3 y N5 , ya sale p values
# Sin embargo, no hay diferencias en los experimentos para Fs con ratio 30 
# por lo que se pueden usar ambas soluciones extractoras N3 y N5 para extraer colageno de Fs sin que hayan diferencias significativas en los resultados 


boxplot(A450 ~ t, data=selected_ratio)# se plotean los datos solo para el selected_ratio




############## box mejorado N3 N5 Fs 20-40 min ################


Absorbance_450nm_Fs_N3_N5 <- selected_ratio$A450
Rt_30 <- selected_ratio$Rt
Time <- selected_ratio$t


# Define the data frame
Data_Fs <- data.frame(
  Absorbance_450nm_Fs_N3_N5,
  Time,
  Rt_30 
)

# Create the boxplot with separated colors
ggplot(Data_Fs, aes(x = (Time), y = Absorbance_450nm_Fs_N3_N5, fill = (Rt_30))) +
  ggtitle("FS N3 y N5 ratio 30 , 20-40 min ") +
  geom_boxplot() +
  scale_fill_manual(values = c("magenta", "blue","yellow","orange","white","red","pink","purple")) +
  # Add vertical lines at Rt boundary
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = "dotted", color = "black") +
  theme_classic()
