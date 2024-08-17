# Ejemplo del uso de comentarios
a <- 10  # Asignar el valor 10 a la variable a
b <- 5   # Asignar el valor 5 a la variable b

# Realizar algunas operaciones matemáticas
suma <- a + b         # Sumar a y b
suma
resta <- a - b        # Restar b de a
resta

#algunos paquetes importantes
"tidyverse: es un conjunto de paquetes en R diseñados para facilitar el
análisis de datos de manera coherente y eficiente."


# Sin usar el pipe
resultado <- func2(func1(data))

# Usando el pipe
resultado <- data %>% 
  func1() %>% 
  func2()

"dplyr: es un paquete en R diseñado para la manipulación de datos de 
manera eficiente y sencilla.
    
    df %>% filter(Age > 30)
    df %>% select(Name, Age)
    df %>% arrange(desc(Score))
    df %>% mutate(Age_in_months = Age * 12)"

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)

#cargar datos#

datos <- read.csv ("C:/Users/jocam/OneDrive/Escritorio/ESTUDIO/ANALISIS DE DATOS_MACHING/DATOS/diabetes_prediction_dataset.csv")
# ver los primeros seis datos#
head (datos)
# ver los primeros tres datos#
head (datos,3)
#ver la dimension del Dataframe#
dim (datos)
#ver cola del archivo los ultimos seis#
tail (datos)
# ver como esta leyendo el programa cada variable#
str (datos)
#crear una base de datos solo con 1000 datos#
datos1 <- datos[1:1000,]
#tomas todas las filas, de la columna 1  a la siete
datos2 <- datos[,1:7]
# verificar nuevas dimensiones
dim (datos1)
dim (datos2)
# 
names(datos)

datos3 <- select(datos,gender,age,hypertension)
head (datos3)
#tomar ciertas filas y columnas
# selecciona las filas 1 a la 30 para las columnas 2 y 3.
datos4 <- datos[1:30, 2:3]
dim (datos4)
head (datos4)
str (datos4)
#cambiar el formato de las variables#
datos4$age
datos4$hypertension_f <- as.factor(datos4$hypertension)
head (datos4)
str(datos4)
mean (datos4$hypertension)
mean (datos4$hypertension_f)
head(datos)

#la variable de interes de que tipo es ?
str(datos$diabetes)
table(datos$diabetes)


#EDA - ANALISIS EXPLORATORIO# Exploratory data analysis #
head(datos)
#analisis variables categoricas#
frecuencias <- table(datos$gender)
frecuencias_ordenadas <- sort(frecuencias, decreasing = TRUE)
# Crear gráfico de barras:
barplot(frecuencias_ordenadas, main=paste("genero"),
        ylab= 'Frecuencia',
        col='blue', las = 1, cex.names = 0.75, cex.axis = 0.8)
str (datos$hypertension)
frecuencias_hipertension <- table(datos$hypertension)
barplot(frecuencias_hipertension, main=paste("hipertension"),
        ylab= 'Frecuencia',
        col='blue', las = 1, cex.names = 0.75, cex.axis = 0.8)

#pasar las variables a caracter#
str (datos)
datos$hypertension <- as.character(datos$hypertension)
datos$heart_disease <- as.character(datos$heart_disease)
datos$diabetes <- as.character(datos$diabetes)

#tip: identificar todas las variables tipo caracter de un dataframe
variables_categoricas <- sapply(datos, is.character)
variables_categoricas
nombres_variables_categoricas <- names(variables_categoricas[variables_categoricas])


for (categoricas in nombres_variables_categoricas){
  
  #calcular tabla de frecuencias
  frecuencias <- table(datos[[categoricas]])
  
  frecuencias_ordenadas <- sort(frecuencias, decreasing = TRUE)
  top_10 <- head(frecuencias_ordenadas, 10)
  
  # Crear gráfico de barras:
  barplot(top_10, main=paste("Top 10 de", categoricas),
          ylab= 'Frecuencia',
          col='blue', las = 1, cex.names = 0.75, cex.axis = 0.8)
}

summary (datos)

# Graficos para variables numericas#
# diagramas
library(ggplot2)
install.packages("gridExtra") 
library(gridExtra)

# Crear un segundo gráfico de ejemplo (histograma)
plot1 <- ggplot(datos, aes(x = age)) +
  geom_histogram(bins = 8, fill = "lightgreen", color = "black") +
  labs(title = "Histograma de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

# Crear el primer boxplot
plot2 <- ggplot(datos, aes(y = age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de Edad", y = "Edad") +
  theme_minimal()
  


grid.arrange(plot1, plot2, nrow = 1, ncol = 2)

#GRAFICAR OTRA VARIABLE
plot1 <- ggplot(datos, aes(x = bmi)) +
  geom_histogram(bins = 8, fill = "lightgreen", color = "black") +
  labs(title = "Histograma de bmi", x = "bmi", y = "Frecuencia") +
  theme_minimal()

# Crear el primer boxplot
plot2 <- ggplot(datos, aes(y = bmi)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de bmi", y = "bmi") +
  theme_minimal()

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)

head (datos)

#GRAFICAR OTRA VARIABLE
plot1 <- ggplot(datos, aes(x = HbA1c_level)) +
  geom_histogram(bins = 8, fill = "lightgreen", color = "black") +
  labs(title = "Histograma de HbA1c_level", x = "HbA1c_level", y = "Frecuencia") +
  theme_minimal()

# Crear el primer boxplot
plot2 <- ggplot(datos, aes(y = HbA1c_level)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de HbA1c_level", y = "HbA1c_level") +
  theme_minimal()

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)
summary (datos$HbA1c_level)

#GRAFICAR OTRA VARIABLE
plot1 <- ggplot(datos, aes(x = blood_glucose_level)) +
  geom_histogram(bins = 8, fill = "lightgreen", color = "black") +
  labs(title = "Histograma de blood_glucose_level", x = "blood_glucose_level", y = "Frecuencia") +
  theme_minimal()

# Crear el primer boxplot
plot2 <- ggplot(datos, aes(y = blood_glucose_level)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de blood_glucose_level", y = "Hblood_glucose_level") +
  theme_minimal()

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)

str (datos)

#graficar dos variables categoricas, en este grafico no aporta mucha informacion
ggplot(datos, 
       aes(x=heart_disease, 
           y=diabetes,
           color=diabetes)) +
  geom_point()

# graficar dos variables categoricas
table(datos$diabetes,datos$heart_disease)
table(datos$diabetes)

#en esta tabla vamos a verificar por porcentajes.
prop.table(table(datos$diabetes,datos$heart_disease),margin = 1) # el 14 % de los diabeticos tiene heart
prop.table(table(datos$diabetes,datos$hypertension),margin = 1) # el 24 porcientos de los diaberticos tiene hipertension
prop.table(table(datos$diabetes,datos$gender),margin = 1) # el 47 porciento de los diabeticos son hombres
prop.table(table(datos$diabetes,datos$smoking_history ),margin = 1)


table(datos$diabetes,datos$smoking_history)

#grficar variables numericas , con el target
ggplot(datos, 
       aes(x=age, 
           y=diabetes,
           )) +
  geom_point()

ggplot(datos, 
       aes(x=age, 
           y=diabetes,
           color=diabetes)) +
  geom_point()
# 
ggplot(datos, 
       aes(x=age, 
           y=diabetes,
           color=blood_glucose_level)) +
  geom_point()

ggplot(datos, 
       aes(x=blood_glucose_level, 
           y=diabetes,
           )) +
  geom_point()

ggplot(datos, 
       aes(x=HbA1c_level, 
           y=diabetes,
       )) +
  geom_point()

# Identificar variables numéricas
variables_numericas <- sapply(datos, is.numeric)
# Crear un nuevo data frame con solo las variables numéricas
datos_numericos <- datos[, variables_numericas]
head (datos_numericos)

install.packages("corrplot")
library(corrplot)
#verificar correelaciones
correlacion<-round(cor(datos_numericos), 1)
par(mfrow = c(1, 1))

corrplot(correlacion, method="number", type="upper")

#grafica de la variable target con variable cuantitativa.
#crear primeros diabtes_f 
datos$diabetes_factor <-as.factor(datos$diabetes)
str(datos)
head(datos)
# Aplicacion del algoritmo regresión logistica
ggplot(data = datos, mapping = aes(x = diabetes_factor, y = blood_glucose_level)) +
  geom_boxplot(aes(color = diabetes_factor)) +
  geom_point(aes(color = diabetes_factor)) +
  theme_bw() +
  theme(legend.position = "null")

#prueba de diferencia de medias o de promedios
#Primera hipotesis es nula es la igualdad
#Segunda hipotesis 
t.test(subset(datos, diabetes_factor=="0")$blood_glucose_level,
       subset(datos, diabetes_factor=="1")$blood_glucose_level)


#regresion logistica
modelo.log.s <- glm(diabetes_factor ~ blood_glucose_level, 
                    data = datos, family = binomial)
summary(modelo.log.s)
#representacion grafica
theme_bw() +
  theme(legend.position = "none")
ggplot(data = datos, aes(x = blood_glucose_level, y = diabetes)) +
  geom_point(aes(color = as.factor(diabetes)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo.log.s,
                                          newdata = data.frame(blood_glucose_level = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")
# Con geom_smooth se puede obtener el gráfico directamente.
ggplot(data = datos, aes(x = blood_glucose_level, y = diabetes)) +
  geom_point(aes(color = as.factor(diabetes)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE) +
  theme_bw() +
  theme(legend.position = "none")

#construccion la base de entrenamiento y la base de ´prueba.
#fijar una semilla.
set.seed(123)
tamano_entrenamiento <- floor(0.8 * nrow(datos))
# Crear índice aleatorio para el conjunto de entrenamiento
indices_entrenamiento <- sample(seq_len(nrow(datos)),
                                size = tamano_entrenamiento)

data_entrenamiento <- datos[indices_entrenamiento, ]
dim (data_entrenamiento)
head (data_entrenamiento)
data_prueba <- datos[-indices_entrenamiento, ]
dim (data_prueba)


#ahora imprimos modelo en la data_entrenamiento.
modelo1 <- glm(diabetes_factor ~ blood_glucose_level, 
                    data = data_entrenamiento, family = binomial)
summary(modelo1)
#dibujar el modelo
# Ajuste de un modelo logístico.
modelo_logistico <- glm(diabetes_factor ~ blood_glucose_level,
                        data = data_entrenamiento, family = "binomial")

library(ggplot2)

# Representación gráfica del modelo.
ggplot(data = data_entrenamiento, aes(x = blood_glucose_level, 
                                      y = diabetes_factor)) +
  geom_point(aes(color = as.factor(diabetes_factor)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo_logistico,
                                          newdata = data.frame(blood_glucose_level = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

#Interpretacion de Odd ratios
exp(cbind(OR =coef(modelo1), confint(modelo1)))

#construccion de otro modelo
data_entrenamiento <- data_entrenamiento %>% select(-diabetes)
head (data_entrenamiento)
modelo2 <- glm(diabetes_factor ~ ., 
               data = data_entrenamiento, family = binomial)
summary(modelo2)



modelo3<- glm(diabetes_factor ~ . - smoking_history,
                        data = data_entrenamiento, family = "binomial")
summary(modelo3)

