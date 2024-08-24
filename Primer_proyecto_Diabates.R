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

#modelo 4
modelo4<- glm(diabetes_factor ~ . - smoking_history - gender,
              data = data_entrenamiento, family = "binomial")


summary(modelo4)

exp(cbind(OR =coef(modelo4), confint(modelo4)))

table(data_entrenamiento$diabetes_factor)

# aplicar el modelo a la tabla de entrenamiento
predicciones <- predict(modelo4, newdata = data_entrenamiento,
                        type = "response")
print(predicciones)

#los resultados arrojados están presentados en probabilidad. 
#Para la clasificación se pasan a valores 1 o 0. 1: diabetes"

predicciones_modelo4 <- if_else(predicciones > 0.5, 1, 0)

data_entrenamiento$predicciones_modelo4<- predicciones_modelo4
head (data_entrenamiento)

table(data_entrenamiento$diabetes_factor,data_entrenamiento$predicciones_modelo4)


table(data_entrenamiento$diabetes_factor)

#75047 NO diabeticos, 4953 diabeticos : posible interpretacion

#poner nombre a la matrix - SE LLAMA MATRIX DE CONFUSION
conf_matrix <- table(data_entrenamiento$diabetes_f, data_entrenamiento$predicciones_modelo4,
                     dnn = c("observaciones", "predicciones"))
print(conf_matrix)

#Interpretacion: 72482 que no tienen diabetes y el modelo predice que no tienen diabetes (True Negative)
                 #4312 que si tienen diabetes y que el modelo predice que si tienen diabetes.  (True Positive)
                 #2565 que si tienen diabetes y que el modelo predice que no tiene diabetes - malo (False Positive)
                 # 641 que no tienen diabtes y que el modelo predice que si tiene diabetes. - malo (False Negative)


Rendimiento del modelo
# Exactitud (accuracy)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

sum(conf_matrix)
sum(diag(conf_matrix))

# Sensibilidad (recall o tasa positiva verdadera)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste("Sensitivity:", sensitivity))


sum(conf_matrix[2, ])
conf_matrix[2, 2] 

# Especificidad (tasa negativa verdadera)
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
print(paste("Specificity:", specificity))

sum(conf_matrix[1, ])
conf_matrix[1, 1] 

Otros indicadores
install.packages("caret")
library(caret)
confusionMatrix(data=as.factor(data_entrenamiento$predicciones_modelo4), 
                reference = as.factor(data_entrenamiento$diabetes_f), 
                positive="1")

# Aplicar el modelo a la data de prueba
predicciones_prueba <- predict(modelo4, newdata = data_prueba,
                        type = "response")
print(predicciones_prueba)

predicciones_prueba_calif <- if_else(predicciones_prueba > 0.5, 1, 0)

data_prueba$predicciones_modelo4<- predicciones_prueba_calif
head (data_prueba)

conf_matriz_prueba<-table(data_prueba$diabetes_factor,data_prueba$predicciones_modelo4)


table(data_prueba$diabetes_factor)

#redimiento del modelo
# Exactitud (accuracy)
accuracy <- sum(diag(conf_matriz_prueba)) / sum(conf_matriz_prueba)
print(paste("Accuracy:", accuracy))

sum(conf_matriz_pruebax)
sum(diag(conf_matriz_prueba))

# Sensibilidad (recall o tasa positiva verdadera)
sensitivity <- conf_matriz_prueba[2, 2] / sum(conf_matriz_prueba[2, ])
print(paste("Sensitivity:", sensitivity))


sum(conf_matriz_prueba[2, ])
conf_matriz_prueba[2, 2] 

# Especificidad (tasa negativa verdadera)
specificity <- conf_matriz_prueba[1, 1] / sum(conf_matriz_prueba[1, ])
print(paste("Specificity:", specificity))

sum(conf_matriz_prueba[1, ])
conf_matriz_prueba[1, 1] 

#nota cuando los indicadores son mas altos en la data de entrenamiento y muy bajos y/o diferentes
#en la dat de prueba, puede estan indicando sobreajuste, eso se conoce en Overfitting

confusionMatrix(data=as.factor(data_prueba$predicciones_modelo4), 
                reference = as.factor(data_prueba$diabetes_factor), 
                positive="1")

head(data_prueba)


### K FOLD CROSS VALIDATION - VALIDACIÓN CRUZADA #####
install.packages("e1071")
library(e1071)
library(tidyverse)
library(dplyr)
library(ggplot2)


datos <- read.csv ("C:/Users/jocam/OneDrive/Escritorio/ESTUDIO/ANALISIS DE DATOS_MACHING/DATOS/diabetes_prediction_dataset.csv")
datos$diabetes_factor <- as.factor(datos$diabetes)
head(datos)
datos <- datos %>% 
  select(-diabetes)
head(datos)
dim(datos)

#datos <- datos[1:20,]

# "se procede a preparar los datos para crear 10 conjuntos de entrenamiento"

set.seed(123) #si no se esta corriendo el programa desde el principio fijar la semilla

#se barajan los indices del conjunto de datos 

n <- nrow(datos) 
indices_vc <- sample(1:n) # vc: validacion cruzada

#los indices_cv se dividen en 10 grupos 


grupos <- cut(indices_vc, breaks=10, labels=FALSE)

#"el siguiente paso es crear una funcion que permita entrenar el modelo, realizar
#predicciones y evaluar estas predicciones"

evaluar_modelos_vc <- function(data_entrenamiento_vc, data_prueba_vc, tipo_modelo) {
  
  if (tipo_modelo == "r_logistica") {
    # Se entrena el modelo de regresión logística
    modelo <- glm(diabetes_factor ~ . - smoking_history -gender,
                  family = binomial(link = 'logit'), data = data_entrenamiento_vc)
    
    
    # Se predicen las probabilidades
    predicciones_vc <- predict(modelo, newdata = data_prueba_vc, type = "response")
    
    # Dado que estas predicciones son dadas en probabilidad deben convertirse 
    # a un tipo de respuesta 0 o 1 (clases) dependiendo de si retira o no ( 1: se retira)
    predicciones_vc <- ifelse(predicciones_vc >= 0.5, 1, 0)
    
  } else if (tipo_modelo == "svm") {
    # Se entrena el modelo
    modelo <- svm(diabetes_factor ~ . - smoking_history -gender,
                  type = 'C-classification', kernel = 'radial', 
                  data = data_entrenamiento_vc)
    
    # Se predicen las clases. este modelo arroja directamente valores 0 o 1
    predicciones_vc <- predict(modelo, newdata = data_prueba_vc)
    
  }
  
  # Calcular métricas
  actual_vc <- data_prueba_vc$diabetes_factor
  confusion_vc <- table(Prediccion = predicciones_vc, Actual = actual_vc)
  exactitud_vc <- sum(diag(confusion_vc)) / sum(confusion_vc)
  sensibilidad_vc <- confusion_vc[2,2] / sum(confusion_vc[,2])
  especificidad_vc <- confusion_vc[1,1] / sum(confusion_vc[,1])
  f1 <- 2 * confusion_vc[2,2] / (2 * confusion_vc[2,2] + 
                                   confusion_vc[1,2] + 
                                   confusion_vc[2,1])
  
  
  return(c(Exactitud = exactitud_vc, Sensibilidad = sensibilidad_vc, 
           Especificidad = especificidad_vc, F1 = f1))
}


#Se crea el contenedor de los resultados de cada grupo o fold
resultados_vc <- list(r_logistica = vector("list", 10), svm = vector("list", 10))

for (i in 1:10) {
  # datos de entrenamiento y prueba para el fold o grupo actual
  prueba_indices <- which(grupos == i)
  entrenamiento_indices <- setdiff(1:n, prueba_indices)
  
  data_entrenamiento_vc <- datos[entrenamiento_indices, ]
  data_prueba_vc <- datos[prueba_indices, ]
  
  # Evaluación de los modelos para el fold o grupo en loop
  resultados_vc$r_logistica[[i]] <- evaluar_modelos_vc(data_entrenamiento_vc,
                                                       data_prueba_vc, "r_logistica")
  resultados_vc$svm[[i]] <- evaluar_modelos_vc(data_entrenamiento_vc,
                                               data_prueba_vc, "svm")
  
}






