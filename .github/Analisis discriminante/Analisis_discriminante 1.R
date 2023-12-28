################# CARGAMOS LA LIBRERIA #############

# install.packages("tidyverse")
# install.packages("caret")
library(readxl)
library(tidyverse)
library(caret)
library(MASS)

################# PREPARAMOS LOS DATOS ############
# Instalar y cargar la biblioteca 'tidyverse' si aún no está instalada
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
# Cargar la biblioteca 'tidyverse'
library(tidyverse)
# Obtener el enlace directo al archivo CS
url <- "https://raw.githubusercontent.com/Jerico-jp/RStudio/main/.github/Analisis%20discriminante/analisis_discriminate.csv"
# Leer los datos desde el enlace
datos <- read_csv(url)
# Mostrar las primeras filas de los datos
head(datos)




# Dividimos los datos en entrenamiento y conjunto de prueba
set.seed(123)
training.samples <- datos$`Cantidad_gastada` %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- datos[training.samples, ]
test.data <- datos[-training.samples, ]

# Normalizamos los datos. Las variables categóricas se ignoran automáticamente.
# Estimar parámetros de procesamiento
preproc.param <- train.data %>%
  preProcess(method = c("center", "scale"))
# Transformar los datos usando los parámetros estimados
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

# Análisis discriminante lineal
# Estimación del modelo
model <- lda(`Cantidad_gastada` ~ ., data = train.transformed)
# Hacer predicciones del modelo
predictions <- model %>% predict(test.transformed)
# Precisión del modelo
precision_porcentaje <- mean(predictions$class == test.transformed$`Cantidad_gastada`) * 100
cat("Precisión del modelo: ", precision_porcentaje, "%\n")

# Imprimir coeficientes del modelo
coeficientes <- model$scaling
cat("Los coeficientes del modelo son:\n")
print(coeficientes)

# Gráfico
plot(model, main = "Gráfico de discriminantes lineales")

# Establecer límites para el gráfico
xlim <- c(-2, 4)
ylim <- c(-3, 3)
# Crear un vector de colores según las categorías
colores <- ifelse(train.transformed$`Cantidad_gastada` == "Alta", "red",
                  ifelse(train.transformed$`Cantidad_gastada` == "Media", "yellow", "green"))
# Crear el gráfico con colores personalizados y límites ajustados
plot(model, col = colores, xlim = xlim, ylim = ylim)

# Haciendo predicciones
names(predictions)

# Predicción de clase
head(predictions$class)

# Probabilidades pronosticadas de pertenencia a una clase
head(round(predictions$posterior, 8))

# Discriminante lineal
head(predictions$x)

# Mostrar cantidad de observaciones clasificadas como "Alta"
sum(predictions$posterior[, 1] >= 0.5)

