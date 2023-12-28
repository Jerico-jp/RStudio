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
dat <- read_csv(url)
# Mostrar las primeras filas de los datos
head(dat)


# Ajustar el modelo LDA
modelo1 <- lda(Cantidad_gastada ~ ., data = dat)
modelo1
# Coeficientes 
coeficientes <- modelo1$scaling;coeficientes;

columnas_interes <- dat[, c("Ingreso_familiar", "Actitud_viajes", "Importancia_vacaciones", "Tamano_familia", "Edad_jefe")]

# Calcular las matrices de correlaciones intragrupales
correlaciones_intragrupo <- cor(columnas_interes)

# Reemplazar NaN con 0 (si es necesario)
correlaciones_intragrupo[is.na(correlaciones_intragrupo)] <- 0

# Mostrar las matrices de correlaciones intragrupales
print(correlaciones_intragrupo)

# Presentar la matriz de correlaciones en forma de tabla
dt_correlaciones <- data.frame(correlaciones_intragrupo)
View(round(dt_correlaciones, 2))

# Ajustar un modelo MANOVA
modelo_manova <- manova(cbind(dat$Ingreso_familiar, dat$Actitud_viajes, 
                              dat$Importancia_vacaciones, dat$Tamano_familia, 
                              dat$Edad_jefe) ~ dat$Cantidad_gastada, data = dat)
# Obtener el estadístico lambda de Wilks y la razón F univariada
resumen_manova <- summary(modelo_manova, test = "Pillai")

# Imprimir resultados
cat("Lambda de Wilks:", resumen_manova$stats["dat$Cantidad_gastada", "Pillai"], "\n")
cat("Razón F univariada:", resumen_manova$stats["dat$Cantidad_gastada", "approx F num"], "\n")

# Establecer límites para el gráfico
xlim <- c(-3, 3)
ylim <- c(-4, 4)

dat$Cantidad_gastada <- as.factor(dat$Cantidad_gastada)

# Crear un vector de colores según las categorías
colores <- ifelse(dat$Cantidad_gastada == "Alta", "red",
                  ifelse(dat$Cantidad_gastada == "Media", "yellow", "green"))

# Crear el gráfico con colores personalizados y límites ajustados
plot(modelo1, col = colores, xlim = xlim, ylim = ylim)

# Añadir leyenda
legend("topright", legend = levels(dat$Cantidad_gastada), fill = c("red", "yellow", "green"))

##############  CENTROIDES ################

# Predecir las asignaciones de clase
predicciones <- predict(modelo1)

# Extraer las asignaciones de clase y asignar a las observaciones originales
dat$clase <- as.factor(predicciones$class)

# Calcular los centroides por grupo
centroides <- tapply(dat[, c("Ingreso_familiar", "Actitud_viajes", 
                             "Importancia_vacaciones", "Tamano_familia", 
                             "Edad_jefe")],
                     INDEX = dat$clase, FUN = colMeans)

# Imprimir los centroides
print("Centroides por grupo:")
print(centroides)

