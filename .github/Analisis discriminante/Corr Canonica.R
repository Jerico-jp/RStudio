# Instalar y cargar la biblioteca 'tidyverse' si aún no está instalada
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Cargar la biblioteca 'tidyverse'
library(tidyverse)

# Obtener el enlace directo al archivo CSV
url <- "https://raw.githubusercontent.com/Jerico-jp/RStudio/main/.github/workflows/correlacioncanonica.csv"

# Leer los datos desde el enlace
datos <- read_csv(url)

# Mostrar las primeras filas de los datos
head(datos)


# Matriz de covarianzas muestrales
covarianzas_muestrales <-round( cov(datos),4);covarianzas_muestrales

# Matriz de cavarianza sigma 11
# Generar datos de ejemplo
datos1 <- data.frame(
  Energia = datos$Energia,
  Proteina =datos$Proteina
)
covarianzas_muestrales_sigma11 <-round( cov(datos1),4);covarianzas_muestrales_sigma11
#Matriz de covarinzas sigma 22
datos2 <- data.frame(
  Carboidrato = datos$Carbohidrato,
  Azucar=datos$Azucares,
  Grasa= datos$Grasa
)
covarianzas_muestrales_sigma22 <-round( cov(datos2),4);covarianzas_muestrales_sigma22
#Matriz de covarinzas sigma 12
covarianzas_muestrales_sigma12 <-round( cov(datos1,datos2),4);covarianzas_muestrales_sigma12
#Matriz de covarinzas sigma 21
covarianzas_muestrales_sigma21 <-round( cov(datos2,datos1),4);covarianzas_muestrales_sigma21


# Matriz de correlaciones muestrales
correlaciones_muestrales <- round(cov(scale(datos)),4);correlaciones_muestrales

# Usaré la matriz de covarianzas para la correlacion canonica

covarianzas_muestrales_sigma11
inversa_covarianzas_muestrales_sigma11 <- solve(covarianzas_muestrales_sigma11);inversa_covarianzas_muestrales_sigma11

covarianzas_muestrales_sigma22
inversa_covarianzas_muestrales_sigma22 <- solve(covarianzas_muestrales_sigma22);inversa_covarianzas_muestrales_sigma22

##### Analisis para Sigma 11

# Hallamos el Eigen valor de sigma 11
covarianzas_muestrales_sigma11
eigenvalores11 <- eigen(covarianzas_muestrales_sigma11)$values;eigenvalores11
vectores11<-eigen(covarianzas_muestrales_sigma11)$vectors;vectores11
vectores11_T<-solve(vectores11);vectores11_T
lambda11<- diag(1/sqrt(eigenvalores11));lambda11

sigma11_medio<-vectores11 %*% (lambda11 %*% vectores11_T);sigma11_medio

multi_sigma11<- (sigma11_medio%*%(covarianzas_muestrales_sigma12%*%(inversa_covarianzas_muestrales_sigma22 %*%(covarianzas_muestrales_sigma21%*%sigma11_medio))))
multi_sigma11

#hallaremos los Eigen valores y Eigen vectores de  multi_sigma11
eigenvalores11 <- eigen(multi_sigma11)$values;eigenvalores11
vectores11<-(eigen(multi_sigma11)$vectors)*-1;vectores11

##### Analisis para sigma 22
# Hallamos el Eigen valor de sigma 22

eigenvalores22 <- eigen(covarianzas_muestrales_sigma22)$values;eigenvalores22
vectores22<-eigen(covarianzas_muestrales_sigma22)$vectors;vectores22
vectores22_T<-solve(vectores22);vectores22_T
lambda22<- diag(1/sqrt(eigenvalores22));lambda22
sigma22_medio<-vectores22 %*% (lambda22%*% vectores22_T);sigma22_medio

multi_sigma22<-(sigma22_medio%*%(covarianzas_muestrales_sigma21%*%(inversa_covarianzas_muestrales_sigma11 %*%(covarianzas_muestrales_sigma12%*%sigma22_medio))))
multi_sigma22
#hallaremos los Eigen valores y Eigen vectores de  multi_sigma11
eigenvalores22 <-round( eigen(multi_sigma22)$values,4);eigenvalores22
vectores22<-(eigen(multi_sigma22)$vectors)*-1;vectores22


# Analisis del Rho

# Para sigma 11
cat("Rho, el coeficiente de coorelacion canonica al cuadrado son los eigen valores")
cat("Tenemos a Rho^2_ 1=",eigenvalores11[1], "donde rho_1=",sqrt(eigenvalores11[1]))
cat("Tenemos a Rho^2_ 2=",eigenvalores11[2],"donde rho_2=",sqrt(eigenvalores11[2]))

cat(" ",
    if(sqrt(eigenvalores11[1])>sqrt(eigenvalores11[2])){
      cat("rho_1=",sqrt(eigenvalores11[1]),"es la correlacion canonica correspondiente al par de vectores aleatorios ")
    }else{
      cat("rho_2",sqrt(eigenvalores11[2])," es la correlacion canonica correspondiente ") 
    })
e1_trans11<-(vectores11[,1]) ; e1_trans11

a_trans11<-t(sigma11_medio %*% e1_trans11)
a_trans11



# Para sigma 22
cat("Rho, el coeficiente de coorelacion canonica al cuadrado son los eigen valores")
cat("Tenemos a Rho^2_ 1=",eigenvalores22[1], "donde rho_1=",sqrt(eigenvalores22[1]))
cat("Tenemos a Rho^2_ 2=",eigenvalores22[2],"donde rho_2=",sqrt(eigenvalores22[2]))
cat("Tenemos a Rho^2_ 3=",eigenvalores22[3],"donde rho_3=",sqrt(eigenvalores22[3]))

cat(" ")

if (sqrt(eigenvalores22[1]) > sqrt(eigenvalores22[2]) && sqrt(eigenvalores22[1]) > sqrt(eigenvalores22[3])) {
  cat("rho_1 =", sqrt(eigenvalores11[1]), "es la correlación canónica correspondiente al primer par de vectores aleatorios.")
} else if (sqrt(eigenvalores22[2]) > sqrt(eigenvalores22[1]) && sqrt(eigenvalores22[2]) > sqrt(eigenvalores22[3])) {
  cat("rho_2 =", sqrt(eigenvalores11[2]), "es la correlación canónica correspondiente al segundo par de vectores aleatorios.")
} else {
  cat("rho_3 =", sqrt(eigenvalores11[3]), "es la correlación canónica correspondiente al tercer par de vectores aleatorios.")
}


f1_trans22<-(vectores22[,1]) ; f1_trans22

b_trans22<-t(sigma22_medio %*% f1_trans22)
b_trans22




# Graficando

# Hallamos U y V
U <- (a_trans11[1] * datos$Energia + a_trans11[2] * datos$Proteina)
V <- (b_trans22[1] * datos$Carbohidrato + b_trans22[2] * datos$Azucares + b_trans22[3] * datos$Grasa)

# Crear un gráfico de dispersión
plot(U, V, main = "Gráfico de dispersión U vs V", xlab = "U", ylab = "V", col = "blue")

# Calcular la correlación entre U y V
correlacion_uv <- cor(U, V)

# Agregar texto con el valor de correlación en el gráfico
text(quantile(U, 0.5), quantile(V, 0.9), paste("Correlación Canónica =", round(correlacion_uv, 4)), pos = 4, cex = 0.8, col = "blue")

# Añadir la línea de regresión
abline(lm(V ~ U), col = "green", lwd = 2)





