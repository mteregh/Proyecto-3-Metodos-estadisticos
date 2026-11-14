# PROYECTO COMPUTACIONAL 3 - GRUPO 25
# Experimento: distancia recorrida por un autito
# Codigo basado en las clases de diseno factorial, ANOVA y superficie de respuesta.

# 1. DISENO DEL EXPERIMENTO (codigo de la propuesta)
altura <- c(10, 15, 20)
longitud <- c(40, 60)
masa <- c(0, 20)

diseno <- expand.grid(altura = altura, longitud = longitud, masa = masa)
experimento <- diseno[rep(1:nrow(diseno), each = 3), ]
experimento$tratamiento <- rep(1:12, each = 3)
experimento$replica <- rep(1:3, times = 12)

set.seed(25)
experimento <- experimento[sample(1:nrow(experimento)), ]
rownames(experimento) <- NULL
experimento$orden <- 1:36
print(experimento)

# 2. DATOS: 36 CORRIDAS + 3 CONFIRMACIONES
# Guardar el CSV en la misma carpeta que este archivo R.
datos_completos <- read.csv("resultados_p3_datos.csv")
datos <- datos_completos[1:36, ]
confirmacion <- datos_completos[37:39, ]

# Medias de distancia para cada uno de los 12 tratamientos
medias <- aggregate(distancia_cm ~ altura + longitud + masa,
                   data = datos, FUN = mean)
print(medias)

# 3. ANOVA FACTORIAL 3 x 2 x 2
# Se estudian los efectos principales y las interacciones.
datos$A <- factor(datos$altura)
datos$B <- factor(datos$longitud)
datos$C <- factor(datos$masa)

modelo_anova <- aov(distancia_cm ~ A * B * C, data = datos)
print(summary(modelo_anova))

# 4. GRAFICOS DE EFECTOS E INTERACCIONES
png("interacciones.png", width = 1000, height = 350)
par(mfrow = c(1, 3))
interaction.plot(datos$A, datos$B, datos$distancia_cm,
                 xlab = "Altura", ylab = "Distancia media",
                 main = "Altura y longitud")
interaction.plot(datos$A, datos$C, datos$distancia_cm,
                 xlab = "Altura", ylab = "Distancia media",
                 main = "Altura y masa")
interaction.plot(datos$B, datos$C, datos$distancia_cm,
                 xlab = "Longitud", ylab = "Distancia media",
                 main = "Longitud y masa")
dev.off()

# 5. SUPUESTOS DEL ANOVA
# Normalidad: Q-Q plot y prueba Shapiro-Wilk.
# Varianza constante: residuos versus valores ajustados.
png("supuestos_anova.png", width = 850, height = 700)
par(mfrow = c(2, 2))
plot(modelo_anova)
dev.off()
print(shapiro.test(residuals(modelo_anova)))

# Independencia: revisar si hay patrones en los residuos segun el orden.
png("residuos_orden.png", width = 750, height = 450)
plot(datos$orden, residuals(modelo_anova),
     xlab = "Orden de la corrida", ylab = "Residuo",
     main = "Residuos versus orden")
abline(h = 0, lty = 2)
dev.off()

# 6. SUPERFICIE DE RESPUESTA
# Variables codificadas como en clases: -1, 0 y +1.
# Solo la altura tiene tres niveles; por eso solo se incluye altura^2.
datos$xA <- (datos$altura - 15) / 5
datos$xB <- (datos$longitud - 50) / 10
datos$xC <- (datos$masa - 10) / 10

modelo_superficie <- lm(distancia_cm ~ xA + xB + xC +
                        I(xA^2) + xA:xB + xA:xC + xB:xC,
                        data = datos)
print(summary(modelo_superficie))

# Comparacion con el modelo que usa una media para cada tratamiento.
# Sirve para revisar la falta de ajuste.
modelo_celdas <- lm(distancia_cm ~ factor(tratamiento), data = datos)
print(anova(modelo_superficie, modelo_celdas))

# 7. OPTIMIZACION DENTRO DE LA REGION ESTUDIADA
# En estos datos el coeficiente de altura^2 es positivo: la parabola
# tiene un minimo. Por eso su maximo esta en un extremo (lamina 29).
print(coef(modelo_superficie)["I(xA^2)"])

# Evaluamos las 8 esquinas del rango experimental.
opciones <- expand.grid(altura = c(10, 20),
                        longitud = c(40, 60), masa = c(0, 20))
opciones$xA <- (opciones$altura - 15) / 5
opciones$xB <- (opciones$longitud - 50) / 10
opciones$xC <- (opciones$masa - 10) / 10
opciones$prediccion <- predict(modelo_superficie, newdata = opciones)
print(opciones)

mejor <- opciones[which.max(opciones$prediccion), ]
altura_optima <- mejor$altura
longitud_optima <- mejor$longitud
masa_optima <- mejor$masa

cat("\nRESULTADO OPTIMO\n")
cat("Altura:", altura_optima, "cm\n")
cat("Longitud:", longitud_optima, "cm\n")
cat("Masa adicional:", masa_optima, "g\n")
cat("Distancia predicha:", mejor$prediccion, "cm\n")

# 8. GRAFICOS DE CONTORNOS DE LA SUPERFICIE (masa 0 y 20 g)
alturas_grafico <- seq(-1, 1, length.out = 30)
longitudes_grafico <- seq(-1, 1, length.out = 30)

png("superficie_contornos.png", width = 950, height = 450)
par(mfrow = c(1, 2))
for (xc in c(-1, 1)) {
  grilla <- expand.grid(xA = alturas_grafico, xB = longitudes_grafico)
  grilla$xC <- xc
  predicciones <- predict(modelo_superficie, newdata = grilla)
  z <- matrix(predicciones, nrow = 30)
  contour(15 + 5 * alturas_grafico,
          50 + 10 * longitudes_grafico, z,
          xlab = "Altura (cm)", ylab = "Longitud (cm)",
          main = paste("Masa:", 10 + 10 * xc, "g"))
}
dev.off()

# 9. TRES CORRIDAS ADICIONALES PARA CONFIRMAR EL OPTIMO
cat("\nDISTANCIAS DE CONFIRMACION (cm)\n")
print(confirmacion$distancia_cm)
media_confirmacion <- mean(confirmacion$distancia_cm)

# Se compara el promedio confirmado con la prediccion en ese mismo punto.
punto_confirmacion <- data.frame(
  xA = (confirmacion$altura[1] - 15) / 5,
  xB = (confirmacion$longitud[1] - 50) / 10,
  xC = (confirmacion$masa[1] - 10) / 10
)
prediccion_confirmacion <- predict(modelo_superficie,
                                   newdata = punto_confirmacion)
cat("Promedio de confirmacion:", media_confirmacion, "cm\n")
cat("Prediccion del modelo en ese punto:", prediccion_confirmacion, "cm\n")
cat("Diferencia:", media_confirmacion - prediccion_confirmacion, "cm\n")

if (confirmacion$altura[1] == altura_optima &&
    confirmacion$longitud[1] == longitud_optima &&
    confirmacion$masa[1] == masa_optima) {
  cat("Las corridas adicionales verifican el punto optimo calculado.\n")
} else {
  cat("Atencion: las corridas adicionales no se hicieron exactamente en el optimo.\n")
}
