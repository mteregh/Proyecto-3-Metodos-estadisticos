# =====================================================================
# UNIVERSIDAD DE LOS ANDES - METODOS ESTADISTICOS PARA LA GESTION
# PROYECTO COMPUTACIONAL 3 - GRUPO 25
# Experimento: rampa, masa adicional y distancia recorrida por autito
# =====================================================================
# Este script conserva la propuesta original y la aleatorizacion
# del archivo proyecto_3.R. Usa exclusivamente funciones de R base.
#
# INSTRUCCIONES:
# 1. Guarde este script en una carpeta de trabajo y ejecutelo completo.
# 2. Se creara "mediciones_autito.csv" con 36 corridas ALEATORIZADAS.
# 3. Realice las corridas EN ESE ORDEN y complete solo "distancia_cm"
#    con valores REALES en centimetros (decimales con PUNTO).
# 4. Guarde el CSV y ejecute nuevamente este mismo script completo.
#    Se produciran ANOVA, diagnosticos, superficie y optimo estimado.
# 5. Complete las tres distancias REALES de "confirmacion_autito.csv"
#    y ejecute una tercera vez para analizar la confirmacion.
# Nunca rellene los resultados con datos simulados o inventados.
# =====================================================================

options(stringsAsFactors = FALSE)
set.seed(25)

carpeta_resultados <- "resultados_proyecto3"
if (!dir.exists(carpeta_resultados)) dir.create(carpeta_resultados)

# ---------------------------------------------------------------------
# 1. DISENO FACTORIAL 3 x 2 x 2: 12 tratamientos x 3 replicas = 36
# ---------------------------------------------------------------------
altura <- c(10, 15, 20)  # cm: factor de 3 niveles igualmente espaciados
longitud <- c(40, 60)    # cm: factor de 2 niveles
masa <- c(0, 20)         # g: masa ADICIONAL

tratamientos <- expand.grid(altura = altura, longitud = longitud,
                           masa = masa)
tratamientos$tratamiento <- seq_len(nrow(tratamientos))

experimento <- tratamientos[
  rep(seq_len(nrow(tratamientos)), each = 3),
  c("tratamiento", "altura", "longitud", "masa")
]
experimento$replica <- rep(1:3, times = nrow(tratamientos))

# Aleatorizacion reproducible con la semilla exigida por el proyecto
experimento <- experimento[sample(seq_len(nrow(experimento))), ]
rownames(experimento) <- NULL
experimento$orden <- seq_len(nrow(experimento))
experimento <- experimento[, c("orden", "tratamiento", "replica",
                              "altura", "longitud", "masa")]

stopifnot(nrow(tratamientos) == 12L,
          nrow(experimento) == 36L,
          all(table(experimento$tratamiento) == 3L))

cat("\n--- DISENO ORIGINAL DEL GRUPO 25 ---\n")
print(tratamientos)
cat("\nCorridas: 36, en orden aleatorio con set.seed(25).\n")

# ---------------------------------------------------------------------
# 2. REGISTRO DE LAS 36 DISTANCIAS EXPERIMENTALES REALES
# ---------------------------------------------------------------------
archivo_datos <- "mediciones_autito.csv"

if (!file.exists(archivo_datos)) {
  plantilla <- experimento
  plantilla$distancia_cm <- NA_real_
  write.csv(plantilla, archivo_datos, row.names = FALSE, na = "")
  cat("\nSe creo:", archivo_datos, "\n")
  cat("Complete la columna distancia_cm con las mediciones reales.\n")
}

# Esta funcion contiene todo el analisis; si no hay mediciones,
# termina SIN cerrar RStudio y sin generar resultados ficticios.
analizar_proyecto <- function() {
  datos <- read.csv(archivo_datos, na.strings = c("", "NA"),
                    strip.white = TRUE)
  columnas_necesarias <- c(names(experimento), "distancia_cm")

  if (!all(columnas_necesarias %in% names(datos))) {
    stop("El CSV debe contener: ", paste(columnas_necesarias, collapse = ", "))
  }
  if (nrow(datos) != 36L) stop("Se requieren exactamente 36 filas.")

  # Protege contra errores al reordenar o editar las columnas del diseno
  for (nm in names(experimento)) {
    if (!isTRUE(all.equal(as.numeric(datos[[nm]]),
                          as.numeric(experimento[[nm]])))) {
      stop("La columna '", nm, "' no coincide con el diseno aleatorizado. ",
           "No cambie el orden ni los factores del CSV.")
    }
  }

  datos$distancia_cm <- suppressWarnings(as.numeric(datos$distancia_cm))
  n_validas <- sum(!is.na(datos$distancia_cm))
  if (n_validas < 36L) {
    cat("\nMediciones ingresadas:", n_validas, "de 36.\n")
    cat("Aun no se puede calcular ANOVA ni optimizacion.\n")
    cat("Complete las distancias reales en", archivo_datos, "y ejecute otra vez.\n")
    return(invisible(NULL))
  }
  if (any(!is.finite(datos$distancia_cm)) ||
      any(datos$distancia_cm < 0)) {
    stop("Las distancias deben ser numericas, finitas y >= 0 cm.")
  }
  n_valores_por_tratamiento <- tapply(datos$distancia_cm,
                                      datos$tratamiento,
                                      function(x) length(unique(x)))
  if (any(n_valores_por_tratamiento == 1L)) {
    warning("Hay tratamientos con sus 3 distancias identicas. ",
            "Revise la medicion y la condicion de variabilidad exigida.")
  }

  # -------------------------------------------------------------------
  # 3. RESUMEN DESCRIPTIVO DE LOS 12 TRATAMIENTOS
  # -------------------------------------------------------------------
  medias <- aggregate(distancia_cm ~ tratamiento + altura + longitud + masa,
                      data = datos, FUN = mean)
  desviaciones <- aggregate(distancia_cm ~ tratamiento,
                            data = datos, FUN = sd)
  names(medias)[names(medias) == "distancia_cm"] <- "media_cm"
  names(desviaciones)[2] <- "sd_cm"
  resumen <- merge(medias, desviaciones, by = "tratamiento", sort = FALSE)
  resumen <- resumen[order(resumen$tratamiento), ]
  write.csv(resumen,
            file.path(carpeta_resultados, "tabla_tratamientos.csv"),
            row.names = FALSE)
  cat("\n--- PROMEDIOS POR TRATAMIENTO ---\n")
  print(resumen, row.names = FALSE)

  # -------------------------------------------------------------------
  # 4. ANOVA FACTORIAL: A, B, C, AB, AC, BC y ABC
  # -------------------------------------------------------------------
  # Como el diseno es balanceado, el ANOVA secuencial de aov() permite
  # separar correctamente las sumas de cuadrados por efecto.
  datos$altura_f   <- factor(datos$altura, levels = altura)
  datos$longitud_f <- factor(datos$longitud, levels = longitud)
  datos$masa_f     <- factor(datos$masa, levels = masa)

  modelo_factorial <- aov(distancia_cm ~ altura_f * longitud_f * masa_f,
                          data = datos)
  tabla_anova <- summary(modelo_factorial)
  cat("\n--- ANOVA FACTORIAL 3 x 2 x 2 ---\n")
  print(tabla_anova)
  capture.output(tabla_anova,
                 file = file.path(carpeta_resultados, "anova_factorial.txt"))
  anova_tab <- as.data.frame(tabla_anova[[1]])
  anova_tab$Fuente <- rownames(anova_tab)
  rownames(anova_tab) <- NULL
  write.csv(anova_tab, file.path(carpeta_resultados,
                                  "anova_factorial.csv"), row.names = FALSE)

  # Efectos principales: diferencia marginal alto - bajo.
  # Para altura de 3 niveles tambien se muestran sus 3 medias.
  medias_altura   <- aggregate(distancia_cm ~ altura, datos, mean)
  medias_longitud <- aggregate(distancia_cm ~ longitud, datos, mean)
  medias_masa     <- aggregate(distancia_cm ~ masa, datos, mean)
  cat("\n--- EFECTOS MARGINALES ---\n")
  print(medias_altura)
  print(medias_longitud)
  print(medias_masa)
  cat("Efecto longitud (60-40 cm): ",
      diff(medias_longitud$distancia_cm), " cm de respuesta\n", sep = "")
  cat("Efecto masa adicional (20-0 g): ",
      diff(medias_masa$distancia_cm), " cm de respuesta\n", sep = "")

  # -------------------------------------------------------------------
  # 5. SUPUESTOS DEL ANOVA
  # -------------------------------------------------------------------
  residuos_factorial <- residuals(modelo_factorial)
  shapiro <- shapiro.test(residuos_factorial)
  # Fligner-Killeen es una prueba robusta de igualdad de varianzas.
  fligner <- fligner.test(distancia_cm ~ factor(tratamiento), data = datos)
  diagnosticos <- data.frame(
    prueba = c("Shapiro-Wilk de residuos (normalidad)",
               "Fligner-Killeen por tratamiento (homocedasticidad)"),
    estadistico = c(unname(shapiro$statistic), unname(fligner$statistic)),
    valor_p = c(shapiro$p.value, fligner$p.value)
  )
  write.csv(diagnosticos,
            file.path(carpeta_resultados, "pruebas_supuestos.csv"),
            row.names = FALSE)
  cat("\n--- REVISION DE SUPUESTOS ---\n")
  print(diagnosticos, row.names = FALSE)
  cat("Independencia: respaldada por aleatorizacion y protocolo; ",
      "se revisa el grafico de residuos vs orden.\n")

  png(file.path(carpeta_resultados, "diagnosticos_anova.png"),
      width = 1200, height = 900, res = 120)
  par(mfrow = c(2, 2))
  plot(modelo_factorial, which = c(1, 2, 3, 5))
  dev.off()

  png(file.path(carpeta_resultados, "residuos_vs_orden.png"),
      width = 1000, height = 650, res = 120)
  plot(datos$orden, residuos_factorial, type = "b", pch = 16,
       xlab = "Orden aleatorio de ejecucion", ylab = "Residuo (cm)",
       main = "Independencia: residuos vs orden de corrida")
  abline(h = 0, lty = 2)
  dev.off()

  png(file.path(carpeta_resultados, "efectos_principales.png"),
      width = 1100, height = 650, res = 120)
  plot.design(distancia_cm ~ altura_f + longitud_f + masa_f,
              data = datos, ylab = "Distancia media (cm)",
              main = "Efectos principales")
  dev.off()

  png(file.path(carpeta_resultados, "interacciones.png"),
      width = 1400, height = 450, res = 115)
  par(mfrow = c(1, 3), mar = c(5, 4, 4, 1))
  with(datos, interaction.plot(altura_f, longitud_f, distancia_cm,
                                xlab = "Altura (cm)", ylab = "Media (cm)",
                                trace.label = "Longitud", main = "Altura x longitud"))
  with(datos, interaction.plot(altura_f, masa_f, distancia_cm,
                                xlab = "Altura (cm)", ylab = "Media (cm)",
                                trace.label = "Masa", main = "Altura x masa"))
  with(datos, interaction.plot(longitud_f, masa_f, distancia_cm,
                                xlab = "Longitud (cm)", ylab = "Media (cm)",
                                trace.label = "Masa", main = "Longitud x masa"))
  dev.off()

  # -------------------------------------------------------------------
  # 6. SUPERFICIE DE RESPUESTA, SEGUNDO ORDEN ESTIMABLE
  # -------------------------------------------------------------------
  # Codificacion: xA=(altura-15)/5, xB=(longitud-50)/10,
  #               xC=(masa-10)/10. Todos varian entre -1 y +1;
  #               altura tambien tiene nivel central 0.
  # Solo altura^2 es estimable: longitud^2 = masa^2 = 1 en todo
  # el diseno y sus coeficientes se confunden con el intercepto.
  # Modelo basado en la lamina 29 de "8.2 Superficie de Respuesta".
  datos$xA <- (datos$altura - 15) / 5
  datos$xB <- (datos$longitud - 50) / 10
  datos$xC <- (datos$masa - 10) / 10

  modelo_respuesta <- lm(
    distancia_cm ~ xA + xB + xC + I(xA^2) + xA:xB + xA:xC + xB:xC,
    data = datos
  )
  cat("\n--- MODELO DE SUPERFICIE DE RESPUESTA ---\n")
  print(summary(modelo_respuesta))
  capture.output(summary(modelo_respuesta),
                 file = file.path(carpeta_resultados, "superficie_respuesta.txt"))
  write.csv(data.frame(termino = names(coef(modelo_respuesta)),
                       coeficiente = as.numeric(coef(modelo_respuesta))),
            file.path(carpeta_resultados, "coeficientes_superficie.csv"),
            row.names = FALSE)

  # Evaluacion de falta de ajuste contra el modelo de medias por celda:
  modelo_celdas <- lm(distancia_cm ~ factor(tratamiento), data = datos)
  falta_ajuste <- anova(modelo_respuesta, modelo_celdas)
  cat("\n--- PRUEBA DE FALTA DE AJUSTE ---\n")
  print(falta_ajuste)
  capture.output(falta_ajuste,
                 file = file.path(carpeta_resultados, "falta_de_ajuste.txt"))

  # -------------------------------------------------------------------
  # 7. OPTIMIZACION: MAXIMIZAR LA DISTANCIA DENTRO DE LA REGION
  # -------------------------------------------------------------------
  # Para cada (xB, xC) = (-1/+1,-1/+1) la respuesta es una parabola
  # en xA. Se comparan los extremos con el vertice si este es maximo
  # y esta dentro de [-1,1]. No se extrapola fuera del experimento.
  beta <- coef(modelo_respuesta)
  curvatura <- unname(beta["I(xA^2)"])
  soluciones <- list()
  k <- 0L

  for (xb in c(-1, 1)) {
    for (xc in c(-1, 1)) {
      b_lineal <- unname(beta["xA"] + beta["xA:xB"] * xb +
                           beta["xA:xC"] * xc)
      candidatos <- c(-1, 1)
      if (is.finite(curvatura) && curvatura < -1e-10) {
        vertice <- -b_lineal / (2 * curvatura)
        if (is.finite(vertice) && vertice >= -1 && vertice <= 1) {
          candidatos <- c(candidatos, vertice)
        }
      }
      candidatos <- unique(candidatos)
      para_predecir <- data.frame(xA = candidatos,
                                 xB = rep(xb, length(candidatos)),
                                 xC = rep(xc, length(candidatos)))
      estimaciones <- as.numeric(predict(modelo_respuesta,
                                          newdata = para_predecir))
      mejor <- which.max(estimaciones)
      k <- k + 1L
      soluciones[[k]] <- data.frame(xA = candidatos[mejor], xB = xb,
                                    xC = xc, prediccion_cm = estimaciones[mejor])
    }
  }

  opciones <- do.call(rbind, soluciones)
  opciones$altura_cm <- 15 + 5 * opciones$xA
  opciones$longitud_cm <- 50 + 10 * opciones$xB
  opciones$masa_g <- 10 + 10 * opciones$xC
  opciones <- opciones[order(-opciones$prediccion_cm), ]
  rownames(opciones) <- NULL
  write.csv(opciones,
            file.path(carpeta_resultados, "optimizacion_4_esquinas.csv"),
            row.names = FALSE)
  cat("\n--- MEJOR SOLUCION POR CADA PAR LONGITUD-MASA ---\n")
  print(opciones, row.names = FALSE)

  # Redondeo a 0,1 cm para facilitar una altura medible.
  # La prediccion se recalcula en la altura efectivamente recomendada.
  opt <- opciones[1, ]
  alt_recomendada <- round(opt$altura_cm, digits = 1)
  datos_opt <- data.frame(xA = (alt_recomendada - 15) / 5,
                          xB = opt$xB, xC = opt$xC)
  pred_opt <- as.numeric(predict(modelo_respuesta, newdata = datos_opt))
  optimo <- data.frame(altura_cm = alt_recomendada,
                       longitud_cm = opt$longitud_cm,
                       masa_g = opt$masa_g,
                       distancia_predicha_cm = pred_opt)
  write.csv(optimo,
            file.path(carpeta_resultados, "optimo_recomendado.csv"),
            row.names = FALSE)
  cat("\n--- OPTIMO RECOMENDADO ---\n")
  print(optimo, row.names = FALSE)

  # Prediccion de las 12 combinaciones de tratamiento originales
  puntos_originales <- tratamientos
  puntos_originales$xA <- (puntos_originales$altura - 15) / 5
  puntos_originales$xB <- (puntos_originales$longitud - 50) / 10
  puntos_originales$xC <- (puntos_originales$masa - 10) / 10
  puntos_originales$prediccion_cm <- predict(modelo_respuesta,
                                            newdata = puntos_originales)
  write.csv(puntos_originales,
            file.path(carpeta_resultados, "predicciones_12_tratamientos.csv"),
            row.names = FALSE)

  # Superficies y contornos para masa 0 y 20 g.
  # La longitud continua entre 40 y 60 es solo interpolacion del modelo:
  # NO fue ensayada en niveles intermedios.
  alt_seq <- seq(10, 20, length.out = 45)
  long_seq <- seq(40, 60, length.out = 45)
  guardar_grilla <- function(masa_real) {
    gr <- expand.grid(xA = (alt_seq - 15) / 5,
                      xB = (long_seq - 50) / 10)
    gr$xC <- (masa_real - 10) / 10
    matrix(predict(modelo_respuesta, newdata = gr),
           nrow = length(alt_seq), ncol = length(long_seq))
  }

  png(file.path(carpeta_resultados, "superficies_3d.png"),
      width = 1400, height = 650, res = 115)
  par(mfrow = c(1, 2))
  for (m in c(0, 20)) {
    z <- guardar_grilla(m)
    persp(alt_seq, long_seq, z, theta = 35, phi = 25,
          xlab = "Altura (cm)", ylab = "Longitud (cm)",
          zlab = "Distancia (cm)", col = "lightblue", shade = 0.4,
          main = paste("Masa adicional:", m, "g"))
  }
  dev.off()

  png(file.path(carpeta_resultados, "contornos.png"),
      width = 1400, height = 650, res = 115)
  par(mfrow = c(1, 2))
  for (m in c(0, 20)) {
    z <- guardar_grilla(m)
    contour(alt_seq, long_seq, z,
            xlab = "Altura (cm)", ylab = "Longitud (cm)",
            main = paste("Contornos: masa", m, "g"))
    points(rep(alt_seq[c(1, length(alt_seq))], 2),
           rep(long_seq[c(1, length(long_seq))], each = 2),
           pch = 16, cex = 0.7)
    if (optimo$masa_g == m) {
      points(optimo$altura_cm, optimo$longitud_cm,
             pch = 8, cex = 1.6, lwd = 2)
    }
  }
  dev.off()

  # -------------------------------------------------------------------
  # 8. TRES CORRIDAS DE CONFIRMACION EN EL OPTIMO
  # -------------------------------------------------------------------
  archivo_confirmacion <- "confirmacion_autito.csv"
  if (!file.exists(archivo_confirmacion)) {
    confirm <- data.frame(corrida = 1:3,
                          altura = rep(optimo$altura_cm, 3),
                          longitud = rep(optimo$longitud_cm, 3),
                          masa = rep(optimo$masa_g, 3),
                          distancia_cm = rep(NA_real_, 3))
    write.csv(confirm, archivo_confirmacion,
              row.names = FALSE, na = "")
    cat("\nSe creo:", archivo_confirmacion, "\n")
    cat("Haga 3 corridas REALES con la configuracion optima.\n")
  }
  conf <- read.csv(archivo_confirmacion, na.strings = c("", "NA"),
                   strip.white = TRUE)
  cols_conf <- c("corrida", "altura", "longitud", "masa", "distancia_cm")
  if (!all(cols_conf %in% names(conf)) || nrow(conf) != 3L) {
    stop("El archivo de confirmacion necesita 3 filas y columnas: ",
         paste(cols_conf, collapse = ", "))
  }
  if (!all(abs(conf$altura - optimo$altura_cm) < 1e-8 &
           abs(conf$longitud - optimo$longitud_cm) < 1e-8 &
           abs(conf$masa - optimo$masa_g) < 1e-8)) {
    warning("La confirmacion corresponde a una configuracion anterior. ",
            "Verifique el optimo, archive el CSV antiguo y repita las corridas.")
    return(invisible(optimo))
  }
  conf$distancia_cm <- suppressWarnings(as.numeric(conf$distancia_cm))
  if (anyNA(conf$distancia_cm)) {
    cat("\nConfirmacion pendiente: ",
        sum(!is.na(conf$distancia_cm)), " de 3 mediciones.\n", sep = "")
    return(invisible(optimo))
  }
  if (any(!is.finite(conf$distancia_cm)) ||
      any(conf$distancia_cm < 0)) {
    stop("Las distancias de confirmacion deben ser validas y >= 0.")
  }
  media_conf <- mean(conf$distancia_cm)
  sd_conf <- sd(conf$distancia_cm)
  sem_conf <- sd_conf / sqrt(3)
  ic <- media_conf + c(-1, 1) * qt(0.975, df = 2) * sem_conf
  tabla_confirmacion <- data.frame(
    altura_cm = optimo$altura_cm,
    longitud_cm = optimo$longitud_cm,
    masa_g = optimo$masa_g,
    prediccion_cm = optimo$distancia_predicha_cm,
    media_observada_cm = media_conf,
    sd_observada_cm = sd_conf,
    diferencia_observada_menos_predicha_cm =
      media_conf - optimo$distancia_predicha_cm,
    IC95_media_inferior_cm = ic[1], IC95_media_superior_cm = ic[2]
  )
  cat("\n--- CONFIRMACION DEL OPTIMO: 3 MEDICIONES REALES ---\n")
  print(tabla_confirmacion, row.names = FALSE)
  write.csv(tabla_confirmacion,
            file.path(carpeta_resultados, "confirmacion_resumen.csv"),
            row.names = FALSE)
  cat("\nANALISIS FINALIZADO. Ver archivos en:", carpeta_resultados, "\n")
  invisible(tabla_confirmacion)
}

analizar_proyecto()
