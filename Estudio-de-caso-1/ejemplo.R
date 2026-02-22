set.seed(2026)
n <- 2000  

# 1) Beta escalada (sesgada a la derecha)
cpu_beta <- rbeta(n, shape1 = 2, shape2 = 5) * 100
cat("\nResumen - Beta escalada:\n")
cat(sprintf("  n=%d  media=%.2f  mediana=%.2f  sd=%.2f\n", n, mean(cpu_beta), median(cpu_beta), sd(cpu_beta)))
cat(sprintf("  %% > 90: %.2f%%\n", mean(cpu_beta > 90) * 100))

# 2) Log-normal escalada (cola larga)
cpu_ln_raw <- rlnorm(n, meanlog = 3.5, sdlog = 1)
# Recortar valores extremos y escalar a 0-100 preservando sesgo
cpu_ln_trim <- pmin(cpu_ln_raw, quantile(cpu_ln_raw, 0.999))
cpu_ln <- (cpu_ln_trim - min(cpu_ln_trim)) / (max(cpu_ln_trim) - min(cpu_ln_trim)) * 100
cat("\nResumen - Log-normal escalada:\n")
cat(sprintf("  n=%d  media=%.2f  mediana=%.2f  sd=%.2f\n", n, mean(cpu_ln), median(cpu_ln), sd(cpu_ln)))
cat(sprintf("  %% > 90: %.2f%%\n", mean(cpu_ln > 90) * 100))

# Guardar histogramas lado a lado en PNG
png(filename = "cpu_histograms.png", width = 1200, height = 600)
par(mfrow = c(1, 2))

hist(cpu_beta, breaks = 30, col = "skyblue", main = "CPU (%) - Beta escalada", xlab = "Uso CPU (%)", xlim = c(0,100), prob = TRUE)
lines(density(cpu_beta), col = "darkblue", lwd = 2)

hist(cpu_ln, breaks = 30, col = "salmon", main = "CPU (%) - Log-normal escalada", xlab = "Uso CPU (%)", xlim = c(0,100), prob = TRUE)
lines(density(cpu_ln), col = "darkred", lwd = 2)

dev.off()
cat("\nSe generó el archivo 'cpu_histograms.png' con los dos histogramas.\n")

# Si se ejecuta en modo interactivo también los mostramos en pantalla
if (interactive()) {
  par(mfrow = c(1, 2))
  hist(cpu_beta, breaks = 30, col = "skyblue", main = "CPU (%) - Beta escalada", xlab = "Uso CPU (%)", xlim = c(0,100), prob = TRUE)
  lines(density(cpu_beta), col = "darkblue", lwd = 2)

  hist(cpu_ln, breaks = 30, col = "salmon", main = "CPU (%) - Log-normal escalada", xlab = "Uso CPU (%)", xlim = c(0,100), prob = TRUE)
  lines(density(cpu_ln), col = "darkred", lwd = 2)
  par(mfrow = c(1,1))
}

# ============================================================================
# PARTE 2: Teorema del Límite Central - Intervalos de Confianza 95%
# ============================================================================

cat("\n\n================================================================================")
cat("\nTEOREMA DEL LÍMITE CENTRAL - INTERVALOS DE CONFIANZA AL 95%")
cat("\n================================================================================\n")

# Definir tres tamaños de muestra
tamaños <- c(30, 100, 500)
num_simulaciones <- 1000  # número de muestras para cada tamaño
z_critico <- 1.96  # valor crítico para 95% de confianza

# Usar los datos cpu_beta como población
poblacion <- cpu_beta

# Almacenar resultados
resultados <- list()

# Para cada tamaño de muestra
for (i in seq_along(tamaños)) {
  n <- tamaños[i]
  
  cat(sprintf("\n--- Tamaño de muestra: n = %d ---\n", n))
  
  # Generar 'num_simulaciones' muestras de tamaño 'n' y calcular sus promedios
  promedios <- replicate(num_simulaciones, mean(sample(poblacion, size = n, replace = TRUE)))
  
  # a) Calcular media y desviación estándar de los promedios
  media_promedios <- mean(promedios)
  sd_promedios <- sd(promedios)
  
  cat(sprintf("  Media de promedios: %.4f\n", media_promedios))
  cat(sprintf("  Desv. estándar de promedios: %.4f\n", sd_promedios))
  cat(sprintf("  Valor crítico z: %.2f\n", z_critico))
  
  # b) Calcular intervalo de confianza del 95%
  margen_error <- z_critico * sd_promedios
  limite_inferior <- media_promedios - margen_error
  limite_superior <- media_promedios + margen_error
  
  cat(sprintf("\n  INTERVALO DE CONFIANZA 95%% CENTRAL:\n"))
  cat(sprintf("  Margen de error: %.4f\n", margen_error))
  cat(sprintf("  [ %.4f , %.4f ]\n", limite_inferior, limite_superior))
  
  # Guardar resultados para usar después en los gráficos
  resultados[[i]] <- list(
    n = n,
    promedios = promedios,
    media = media_promedios,
    sd = sd_promedios,
    limite_inf = limite_inferior,
    limite_sup = limite_superior
  )
}

# Crear histogramas de los promedios
png(filename = "promedios_histogramas.png", width = 1400, height = 1000)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))

for (i in seq_along(resultados)) {
  res <- resultados[[i]]
  
  # Crear histograma
  hist(res$promedios, breaks = 40, col = "lightblue", border = "black",
       main = sprintf("Distribución de promedios (n = %d), 1000 simulaciones", res$n),
       xlab = "Promedio", ylab = "Frecuencia", prob = TRUE)
  
  # Superponer la curva normal teórica
  x <- seq(min(res$promedios) - 3*res$sd, max(res$promedios) + 3*res$sd, length.out = 200)
  y <- dnorm(x, mean = res$media, sd = res$sd)
  lines(x, y, col = "darkblue", lwd = 2.5)
  
  # Sombrear el intervalo de confianza del 95%
  x_intervalo <- seq(res$limite_inf, res$limite_sup, length.out = 100)
  y_intervalo <- dnorm(x_intervalo, mean = res$media, sd = res$sd)
  polygon(c(res$limite_inf, x_intervalo, res$limite_sup),
          c(0, y_intervalo, 0), col = rgb(1, 0, 0, 0.3), border = "red", lwd = 2)
  
  # Líneas verticales para los límites
  abline(v = res$limite_inf, col = "red", lty = 2, lwd = 2)
  abline(v = res$limite_sup, col = "red", lty = 2, lwd = 2)
  abline(v = res$media, col = "green", lty = 1, lwd = 2)
  
  # Agregar texto con los valores
  text_info <- sprintf("Media: %.4f\nDE: %.4f\nIC 95%% = [%.4f, %.4f]",
                       res$media, res$sd, res$limite_inf, res$limite_sup)
  legend("topright", text_info, bty = "n", cex = 0.9)
}

dev.off()

cat("\n\nSe generó el archivo 'promedios_histogramas.png' con los histogramas de promedios.\n")

