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

