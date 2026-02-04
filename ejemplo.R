# Ejemplo de script en R para Probabilidad y Estadística
# Example R script for Probability and Statistics

# Mostrar información de R
print("¡Bienvenido al entorno R en Codespaces!")
print(paste("Versión de R:", R.version.string))

# Ejemplo básico: Generar números aleatorios y calcular estadísticas
set.seed(123)
datos <- rnorm(100, mean = 50, sd = 10)

cat("\n=== Estadísticas Descriptivas ===\n")
cat("Media:", mean(datos), "\n")
cat("Mediana:", median(datos), "\n")
cat("Desviación Estándar:", sd(datos), "\n")
cat("Varianza:", var(datos), "\n")

# Crear un histograma simple
cat("\n¡El entorno R está funcionando correctamente!\n")
