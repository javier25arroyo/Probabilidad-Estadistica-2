# Practica 3 - Pruebas de hipotesis
# Probabilidad y Estadistica
# Autor: (tu nombre)
# Fecha: 2026-03-25

cat("========================================\n")
cat("PRACTICA 3: PRUEBAS DE HIPOTESIS (5 casos)\n")
cat("========================================\n\n")

# Funcion auxiliar para imprimir decision
decision_text <- function(p_value, alpha) {
  if (p_value < alpha) {
    return("Se rechaza H0")
  } else {
    return("No se rechaza H0")
  }
}

# =========================================================
# PROBLEMA 1
# =========================================================
cat("PROBLEMA 1\n")
cat("Contexto: Tiempo de respuesta de API (< 250 ms)\n\n")

# Datos
n1 <- 40
xbar1 <- 243
mu0_1 <- 250
sigma1 <- 30
alpha1 <- 0.05

# Paso 1: Hipotesis
cat("Paso 1) Hipotesis:\n")
cat("H0: mu >= 250 ms\n")
cat("H1: mu < 250 ms\n\n")

# Paso 2: Nivel de significancia
cat("Paso 2) Nivel de significancia:\n")
cat("alpha =", alpha1, "\n\n")

# Paso 3: Estadistico de prueba (Z) y p-valor
z1 <- (xbar1 - mu0_1) / (sigma1 / sqrt(n1))
p1 <- pnorm(z1) # cola izquierda
zcrit1 <- qnorm(alpha1)

cat("Paso 3) Calculos:\n")
cat("z =", round(z1, 4), "\n")
cat("z critico (cola izq.) =", round(zcrit1, 4), "\n")
cat("p-valor =", round(p1, 6), "\n\n")

# Paso 4: Regla de decision
cat("Paso 4) Decision:\n")
cat(decision_text(p1, alpha1), "\n\n")

# Paso 5: Conclusion en contexto
cat("Paso 5) Conclusion:\n")
if (p1 < alpha1) {
  cat("Con 95% de confianza, SI hay evidencia para afirmar que el tiempo promedio es menor a 250 ms.\n\n")
} else {
  cat("Con 95% de confianza, NO hay evidencia suficiente para afirmar que el tiempo promedio es menor a 250 ms.\n\n")
}

# =========================================================
# PROBLEMA 2
# =========================================================
cat("PROBLEMA 2\n")
cat("Contexto: Proporcion de fallos en despliegues CI/CD\n\n")

# Datos
n2 <- 150
x2 <- 22
phat2 <- x2 / n2
p0_2 <- 0.20
alpha2 <- 0.10

# Paso 1
cat("Paso 1) Hipotesis:\n")
cat("H0: p >= 0.20\n")
cat("H1: p < 0.20\n\n")

# Paso 2
cat("Paso 2) Nivel de significancia:\n")
cat("alpha =", alpha2, "\n\n")

# Paso 3
z2 <- (phat2 - p0_2) / sqrt(p0_2 * (1 - p0_2) / n2)
p2 <- pnorm(z2)
zcrit2 <- qnorm(alpha2)

cat("Paso 3) Calculos:\n")
cat("p gorro =", round(phat2, 4), "\n")
cat("z =", round(z2, 4), "\n")
cat("z critico (cola izq.) =", round(zcrit2, 4), "\n")
cat("p-valor =", round(p2, 6), "\n\n")

# Paso 4
cat("Paso 4) Decision:\n")
cat(decision_text(p2, alpha2), "\n\n")

# Paso 5
cat("Paso 5) Conclusion:\n")
if (p2 < alpha2) {
  cat("Con 90% de confianza, SI se puede concluir que la proporcion de fallos disminuyo con la nueva herramienta.\n\n")
} else {
  cat("Con 90% de confianza, NO se puede concluir que la proporcion de fallos disminuyo.\n\n")
}

# =========================================================
# PROBLEMA 3
# =========================================================
cat("PROBLEMA 3\n")
cat("Contexto: Comparacion de tiempos MongoDB vs PostgreSQL\n\n")

# Datos resumidos
n_m <- 30
xbar_m <- 303.4
s_m <- 7.89

n_p <- 30
xbar_p <- 275.8
s_p <- 5.43

alpha3 <- 0.05

# Paso 1
cat("Paso 1) Hipotesis:\n")
cat("H0: mu_PostgreSQL >= mu_MongoDB\n")
cat("H1: mu_PostgreSQL < mu_MongoDB\n\n")

# Paso 2
cat("Paso 2) Nivel de significancia:\n")
cat("alpha =", alpha3, "\n\n")

# Paso 3: t de Welch con estadisticas resumidas
se3 <- sqrt((s_p^2 / n_p) + (s_m^2 / n_m))
t3 <- (xbar_p - xbar_m) / se3
df3 <- ((s_p^2 / n_p + s_m^2 / n_m)^2) /
  (((s_p^2 / n_p)^2 / (n_p - 1)) + ((s_m^2 / n_m)^2 / (n_m - 1)))
p3 <- pt(t3, df = df3) # cola izquierda
tcrit3 <- qt(alpha3, df = df3)

cat("Paso 3) Calculos:\n")
cat("t =", round(t3, 4), "\n")
cat("gl (Welch) =", round(df3, 4), "\n")
cat("t critico (cola izq.) =", round(tcrit3, 4), "\n")
cat("p-valor =", round(p3, 8), "\n\n")

# Paso 4
cat("Paso 4) Decision:\n")
cat(decision_text(p3, alpha3), "\n\n")

# Paso 5
cat("Paso 5) Conclusion:\n")
if (p3 < alpha3) {
  cat("Con 95% de confianza, SI se puede afirmar que PostgreSQL tiene menor tiempo promedio que MongoDB.\n\n")
} else {
  cat("Con 95% de confianza, NO se puede afirmar que PostgreSQL tenga menor tiempo promedio.\n\n")
}

# =========================================================
# PROBLEMA 4
# =========================================================
cat("PROBLEMA 4\n")
cat("Contexto: Diferencia en proporcion de satisfaccion (movil vs web)\n\n")

# Datos
x_movil <- 120
n_movil <- 150
x_web <- 100
n_web <- 160
alpha4 <- 0.05

p_movil <- x_movil / n_movil
p_web <- x_web / n_web

# Paso 1
cat("Paso 1) Hipotesis:\n")
cat("H0: p_movil - p_web = 0\n")
cat("H1: p_movil - p_web != 0\n\n")

# Paso 2
cat("Paso 2) Nivel de significancia:\n")
cat("alpha =", alpha4, "\n\n")

# Paso 3: Z dos proporciones (pooled)
p_pool <- (x_movil + x_web) / (n_movil + n_web)
se4 <- sqrt(p_pool * (1 - p_pool) * (1 / n_movil + 1 / n_web))
z4 <- (p_movil - p_web) / se4
p4 <- 2 * (1 - pnorm(abs(z4)))
zcrit4 <- qnorm(1 - alpha4 / 2)

cat("Paso 3) Calculos:\n")
cat("p_movil =", round(p_movil, 4), "\n")
cat("p_web =", round(p_web, 4), "\n")
cat("z =", round(z4, 4), "\n")
cat("z critico bilateral =", round(zcrit4, 4), "\n")
cat("p-valor =", round(p4, 6), "\n\n")

# Paso 4
cat("Paso 4) Decision:\n")
cat(decision_text(p4, alpha4), "\n\n")

# Paso 5
cat("Paso 5) Conclusion:\n")
if (p4 < alpha4) {
  cat("Con 95% de confianza, SI existe diferencia significativa en la proporcion de usuarios satisfechos entre movil y web.\n\n")
} else {
  cat("Con 95% de confianza, NO existe diferencia significativa en la proporcion de usuarios satisfechos entre movil y web.\n\n")
}

# =========================================================
# PROBLEMA 5
# =========================================================
cat("PROBLEMA 5\n")
cat("Contexto: Menor varianza en consumo de memoria (microservicio)\n\n")

# Datos
sigma0_sq <- 16   # varianza historica version anterior
n5 <- 20
s5_sq <- 9
alpha5 <- 0.01
gl5 <- n5 - 1

# Paso 1
cat("Paso 1) Hipotesis:\n")
cat("H0: sigma^2 >= 16\n")
cat("H1: sigma^2 < 16\n\n")

# Paso 2
cat("Paso 2) Nivel de significancia:\n")
cat("alpha =", alpha5, "\n\n")

# Paso 3: Chi-cuadrada una varianza (cola izquierda)
chi5 <- (gl5 * s5_sq) / sigma0_sq
chi_crit5 <- qchisq(alpha5, df = gl5)
p5 <- pchisq(chi5, df = gl5)

cat("Paso 3) Calculos:\n")
cat("Chi-cuadrada =", round(chi5, 4), "\n")
cat("Chi critico (cola izq.) =", round(chi_crit5, 4), "\n")
cat("p-valor =", round(p5, 6), "\n\n")

# Paso 4
cat("Paso 4) Decision:\n")
cat(decision_text(p5, alpha5), "\n\n")

# Paso 5
cat("Paso 5) Conclusion:\n")
if (p5 < alpha5) {
  cat("Respuesta: Si. Con 99% de confianza, se puede afirmar que la nueva version tiene menor varianza en el consumo de memoria.\n\n")
} else {
  cat("Respuesta: No. Con 99% de confianza, no se puede afirmar que la nueva version tenga menor varianza en el consumo de memoria.\n\n")
}

# =========================================================
# RESUMEN FINAL
# =========================================================
cat("========================================\n")
cat("RESUMEN EJECUTIVO\n")
cat("========================================\n")
cat("1) API < 250 ms: ", decision_text(p1, alpha1), "\n")
cat("2) Fallos CI/CD disminuyen: ", decision_text(p2, alpha2), "\n")
cat("3) PostgreSQL mas rapido que MongoDB: ", decision_text(p3, alpha3), "\n")
cat("4) Diferencia de satisfaccion movil vs web: ", decision_text(p4, alpha4), "\n")
cat("5) Menor varianza de memoria en nueva version: ", decision_text(p5, alpha5), "\n")
