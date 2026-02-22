# ==========================================
# PRÁCTICA 1 - PROBABILIDAD Y ESTADÍSTICA 2
# ==========================================

# Configurar dispositivo gráfico para guardar en archivo
png("graficos_practica1.png", width=1200, height=1200, res=120)
par(mfrow=c(2,2)) # Para ver varios gráficos en la misma ventana

# --- PROBLEMA A: GAMMA ---
cat("--- Problema A: Gamma ---\n")
prob_a1 <- pgamma(9, shape = 10, rate = 3)
prob_a2 <- pgamma(1, shape = 1, rate = 3) - pgamma(0.5, shape = 1, rate = 3)
perc_a3 <- qgamma(0.90, shape = 2, rate = 3)

cat("A1 (< 9 min):", prob_a1, "\nA2 (entre 0.5 y 1 min):", prob_a2, "\nA3 (Percentil 90):", perc_a3, "min\n\n")

# Gráfico para A1
curve(dgamma(x, shape=10, rate=3), from=0, to=10, main="A. Gamma(10,3): P(X < 9)", xlab="Minutos", ylab="Densidad")
x_poly <- seq(0, 9, length.out=100)
y_poly <- dgamma(x_poly, shape=10, rate=3)
polygon(c(0, x_poly, 9), c(0, y_poly, 0), col=rgb(0.2,0.4,0.8,0.5))

# --- PROBLEMA B: EXPONENCIAL ---
cat("--- Problema B: Exponencial ---\n")
prob_b1 <- pexp(2, rate = 0.25)
prob_b2 <- pexp(6, rate = 0.25) - pexp(3, rate = 0.25)
perc_b3 <- qexp(0.90, rate = 0.25)

cat("B1 (< 2 seg):", prob_b1, "\nB2 (entre 3 y 6 seg):", prob_b2, "\nB3 (Percentil 90):", perc_b3, "seg\n\n")

# Gráfico para B2
curve(dexp(x, rate=0.25), from=0, to=15, main="B. Exponencial(0.25): P(3 < X < 6)", xlab="Segundos", ylab="Densidad")
x_poly <- seq(3, 6, length.out=100)
y_poly <- dexp(x_poly, rate=0.25)
polygon(c(3, x_poly, 6), c(0, y_poly, 0), col=rgb(0.8,0.2,0.2,0.5))

# --- PROBLEMA C: BETA ---
cat("--- Problema C: Beta ---\n")
prob_c1 <- pbeta(0.60, 14, 6)
prob_c2 <- 1 - pbeta(0.90, 14, 6)
perc_c3 <- qbeta(0.90, 14, 6)

cat("C1 (< 60%):", prob_c1, "\nC2 (Fallo < 10% / Éxito > 90%):", prob_c2, "\nC3 (Percentil 90):", perc_c3, "\n\n")

# Gráfico para C1
curve(dbeta(x, 14, 6), from=0, to=1, main="C. Beta(14,6): P(X < 0.60)", xlab="Tasa de Éxito", ylab="Densidad")
x_poly <- seq(0, 0.60, length.out=100)
y_poly <- dbeta(x_poly, 14, 6)
polygon(c(0, x_poly, 0.60), c(0, y_poly, 0), col=rgb(0.2,0.8,0.2,0.5))

# --- PROBLEMA D: NORMAL ---
cat("--- Problema D: Normal ---\n")
prob_d1 <- pnorm(2.5, mean=2.8, sd=0.3)
prob_d2 <- pnorm(3.0, mean=2.8, sd=0.3) - pnorm(2.6, mean=2.8, sd=0.3)
perc_d3 <- qnorm(0.95, mean=2.8, sd=0.3)

cat("D1 (< 2.5 seg):", prob_d1, "\nD2 (entre 2.6 y 3.0 seg):", prob_d2, "\nD3 (Percentil 95):", perc_d3, "seg\n")

# Gráfico para D2
curve(dnorm(x, 2.8, 0.3), from=1.9, to=3.7, main="D. Normal(2.8, 0.3): P(2.6 < X < 3.0)", xlab="Segundos", ylab="Densidad")
x_poly <- seq(2.6, 3.0, length.out=100)
y_poly <- dnorm(x_poly, 2.8, 0.3)
polygon(c(2.6, x_poly, 3.0), c(0, y_poly, 0), col=rgb(0.8,0.6,0.2,0.5))

# Cerrar el dispositivo gráfico y resetear parámetros
dev.off()
cat("\n✓ Gráficos guardados en 'graficos_practica1.png'\n")