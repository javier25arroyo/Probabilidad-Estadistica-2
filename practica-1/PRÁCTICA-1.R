# ==========================================
# PRACTICA 1 - 12 GRAFICOS EN R
# ==========================================

# Crear carpeta para guardar gráficos
dir.create("graficos_practica1", showWarnings = FALSE)

# ------------------------------------------
# FUNCIONES AUXILIARES
# ------------------------------------------

shade_left <- function(x, y, x_cut, col = "lightblue") {
  xs <- x[x <= x_cut]
  ys <- y[x <= x_cut]
  polygon(c(min(xs), xs, max(xs)), c(0, ys, 0), col = col, border = NA)
}

shade_between <- function(x, y, x1, x2, col = "lightblue") {
  xs <- x[x >= x1 & x <= x2]
  ys <- y[x >= x1 & x <= x2]
  polygon(c(min(xs), xs, max(xs)), c(0, ys, 0), col = col, border = NA)
}

shade_right <- function(x, y, x_cut, col = "lightblue") {
  xs <- x[x >= x_cut]
  ys <- y[x >= x_cut]
  polygon(c(min(xs), xs, max(xs)), c(0, ys, 0), col = col, border = NA)
}

# ==========================================
# PROBLEMA A - GAMMA
# ==========================================

# A1: P(X < 9), X ~ Gamma(10, 3)
png("graficos_practica1/A1_gamma_menor_9.png", width = 900, height = 600)
x <- seq(0, 10, length.out = 1000)
y <- dgamma(x, shape = 10, rate = 3)
plot(x, y, type = "l", lwd = 2, main = "Problema A(a): P(X < 9) - Gamma(10, 3)",
     xlab = "Tiempo (minutos)", ylab = "Densidad")
shade_left(x, y, 9)
abline(v = 9, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Área: P(X < 9)", "x = 9"),
       fill = c("lightblue", NA), border = c(NA, NA),
       lty = c(NA, 2), col = c(NA, "red"), bty = "n")
dev.off()

# A2: P(0.5 < X < 1), X ~ Gamma(1, 3)
png("graficos_practica1/A2_gamma_entre_05_y_1.png", width = 900, height = 600)
x <- seq(0, 3, length.out = 1000)
y <- dgamma(x, shape = 1, rate = 3)
plot(x, y, type = "l", lwd = 2, main = "Problema A(b): P(0.5 < X < 1) - Gamma(1, 3)",
     xlab = "Tiempo (minutos)", ylab = "Densidad")
shade_between(x, y, 0.5, 1)
abline(v = c(0.5, 1), col = "red", lwd = 2, lty = 2)
dev.off()

# A3: Percentil 90, X ~ Gamma(2, 3)
qA3 <- qgamma(0.90, shape = 2, rate = 3)
png("graficos_practica1/A3_gamma_percentil_90.png", width = 900, height = 600)
x <- seq(0, 3, length.out = 1000)
y <- dgamma(x, shape = 2, rate = 3)
plot(x, y, type = "l", lwd = 2, main = "Problema A(c): Percentil 90 - Gamma(2, 3)",
     xlab = "Tiempo (minutos)", ylab = "Densidad")
shade_left(x, y, qA3)
abline(v = qA3, col = "blue", lwd = 2, lty = 2)
text(qA3, max(y)*0.8, labels = paste0("q = ", round(qA3, 3)), pos = 4)
dev.off()

# ==========================================
# PROBLEMA B - EXPONENCIAL
# ==========================================

# B1: P(X < 2), X ~ Exp(0.25)
png("graficos_practica1/B1_exp_menor_2.png", width = 900, height = 600)
x <- seq(0, 20, length.out = 1000)
y <- dexp(x, rate = 0.25)
plot(x, y, type = "l", lwd = 2, main = "Problema B(a): P(X < 2) - Exp(0.25)",
     xlab = "Tiempo (segundos)", ylab = "Densidad")
shade_left(x, y, 2)
abline(v = 2, col = "red", lwd = 2, lty = 2)
dev.off()

# B2: P(3 < X < 6)
png("graficos_practica1/B2_exp_entre_3_y_6.png", width = 900, height = 600)
x <- seq(0, 20, length.out = 1000)
y <- dexp(x, rate = 0.25)
plot(x, y, type = "l", lwd = 2, main = "Problema B(b): P(3 < X < 6) - Exp(0.25)",
     xlab = "Tiempo (segundos)", ylab = "Densidad")
shade_between(x, y, 3, 6)
abline(v = c(3, 6), col = "red", lwd = 2, lty = 2)
dev.off()

# B3: Percentil 90
qB3 <- qexp(0.90, rate = 0.25)
png("graficos_practica1/B3_exp_percentil_90.png", width = 900, height = 600)
x <- seq(0, 20, length.out = 1000)
y <- dexp(x, rate = 0.25)
plot(x, y, type = "l", lwd = 2, main = "Problema B(c): Percentil 90 - Exp(0.25)",
     xlab = "Tiempo (segundos)", ylab = "Densidad")
shade_left(x, y, qB3)
abline(v = qB3, col = "blue", lwd = 2, lty = 2)
text(qB3, max(y)*0.8, labels = paste0("q = ", round(qB3, 2)), pos = 4)
dev.off()

# ==========================================
# PROBLEMA C - BETA
# ==========================================

# C1: P(X < 0.60), X ~ Beta(14, 6)
png("graficos_practica1/C1_beta_menor_060.png", width = 900, height = 600)
x <- seq(0, 1, length.out = 1000)
y <- dbeta(x, shape1 = 14, shape2 = 6)
plot(x, y, type = "l", lwd = 2, main = "Problema C(a): P(X < 0.60) - Beta(14, 6)",
     xlab = "Tasa de éxito", ylab = "Densidad")
shade_left(x, y, 0.60)
abline(v = 0.60, col = "red", lwd = 2, lty = 2)
dev.off()

# C2: P(X > 0.90) equivalente a fallo < 0.10
png("graficos_practica1/C2_beta_exito_mayor_090.png", width = 900, height = 600)
x <- seq(0, 1, length.out = 1000)
y <- dbeta(x, shape1 = 14, shape2 = 6)
plot(x, y, type = "l", lwd = 2, main = "Problema C(b): P(X > 0.90) - Beta(14, 6)",
     xlab = "Tasa de éxito", ylab = "Densidad")
shade_right(x, y, 0.90)
abline(v = 0.90, col = "red", lwd = 2, lty = 2)
dev.off()

# C3: Percentil 90
qC3 <- qbeta(0.90, shape1 = 14, shape2 = 6)
png("graficos_practica1/C3_beta_percentil_90.png", width = 900, height = 600)
x <- seq(0, 1, length.out = 1000)
y <- dbeta(x, shape1 = 14, shape2 = 6)
plot(x, y, type = "l", lwd = 2, main = "Problema C(c): Percentil 90 - Beta(14, 6)",
     xlab = "Tasa de éxito", ylab = "Densidad")
shade_left(x, y, qC3)
abline(v = qC3, col = "blue", lwd = 2, lty = 2)
text(qC3, max(y)*0.8, labels = paste0("q = ", round(qC3, 4)), pos = 4)
dev.off()

# ==========================================
# PROBLEMA D - NORMAL
# ==========================================

# D1: P(X < 2.5), X ~ N(2.8, 0.3)
png("graficos_practica1/D1_norm_menor_25.png", width = 900, height = 600)
x <- seq(1.8, 3.8, length.out = 1000)
y <- dnorm(x, mean = 2.8, sd = 0.3)
plot(x, y, type = "l", lwd = 2, main = "Problema D(a): P(X < 2.5) - N(2.8, 0.3)",
     xlab = "Tiempo (segundos)", ylab = "Densidad")
shade_left(x, y, 2.5)
abline(v = 2.5, col = "red", lwd = 2, lty = 2)
dev.off()

# D2: P(2.6 < X < 3.0)
png("graficos_practica1/D2_norm_entre_26_y_30.png", width = 900, height = 600)
x <- seq(1.8, 3.8, length.out = 1000)
y <- dnorm(x, mean = 2.8, sd = 0.3)
plot(x, y, type = "l", lwd = 2, main = "Problema D(b): P(2.6 < X < 3.0) - N(2.8, 0.3)",
     xlab = "Tiempo (segundos)", ylab = "Densidad")
shade_between(x, y, 2.6, 3.0)
abline(v = c(2.6, 3.0), col = "red", lwd = 2, lty = 2)
dev.off()

# D3: Percentil 95
qD3 <- qnorm(0.95, mean = 2.8, sd = 0.3)
png("graficos_practica1/D3_norm_percentil_95.png", width = 900, height = 600)
x <- seq(1.8, 3.8, length.out = 1000)
y <- dnorm(x, mean = 2.8, sd = 0.3)
plot(x, y, type = "l", lwd = 2, main = "Problema D(c): Percentil 95 - N(2.8, 0.3)",
     xlab = "Tiempo (segundos)", ylab = "Densidad")
shade_left(x, y, qD3)
abline(v = qD3, col = "blue", lwd = 2, lty = 2)
text(qD3, max(y)*0.8, labels = paste0("q = ", round(qD3, 3)), pos = 4)
dev.off()

# ==========================================
# RESULTADOS NUMERICOS
# ==========================================

cat("Problema A(a):", pgamma(9, shape = 10, rate = 3), "\n")
cat("Problema A(b):", pgamma(1, shape = 1, rate = 3) - pgamma(0.5, shape = 1, rate = 3), "\n")
cat("Problema A(c):", qgamma(0.90, shape = 2, rate = 3), "\n\n")

cat("Problema B(a):", pexp(2, rate = 0.25), "\n")
cat("Problema B(b):", pexp(6, rate = 0.25) - pexp(3, rate = 0.25), "\n")
cat("Problema B(c):", qexp(0.90, rate = 0.25), "\n\n")

cat("Problema C(a):", pbeta(0.60, shape1 = 14, shape2 = 6), "\n")
cat("Problema C(b):", 1 - pbeta(0.90, shape1 = 14, shape2 = 6), "\n")
cat("Problema C(c):", qbeta(0.90, shape1 = 14, shape2 = 6), "\n\n")

cat("Problema D(a):", pnorm(2.5, mean = 2.8, sd = 0.3), "\n")
cat("Problema D(b):", pnorm(3.0, mean = 2.8, sd = 0.3) - pnorm(2.6, mean = 2.8, sd = 0.3), "\n")
cat("Problema D(c):", qnorm(0.95, mean = 2.8, sd = 0.3), "\n")