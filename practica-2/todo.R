cat("PRACTICA: INTERVALOS DE CONFIANZA Y TAMANO DE MUESTRA\n\n")

# =========================================================
# Problema 1
# =========================================================
cat("Problema 1\n")
cat("a) El parametro poblacional es la proporcion poblacional p de estudiantes\n")
cat("   de nuevo ingreso en ingenieria con preparacion matematica adecuada.\n")
cat("b) El intervalo adecuado es un intervalo de confianza para una proporcion,\n")
cat("   porque la variable de interes es dicotomica: adecuada / no adecuada.\n\n")

# =========================================================
# Problema 2
# =========================================================
xbar <- 49.8
s <- 1.2
n <- 40
conf <- 0.90

alpha <- 1 - conf
t_crit <- qt(1 - alpha/2, df = n - 1)
E <- t_crit * s / sqrt(n)

LI <- xbar - E
LS <- xbar + E

cat("Problema 2\n")
cat("Valor critico t:", round(t_crit, 4), "\n")
cat("Margen de error:", round(E, 4), "\n")
cat("IC del 90% para la media: [", round(LI, 4), ",", round(LS, 4), "]\n")
cat("Interpretacion: se redujo el nivel de confianza para obtener un intervalo mas estrecho y mas preciso.\n\n")

# =========================================================
# Problema 3
# =========================================================
phat <- 0.64
n <- 250
conf <- 0.95

alpha <- 1 - conf
z_crit <- qnorm(1 - alpha/2)
SE <- sqrt(phat * (1 - phat) / n)
E <- z_crit * SE

LI <- phat - E
LS <- phat + E

cat("Problema 3\n")
cat("Valor critico z:", round(z_crit, 4), "\n")
cat("Error estandar:", round(SE, 4), "\n")
cat("Margen de error:", round(E, 4), "\n")
cat("IC del 95% para la proporcion: [", round(LI, 4), ",", round(LS, 4), "]\n")
cat("Interpretacion: al aumentar el nivel de confianza, el intervalo se vuelve mas ancho y menos preciso.\n\n")

# =========================================================
# Problema 4
# =========================================================
sigma <- 60
E <- 30
conf <- 0.99

alpha <- 1 - conf
z_crit <- qnorm(1 - alpha/2)

n_calc <- (z_crit * sigma / E)^2
n_min <- ceiling(n_calc)

cat("Problema 4\n")
cat("Valor critico z:", round(z_crit, 4), "\n")
cat("Tamano calculado:", round(n_calc, 4), "\n")
cat("Tamano minimo de muestra:", n_min, "\n")
cat("Interpretacion: se requieren al menos 27 observaciones para estimar la media con 99% de confianza y error maximo de +-30 kg.\n\n")

# =========================================================
# Problema 5
# =========================================================
n <- 12
s <- 3.2
gl <- n - 1
conf <- 0.95
alpha <- 1 - conf

chi_sup <- qchisq(1 - alpha/2, df = gl)
chi_inf <- qchisq(alpha/2, df = gl)

LI_var <- (gl * s^2) / chi_sup
LS_var <- (gl * s^2) / chi_inf

LI_sd <- sqrt(LI_var)
LS_sd <- sqrt(LS_var)

cat("Problema 5\n")
cat("Chi-cuadrado superior:", round(chi_sup, 4), "\n")
cat("Chi-cuadrado inferior:", round(chi_inf, 4), "\n")
cat("IC del 95% para la varianza: [", round(LI_var, 4), ",", round(LS_var, 4), "]\n")
cat("IC equivalente para la desviacion estandar: [", round(LI_sd, 4), ",", round(LS_sd, 4), "]\n")
cat("Interpretacion: con 95% de confianza, la varianza poblacional de los dias de recuperacion esta dentro de ese intervalo.\n")