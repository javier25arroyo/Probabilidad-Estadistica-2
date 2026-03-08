# Problema 5
n <- 12
s <- 3.2
gl <- n - 1
conf <- 0.95
alpha <- 1 - conf

chi_sup <- qchisq(1 - alpha/2, df = gl)
chi_inf <- qchisq(alpha/2, df = gl)

LI_var <- (gl * s^2) / chi_sup
LS_var <- (gl * s^2) / chi_inf

cat("Problema 5\n")
cat("Chi-cuadrado superior:", chi_sup, "\n")
cat("Chi-cuadrado inferior:", chi_inf, "\n")
cat("IC del 95% para la varianza: [", LI_var, ",", LS_var, "]\n")

LI_sd <- sqrt(LI_var)
LS_sd <- sqrt(LS_var)

cat("IC equivalente para la desviación estándar: [", LI_sd, ",", LS_sd, "]\n\n")