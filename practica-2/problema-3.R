# Problema 3
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
cat("Valor crítico z:", z_crit, "\n")
cat("Error estándar:", SE, "\n")
cat("Margen de error:", E, "\n")
cat("IC del 95% para la proporción: [", LI, ",", LS, "]\n\n")