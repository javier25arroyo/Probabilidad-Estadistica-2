# Problema 4
sigma <- 60
E <- 30
conf <- 0.99

alpha <- 1 - conf
z_crit <- qnorm(1 - alpha/2)

n_calc <- (z_crit * sigma / E)^2
n_min <- ceiling(n_calc)

cat("Problema 4\n")
cat("Valor crítico z:", z_crit, "\n")
cat("Tamaño calculado:", n_calc, "\n")
cat("Tamaño mínimo de muestra:", n_min, "\n\n")