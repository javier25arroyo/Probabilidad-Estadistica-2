# Problema 2
sample_mean <- 49.8
sample_sd <- 1.2
sample_size <- 40
confidence_level <- 0.90

alpha <- 1 - confidence_level
t_crit <- qt(1 - alpha/2, df = sample_size - 1)
margin_of_error <- t_crit * sample_sd / sqrt(sample_size)

confidence_interval_lower <- sample_mean - margin_of_error
confidence_interval_upper <- sample_mean + margin_of_error

cat("Problema 2\n")
cat("Valor crítico t:", t_crit, "\n")
cat("Margen de error:", margin_of_error, "\n")
cat("IC del 90% para la media: [", confidence_interval_lower, ",", confidence_interval_upper, "]\n\n")

