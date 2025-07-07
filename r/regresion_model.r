# datos <- read.table("micro_milagro.csv", sep = ",", header = TRUE)

# Y ~ X
modelo <- lm(nota_estadistica ~ nota_algebra, data=df_new)
output_file <- "/home/mai_lavender/Escritorio/estadisticas/estadisticaG2/RL_estadistica&algebra.txt"
sink(output_file)
cat("========== REGRESION LINEAL Y=NOTA ESTADISTICA - X=NOTA FUNDAMENTOS DE PROG ==========\n\n")
cat("=== COEFFICIENTS ===\n")
print(coefficients(modelo))
cat("\n\n=== SUMMARY ===\n")
print(summary(modelo))
sink()
diagnorm(modelo)



