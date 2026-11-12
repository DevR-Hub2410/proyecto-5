# tabla_observadas.R
# Dibuja en la pestaña Plots la tabla de frecuencias observadas de los 60 lanzamientos.
# swirl ejecuta este script directamente. 

cara <- 1:6
observadas <- c(8, 12, 9, 11, 10, 10)

encabezado <- c("Cara del dado", "Frecuencia observada")
filas <- rbind(
  cbind(as.character(cara), as.character(observadas)),
  c("Total", as.character(sum(observadas)))
)

n_cols  <- length(encabezado)
n_filas <- nrow(filas) + 1          # filas de datos + encabezado
total   <- n_filas                  # la última fila es el total

par(mar = c(1, 1, 3, 1))
plot.new()
plot.window(xlim = c(0, n_cols), ylim = c(n_filas, 0))   # el eje y crece hacia abajo

# Fondos: encabezado y fila de total
rect(0, 0, n_cols, 1, col = "#D6E4F0", border = NA)
rect(0, total - 1, n_cols, total, col = "#EEEEEE", border = NA)

# Líneas de la tabla
segments(0, 0:n_filas, n_cols, 0:n_filas, col = "gray30")
segments(0:n_cols, 0, 0:n_cols, n_filas, col = "gray30")

# Texto
text((1:n_cols) - 0.5, 0.5, encabezado, font = 2, cex = 1.1)
for (i in seq_len(nrow(filas))) {
  texto_fila <- filas[i, ]
  es_total <- (i == nrow(filas))
  text((1:n_cols) - 0.5, i + 0.5, texto_fila,
       font = if (es_total) 2 else 1, cex = 1.1)
}

title(main = "Frecuencias observadas en 60 lanzamientos", cex.main = 1.1)