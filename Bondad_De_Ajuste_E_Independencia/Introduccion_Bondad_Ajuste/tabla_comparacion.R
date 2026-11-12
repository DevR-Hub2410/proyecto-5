# tabla_comparacion.R
# Dibuja en la pestaña Plots la comparación entre frecuencias observadas y esperadas.
# swirl ejecuta este script directamente. 

# Todo el código va dentro de local() para que sus variables (cara, total, filas, etc.)
# no pasen al espacio de trabajo del estudiante. 

local({
  cara <- 1:6
  observadas <- c(8, 12, 9, 11, 10, 10)
  esperadas  <- rep(60 * (1/6), 6)
  
  encabezado <- c("Cara", "Observada", "Esperada")
  filas <- rbind(
    cbind(as.character(cara), as.character(observadas), as.character(esperadas)),
    c("Total", as.character(sum(observadas)), as.character(sum(esperadas)))
  )
  
  n_cols  <- length(encabezado)
  n_filas <- nrow(filas) + 1
  total   <- n_filas
  
  par(mar = c(1, 1, 3, 1))
  plot.new()
  plot.window(xlim = c(0, n_cols), ylim = c(n_filas, 0))
  
  rect(0, 0, n_cols, 1, col = "#D6E4F0", border = NA)
  rect(0, total - 1, n_cols, total, col = "#EEEEEE", border = NA)
  
  segments(0, 0:n_filas, n_cols, 0:n_filas, col = "gray30")
  segments(0:n_cols, 0, 0:n_cols, n_filas, col = "gray30")
  
  text((1:n_cols) - 0.5, 0.5, encabezado, font = 2, cex = 1.1)
  for (i in seq_len(nrow(filas))) {
    es_total <- (i == nrow(filas))
    text((1:n_cols) - 0.5, i + 0.5, filas[i, ],
         font = if (es_total) 2 else 1, cex = 1.1)
  }
  
  title(main = "Frecuencias observadas y esperadas", cex.main = 1.1)
})