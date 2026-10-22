# fig_tcl.R
# Muestra como se distribuyen las medias muestrales (Teorema del Limite Central)

fig_tcl <- function(){
  hist(medias_muestrales,
       breaks = 30,
       col = "steelblue",
       border = "white",
       main = "Distribucion de 1000 medias muestrales (n = 30)",
       xlab = "Media muestral de calificaciones")
  abline(v = mu_poblacion, col = "red", lwd = 2, lty = 2)
}

fig_tcl()
