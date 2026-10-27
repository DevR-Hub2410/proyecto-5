# fig_efecto_n.R
# Dibuja el margen de error (95%) en funcion del tamano de muestra.
# swirl ejecuta este script, por eso dibuja directamente (sin funcion).
ns <- seq(10, 200, by = 5)
margenes <- z_95 * sd_muestral / sqrt(ns)

plot(ns, margenes, type = "l", lwd = 2, col = "steelblue",
     xlab = "Tamano de muestra (n)",
     ylab = "Margen de error (ms)",
     main = "Efecto del tamano de muestra en el margen de error")

puntos_n <- c(20, 40, 80, 160)
points(puntos_n, z_95 * sd_muestral / sqrt(puntos_n), pch = 19, col = "darkred")
text(puntos_n, z_95 * sd_muestral / sqrt(puntos_n), labels = paste0("n=", puntos_n),
     pos = 3, cex = 0.8)