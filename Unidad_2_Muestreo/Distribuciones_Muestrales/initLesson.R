# initLesson.R
# Leccion: Distribuciones muestrales
# Poblacion simulada: calificaciones de 300 estudiantes (escala 0-100)

set.seed(55)
poblacion_calificaciones <- round(rnorm(300, mean = 78, sd = 10), 1)
poblacion_calificaciones <- pmin(pmax(poblacion_calificaciones, 0), 100)

mu_poblacion    <- mean(poblacion_calificaciones)
sigma_poblacion <- sd(poblacion_calificaciones)
n_poblacion     <- length(poblacion_calificaciones)

# 1000 medias muestrales con muestras de tamano 30, para ilustrar el TCL
set.seed(77)
medias_muestrales <- replicate(1000, mean(sample(poblacion_calificaciones, 30)))

# Ejemplo de proporcion: encuesta sobre aprobacion de un curso
p_poblacion <- 0.6
n_encuesta  <- 50