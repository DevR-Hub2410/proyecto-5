# initLesson.R - Pruebas de Hipotesis
# ------------------------------------------------------------------

# Poblacion para la prueba de hipotesis sobre una media:
# tiempo de respuesta (en segundos) de 3000 solicitudes de un servidor.
# El equipo de TI afirma que el tiempo promedio es de 2 segundos o menos,
# pero la poblacion real (que simulamos para poder comprobarlo) tiene
# una media de 2.3 segundos.
set.seed(55)
tiempos_respuesta <- round(rnorm(3000, mean = 2.3, sd = 0.5), 2)
N <- length(tiempos_respuesta)

# Tamano de muestra y valor hipotetico (afirmacion de la empresa)
n <- 40
mu0 <- 2

# Muestra fija que el estudiante usara a lo largo de la leccion
set.seed(77)
muestra_tiempos <- sample(tiempos_respuesta, n)

# Poblacion para la prueba de hipotesis sobre una proporcion:
# usuarios de una app, con su plan (Gratuito o Premium).
# Misma semilla y parametros que en Aplicacion_integral, para que todo
# el curso use la misma poblacion de usuarios_app.
set.seed(123)
usuarios_app <- data.frame(
  id = 1:800,
  region = sample(c("Norte", "Centro", "Sur"), 800, replace = TRUE),
  plan = sample(c("Gratuito", "Premium"), 800, replace = TRUE, prob = c(0.7, 0.3)),
  satisfaccion = round(rnorm(800, mean = 7, sd = 1.5), 1)
)

# Proporcion afirmada por la empresa (hipotesis a probar)
p0 <- 0.72