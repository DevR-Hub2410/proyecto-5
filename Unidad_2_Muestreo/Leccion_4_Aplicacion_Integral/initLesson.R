# initLesson.R - Actividad integradora de Muestreo
# Poblacion simulada: 800 usuarios de una app.
# La satisfaccion DEPENDE del plan (Premium mas satisfecho que Gratuito),
# de modo que estratificar por plan tenga una ventaja real y medible.

# Reinicia el contador de intentos usado por las pistas progresivas (customTests.R)
options(intentos_swirl = list())

set.seed(123)

N <- 800

usuarios_app <- data.frame(
  id     = 1:N,
  region = sample(c("Norte", "Centro", "Sur"), N, replace = TRUE),
  # Proporciones exactas: 560 Gratuito (70%) y 240 Premium (30%)
  plan   = sample(rep(c("Gratuito", "Premium"), times = c(560, 240))),
  stringsAsFactors = FALSE
)

# Satisfaccion en escala 1 a 10: Premium (media 8.5) y Gratuito (media 5.5), sd = 1.2
usuarios_app$satisfaccion <- round(
  pmin(10, pmax(1, rnorm(N,
                         mean = ifelse(usuarios_app$plan == "Premium", 8.5, 5.5),
                         sd   = 1.2))),
  1
)

# Parametros de la actividad
n                 <- 40     # tamano de cada muestra
num_muestras      <- 500    # repeticiones para la distribucion muestral
media_poblacional <- mean(usuarios_app$satisfaccion)
sd_poblacional    <- sd(usuarios_app$satisfaccion)