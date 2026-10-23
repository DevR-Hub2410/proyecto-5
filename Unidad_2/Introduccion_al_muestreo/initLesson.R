# initLesson.R
# Lección 1: Introducción al Muestreo
# Este archivo tiene como función preparar el entorno antes de que el estudiante empiece la práctica.

# Fijamos una semilla para que los resultados sean reproducibles.
# Cualquier persona que corra esta leccion va a obtener los mismos numeros.
set.seed(100)

# Creamos una poblacion simulada de usuarios de una app de software.
# id: identificador de cada usuario
# dispositivo: en que tipo de dispositivo usa la app
# tiempo_respuesta: cuantos segundos tarda la app en responder para ese usuario
usuarios <- data.frame(
  id = 1:200,
  dispositivo = sample(
    c("Movil", "Laptop", "Tablet"),
    200,
    replace = TRUE
  ),
  tiempo_respuesta = round(
    rnorm(200, mean = 2.5, sd = 0.6),
    2
  )
)

# Guardamos el tamano de la poblacion en un objeto aparte,
# para que el estudiante lo use sin tener que contar filas a mano.
N <- nrow(usuarios)