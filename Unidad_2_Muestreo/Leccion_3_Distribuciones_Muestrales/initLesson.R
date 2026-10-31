# initLesson.R DISTRIBUCION MUESTRAL DE LA MEDIA

# Creamos un vector fijo con 10 tiempos de respuesta de una app, en segundos.
# Son valores fijos, no generados al azar, para que los ejercicios de
# muestreo y simulacion trabajen siempre sobre la misma base de datos.
tiempo <- c(2.1, 3.4, 2.8, 1.9, 2.5, 3.0, 2.2, 2.6, 3.1, 2.4)


# initLesson.R DISTRIBUCION MUESTRAL DE LA PROPORCION

set.seed(123) # Semilla para generar numeros aleatorios
N <- 10000 # Poblacion
p_real <- 0.65 # Proporcion poblacional
poblacion <- sample(c("Si", "No"), size = N, replace = TRUE, # Crea la poblacion
                    prob = c(p_real, 1 - p_real))
n <- 30 # Tamano de muestra