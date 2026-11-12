tiempos_A <- c(2.4, 2.7, 2.2, 2.9, 2.5, 2.6, 2.3, 2.8, 2.5, 2.7, 2.4, 2.6)
tiempos_B <- c(2.1, 2.3, 1.9, 2.2, 2.0, 2.4, 2.1, 2.2, 1.8, 2.3, 2.0, 2.2)

# Usuarios que completaron una tarea en cada version
exitos_A <- 72
n_A <- 120
exitos_B <- 96
n_B <- 120

set.seed(123)

# --- Datos para la diferencia de medias y la razon de varianzas ---
grupoA <- rnorm(30, mean = 50, sd = 6)
grupoB <- rnorm(30, mean = 47, sd = 8)

# --- Datos para muestras pareadas (dependientes) -----------------
antes <- rnorm(20, mean = 55, sd = 5)
despues <- antes - rnorm(20, mean = 3, sd = 2)

# --- Datos para la diferencia de proporciones -------------------
exito_A <- 138
total_A <- 200
exito_B <- 121
total_B <- 200