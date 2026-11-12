
# initLesson.R
# PRUEBAS DE INDEPENDENCIA
# ------------------------------------------------------------

# Semilla para garantizar resultados reproducibles
set.seed(123)

# Datos de 120 estudiantes según su modalidad de estudio
modalidad <- c(
  rep("Presencial", 60),
  rep("Virtual", 60)
)

# Resultados obtenidos en el examen
resultado <- c(
  rep("Aprobado", 42),
  rep("Reprobado", 18),
  rep("Aprobado", 30),
  rep("Reprobado", 30)
)

# Construcción de la base de datos
datos_estudiantes <- data.frame(
  modalidad,
  resultado
)

# Convertimos las variables en factores
datos_estudiantes$modalidad <- factor(
  datos_estudiantes$modalidad,
  levels = c("Presencial", "Virtual")
)

datos_estudiantes$resultado <- factor(
  datos_estudiantes$resultado,
  levels = c("Aprobado", "Reprobado")
)

# Tamaño de la muestra
n_estudiantes <- nrow(datos_estudiantes)
