carrera <- c(
  rep("Software", 60),
  rep("Industrial", 60),
  rep("Administracion", 60)
)

recurso_estudio <- c(
  rep("Videos", 30),
  rep("Lecturas", 18),
  rep("Practicos", 12),
  
  rep("Videos", 20),
  rep("Lecturas", 22),
  rep("Practicos", 18),
  
  rep("Videos", 15),
  rep("Lecturas", 20),
  rep("Practicos", 25)
)

datos_estudiantes <- data.frame(
  carrera,
  recurso_estudio
)

datos_estudiantes$carrera <- factor(
  datos_estudiantes$carrera,
  levels = c("Software", "Industrial", "Administracion")
)

datos_estudiantes$recurso_estudio <- factor(
  datos_estudiantes$recurso_estudio,
  levels = c("Videos", "Lecturas", "Practicos")
)

n_estudiantes <- nrow(datos_estudiantes)