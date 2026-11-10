alumno <- factor(rep(1:4, each = 3))

asignatura <- factor(rep(c("A", "B", "C"), times = 4))

calificacion <- c(
  14, 13, 19,
  12, 14, 17,
  14, 10, 16,
  10, 14, 19
)

datos <- data.frame(alumno, asignatura, calificacion)

datos