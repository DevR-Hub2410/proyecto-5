# instalar_curso.R
# Instala el curso "Curso_Estadistica" en swirl a partir de las carpetas de este repositorio.
#
# Como usarlo:
#   1. Abre el proyecto proyecto-5.Rproj en RStudio (asi el directorio de trabajo es la raiz del repositorio).
#   2. Ejecuta en la consola:  source("instalar_curso.R")
#   3. Ejecuta:  library(swirl); swirl()   y elige el curso Curso_Estadistica.
#
# Por que existe este archivo: swirl solo muestra como lecciones las carpetas de primer nivel de un curso.
# Este repositorio esta organizado por unidades, asi que el script arma una copia plana del curso, con
# todas las lecciones en orden, y la instala.

if (!requireNamespace("swirl", quietly = TRUE)) install.packages("swirl")

curso <- "Curso_Estadistica"

# Orden del curso. Nombre con el que aparece en swirl = carpeta dentro del repositorio
lecciones <- c(
  "Conociendo_R_y_RStudio"                           = "Conociendo_R_Y_RStudio",
  "Unidad_1_Leccion_1_Datos_y_variables"             = "Unidad_1_Estadistica_Descriptiva/Leccion_1_Datos_y_variables",
  "Unidad_1_Leccion_2_Analisis_de_datos"             = "Unidad_1_Estadistica_Descriptiva/Leccion_2_Analisis_de_datos",
  "Unidad_1_Leccion_3_Dispersion_de_datos"           = "Unidad_1_Estadistica_Descriptiva/Leccion_3_Dispersion_de_datos",
  "Unidad_2_Leccion_1_Introduccion_al_muestreo"      = "Unidad_2_Muestreo/Leccion_1_Introduccion_al_muestreo",
  "Unidad_2_Leccion_2_Metodos_de_muestreo"           = "Unidad_2_Muestreo/Leccion_2_Metodos_de_muestreo",
  "Unidad_2_Leccion_3_Distribuciones_Muestrales"     = "Unidad_2_Muestreo/Leccion_3_Distribuciones_Muestrales",
  "Unidad_2_Leccion_4_Aplicacion_Integral"           = "Unidad_2_Muestreo/Leccion_4_Aplicacion_Integral",
  "Unidad_3_Leccion_1_Estimacion_Puntual_Intervalos" = "Unidad_3_Estimacion/Leccion_1_Estimacion_Puntual_Intervalos",
  "Unidad_3_Leccion_2_Confianza_Precision_Tamano"    = "Unidad_3_Estimacion/Leccion_2_Confianza_Precision_Tamano",
  "Unidad_3_Leccion_3_Estimacion_Dos_Poblaciones"    = "Unidad_3_Estimacion/Leccion_3_Estimacion_Dos_Poblaciones"
)

# Comprobacion: el directorio de trabajo debe ser la raiz del repositorio
if (!file.exists(file.path("Conociendo_R_Y_RStudio", "lesson.yaml"))) {
  stop("Abre proyecto-5.Rproj o usa setwd() para ir a la carpeta raiz del repositorio y vuelve a ejecutar.")
}

# Copia plana del curso en una carpeta temporal
tmp <- file.path(tempdir(), curso)
if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
dir.create(tmp)

for (nombre in names(lecciones)) {
  origen <- lecciones[[nombre]]
  if (!file.exists(file.path(origen, "lesson.yaml"))) {
    stop("No se encontro la leccion: ", origen)
  }
  destino <- file.path(tmp, nombre)
  dir.create(destino)
  archivos <- list.files(origen, full.names = TRUE)
  if (!all(file.copy(archivos, destino, recursive = TRUE))) {
    stop("No se pudo copiar la leccion: ", origen)
  }
}

# MANIFEST con el orden del curso
writeLines(names(lecciones), file.path(tmp, "MANIFEST"))

# Reemplaza una version anterior si existe
try(swirl::uninstall_course(curso), silent = TRUE)
swirl::install_course_directory(tmp)

message("Listo. Ejecuta library(swirl); swirl() y elige el curso ", curso, ".")