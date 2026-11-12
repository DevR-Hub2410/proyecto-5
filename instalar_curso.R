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
  "Conociendo_R_y_RStudio"         = "Conociendo_R_Y_RStudio",
  "Datos_Y_Variables"              = "Estadistica_Descriptiva/Datos_Y_Variables",
  "Analisis_De_Datos"              = "Estadistica_Descriptiva/Analisis_De_Datos",
  "Dispersion_De_Datos"            = "Estadistica_Descriptiva/Dispersion_De_Datos",
  "Introduccion_Al_Muestreo"       = "Muestreo/Introduccion_Al_Muestreo",
  "Metodos_De_Muestreo"            = "Muestreo/Metodos_De_Muestreo",
  "Distribuciones_Muestrales"      = "Muestreo/Distribuciones_Muestrales",
  "Aplicacion_Integral"            = "Muestreo/Aplicacion_Integral",
  "Estimacion_Puntual_Intervalos"  = "Estimacion/Estimacion_Puntual_Intervalos",
  "Confianza_Precision_Tamano"     = "Estimacion/Confianza_Precision_Tamano",
  "Estimacion_Dos_Poblaciones"     = "Estimacion/Estimacion_Dos_Poblaciones",
  "Pruebas_De_Hipotesis"           = "Pruebas_De_Hipotesis",
  "Bondad_Ajuste_Distribuciones_De_Probabilidad"    = "Bondad_De_Ajuste_E_Independencia/Bondad_Ajuste_Distribuciones_De_Probabilidad",
  "Introduccion_Bondad_Ajuste"     = "Bondad_De_Ajuste_E_Independencia/Introduccion_Bondad_Ajuste",
  "Correlacion_Y_Regresion_Lineal" = "Correlacion_Y_Regresion_Lineal",
  "Comparaciones_Multiples"        = "ANOVA_De_Un_Factor/Comparaciones_Multiples",
  "Introduccion_ANOVA_De_Un_Fator" = "ANOVA_De_Un_Factor/Introduccion_ANOVA_De_Un_Factor",
  "Supuestos_Del_ANOVA"            = "ANOVA_De_Un_Factor/Supuestos_Del_ANOVA",
  "Tabla_ANOVA"                    = "ANOVA_De_Un_Factor/Tabla_ANOVA"
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