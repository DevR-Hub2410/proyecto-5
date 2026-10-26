#===============================================================================
#                     Muestra aleatoria con sample()
#===============================================================================

test_muestra_aleatoria <- function() {
  e <- get("e", parent.frame())
  v <- e$val
  
  # Revisa si el resultado se guardó en la variable muestra
  es_asignacion <- is.call(e$expr) &&
    (identical(e$expr[[1]], as.name("<-")) ||
       identical(e$expr[[1]], as.name("="))) &&
    identical(e$expr[[2]], as.name("muestra"))
  
  if (!is.numeric(v)) {
    message("Utiliza sample() para elegir 300 números entre 1 y 1000.")
    return(FALSE)
  }
  
  # Respuesta correcta
  if (es_asignacion && length(v) == 300 &&
      all(v >= 1 & v <= 1000) && !any(duplicated(v))) {
    return(TRUE)
  }
  
  # Error 1: cantidad incorrecta de elementos
  if (length(v) != 300) {
    message("La muestra debe tener 300 elementos. Revisa el segundo argumento de sample().")
    return(FALSE)
  }
  
  # Error 2: números fuera del rango de la población 
  if (any(v < 1 | v > 1000)) {
    message("Los programadores están numerados del 1 al 1000. Revisa el rango que escribiste.")
    return(FALSE)
  }
  
  # Error 3: no guardó el resultado en muestra
  if (!es_asignacion) {
    message("La selección es correcta, pero no la guardaste en la variable muestra. Utiliza <- para asignar el resultado.")
    return(FALSE)
  }
  
  message("Revisa la instrucción. Debes elegir 300 números distintos entre 1 y 1000.")
  return(FALSE)
}

#===============================================================================
#                          Media y mediana juntas
#===============================================================================

test_media_mediana <- function() {
  e <- get("e", parent.frame())
  txt <- gsub("\\s+", "", paste(deparse(e$expr), collapse = ""))
  
  # Respuesta correcta
  if (is.numeric(e$val) && length(e$val) == 2 &&
      isTRUE(all.equal(as.numeric(e$val), c(7.6, 7)))) {
    return(TRUE)
  }
  
  # Error 1: calculó solo una función, sin c()
  if (is.call(e$expr) && deparse(e$expr[[1]]) %in% c("mean", "median")) {
    message("Casi... Solo calculaste un valor. Junta la media y la mediana dentro de c().")
    return(FALSE)
  }
  
  # Error 2: le falta mean() o median()
  if (!grepl("mean(", txt, fixed = TRUE) || !grepl("median(", txt, fixed = TRUE)) {
    message("Necesitas las dos funciones dentro de c(), mean() y median().")
    return(FALSE)
  }
  
  # Error 3: puso la mediana antes que la media
  if (is.numeric(e$val) && length(e$val) == 2 &&
      isTRUE(all.equal(as.numeric(e$val), c(7, 7.6)))) {
    message("Los valores están al revés. Pon primero la media y después la mediana.")
    return(FALSE)
  }
  
  # Error 4: cualquier otro caso
  message("Revisa que estés usando el vector notas.")
  return(FALSE)
}

#===============================================================================
#             Variable numérica (media, proporción o intervalo)
#   Se reutiliza en varias preguntas. nombre es la variable que debe crear el
#   estudiante y correcto es el valor (o vector) que debe contener.
#===============================================================================

test_variable_numerica <- function(nombre, correcto, tolerancia = 1e-4) {
  e <- get("e", parent.frame())
  
  # Revisa si el estudiante asignó el resultado a la variable pedida
  es_asignacion <- is.call(e$expr) &&
    (identical(e$expr[[1]], as.name("<-")) ||
       identical(e$expr[[1]], as.name("="))) &&
    identical(e$expr[[2]], as.name(nombre))
  
  # Error 1: dejó el resultado completo de t.test() o prop.test()
  if (inherits(e$val, "htest")) {
    message("La función devuelve varios resultados. Agrega $conf.int al final para quedarte solo con el intervalo de confianza.")
    return(FALSE)
  }
  
  # Revisa si el valor obtenido coincide con el correcto 
  valor_correcto <- is.numeric(e$val) &&
    length(e$val) == length(correcto) &&
    isTRUE(all.equal(as.numeric(e$val), correcto, tolerance = tolerancia))
  
  # Respuesta correcta 
  if (es_asignacion && valor_correcto) {
    return(TRUE)
  }
  
  # Error 2: el valor es correcto, pero no lo guardó en la variable pedida
  if (!es_asignacion && valor_correcto) {
    message("El resultado es correcto, pero no lo guardaste en la variable ", nombre, ". Utiliza <- para asignar el resultado.")
    return(FALSE)
  }
  
  # Error 3: guardó algo en la variable, pero el valor no coincide 
  if (es_asignacion) {
    message("Guardaste el resultado en ", nombre, ", pero el valor no coincide. ", "Revisa las funciones y los vectores que utilizaste.")
    return(FALSE)
  }
  
  # Cualquier otro error 
  message("Calcula el resultado y guárdalo en la variable ", nombre, " utilizando <-.")
  return(FALSE)
}

#===============================================================================
#                  Intervalo de confianza para una media
#===============================================================================

test_ic_media <- function() {
  e <- get("e", parent.frame())
  txt <- gsub("\\s+", "", paste(deparse(e$expr), collapse = ""))
  
  # Respuesta correcta
  if (is.numeric(e$val) && length(e$val) == 2 && 
      isTRUE(all.equal(as.numeric(e$val), c(36.08, 43.92), tolerance = 1e-6))) {
    return(TRUE)
  }
  
  # Error 1: obtuvo un solo límite (sin c(-1,1))
  if (is.numeric(e$val) && length(e$val) == 1) {
    message("Casi... Solo obtuviste un límite. Usa c(-1,1) para calcular el inferior y el superior a la vez.")
    return(FALSE)
  }
  
  # Error 2: olvidó la raíz cuadrada
  if (!grepl("sqrt(", txt, fixed = TRUE)) {
    message("Falta la raíz cuadrada del tamaño de muestra, s / sqrt(n).")
    return(FALSE)
  }
  
  # Error 3: datos u orden de operaciones incorrectos
  message("Revisa tus datos, media = 40, Z = 1.96, s = 12 y n = 36. Cuida también el orden de la operación.")
  return(FALSE)
}

#===============================================================================
#                     Intervalo exacto con binom.test
#===============================================================================

test_ic_binom <- function() {
  e <- get("e", parent.frame())
  txt <- gsub("\\s+", "", paste(deparse(e$expr), collapse = ""))
  
  # Respuesta correcta
  if (is.numeric(e$val) && length(e$val) == 2 &&
      isTRUE(all.equal(as.numeric(e$val), c(0.02521073, 0.55609546), tolerance = 1e-6))) {
    return(TRUE)
  }
  
  # Error 1: escribió binom.test(2, 10) sin $conf.int
  if (identical(e$expr, quote(binom.test(2, 10))) || inherits(e$val, "htest")) {
    message("Ya casi. Agrega $conf.int al final para ver solo el intervalo.")
    return(FALSE)
  }
  
  # Error 2: no usó binom.test
  if (!grepl("binom.test(", txt, fixed = TRUE)) {
    message("Usa la función binom.test().")
    return(FALSE)
  }
  
  # Error 3: argumentos incorrectos 
  message("Revisa los argumentos, binom.test(éxitos, total). Aquí son 2 módulos con errores críticos de 10 revisados.")
  return(FALSE)
}

#===============================================================================
#       Intervalo para la media de calificaciones (actividad integradora)
#===============================================================================

test_ic_calificaciones <- function() {
  e <- get("e", parent.frame())
  datos <- get("calificaciones", envir = globalenv())
  correcto <- as.numeric(t.test(datos, conf.level = 0.95)$conf.int)
  
  # Revisa si el estudiante asignó el resultado a la variable pedida
  es_asignacion <- is.call(e$expr) &&
    (identical(e$expr[[1]], as.name("<-")) ||
       identical(e$expr[[1]], as.name("="))) &&
    identical(e$expr[[2]], as.name("intervalo_calificaciones"))
  
  # Error 1: dejó el resultado completo de t.test()
  if (inherits(e$val, "htest")) {
    message("t.test() devuelve varios resultados. Agrega $conf.int al final para quedarte solo con el intervalo.")
    return(FALSE)
  }

  valor_correcto <- is.numeric(e$val) && length(e$val) == 2 &&
    isTRUE(all.equal(as.numeric(e$val), correcto, tolerance = 1e-4))
  
  # Respuesta correcta 
  if (es_asignacion && valor_correcto) {
    return(TRUE)
  }
  
  # Error 2: el intervalo es correcto, pero no lo guardó
  if (!es_asignacion && valor_correcto) {
    message("El intervalo es correcto, pero no lo guardaste en la variable intervalo_calificaciones. Utiliza <- para asignar el resultado.")
    return(FALSE)
  }
  
  # Error 3: guardó algo, pero el valor no coincide 
  if (es_asignacion) {
    message("Guardaste el resultado, pero el valor no coincide. Revisa que uses t.test() con calificaciones y conf.level = 0.95.")
    return(FALSE)
  }
  
  message("Usa t.test() con calificaciones, agrega $conf.int y guarda el resultado en intervalo_calificaciones con <-.")
  return(FALSE)
}

#===============================================================================
#       Intervalo para la proporción de modo oscuro (actividad integradora)
#   El valor correcto se calcula con los datos del vector modo_oscuro.
#===============================================================================

test_is_oscuro <- function() {
  e <- get("e", parent.frame())
  datos <- get("modo_oscuro", envir = globalenv())
  correcto <- as.numeric(prop.test(sum(datos == "Si"), length(datos), conf.level = 0.95)$conf.int)
  
  # Revisa si el estudiante asignó el resultado a la variable pedida
  es_asignacion <- is.call(e$expr) &&
    (identical(e$expr[[1]], as.name("<-")) ||
       identical(e$expr[[1]], as.name("="))) &&
    identical(e$expr[[2]], as.name("intervalo_oscuro"))
  
  # Error 1: dejó el resultado completo de prop.test()
  if (inherits(e$val, "htest")) {
    message("prop.test() devuelve varios resultados. Agrega $conf.int al final para quedarte solo con el intervalo.")
    return(FALSE)
  }
  
  valor_correcto <- is.numeric(e$val) && length(e$val) == 2 &&
    isTRUE(all.equal(as.numeric(e$val), correcto, tolerance = 1e-4))
  
  # Respuesta correcta 
  if (es_asignacion && valor_correcto) {
    return(FALSE)
  }
  
  # Error 2: el intervalo es correcto, pero no lo guardó
  if (!es_asignacion && valor_correcto) {
    message("El intervalo es correcto, pero no lo guardaste en la variable intervalo_oscuro. Utiliza <- para asignar el resultado.")
    return(FALSE)
  }
  
  # Error 3: guardó algo, pero el valor no coincide 
  if (es_asignacion) {
    message("Guardaste el resultado, pero el valor no coincide. Revisa que el número de éxitos sea sum(modo_oscuro == \"Si\") y el total sea length(modo_oscuro).")
    return(FALSE)
  }
  
  message("Usa prop.test() con el número de usuarios que dicen \"Si\" y el total, agrega $conf.int y guarda el resultado en intervalo_oscuro con <-.")
  return(FALSE)
  
}

