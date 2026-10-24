#===============================================================================
#                     Muestra aleatoria con sample()
#===============================================================================

test_muestra_aleatoria <- function() {
  e <- get("e", parent.frame())
  v <- e%val
  
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



