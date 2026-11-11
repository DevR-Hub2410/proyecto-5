#===============================================================================
# Pruebas personalizadas de la lección
# Leccion_1_Introduccion_Bondad_Ajuste
# swirl carga este archivo automáticamente desde la carpeta de la lección.
#===============================================================================

#-------------------------------------------------------------------------------
# Funciones auxiliares
#-------------------------------------------------------------------------------

# Comprueba que la respuesta sea una asignación a la variable solicitada y que
# el valor obtenido coincida con el esperado.
.es_asignacion <- function(e, nombre) {
  is.call(e$expr) &&
    (identical(e$expr[[1]], as.name("<-")) ||
       identical(e$expr[[1]], as.name("="))) &&
    identical(as.character(e$expr[[2]]), nombre)
}

.verifica <- function(e, nombre, esperado, tol = 1e-8, pista = NULL) {
  es_asignacion <- .es_asignacion(e, nombre)
  valor <- e$val
  
  valor_correcto <- is.numeric(valor) &&
    length(valor) == length(esperado) &&
    isTRUE(all.equal(
      as.numeric(valor), as.numeric(esperado),
      tolerance = tol,
      check.attributes = FALSE
    ))
  
  if (es_asignacion && valor_correcto) {
    return(TRUE)
  }
  
  if (!es_asignacion && valor_correcto) {
    message("El resultado es correcto, pero no lo guardaste en la variable ",
            nombre, ". Utiliza <- para asignarlo.")
    return(FALSE)
  }
  
  if (es_asignacion) {
    message("Guardaste un resultado en ", nombre,
            ", pero el valor no coincide. ", pista)
    return(FALSE)
  }
  
  message("Calcula el resultado y guárdalo en la variable ", nombre,
          " utilizando <-. ", pista)
  return(FALSE)
}

# Comprueba una respuesta numérica que no requiere asignarse a una variable.
.verifica_numero <- function(valor, esperado, tol = 1e-8) {
  is.numeric(valor) && length(valor) == 1 && is.finite(valor) &&
    abs(as.numeric(valor) - esperado) <= tol
}

# Comprueba que la expresión use chisq.test() con los dos objetos solicitados.
.llamada_chisq_correcta <- function(expr) {
  if (!.es_asignacion(expr, "prueba") || length(expr$expr) != 3) {
    return(FALSE)
  }
  
  llamada <- expr$expr[[3]]
  if (!is.call(llamada) || !identical(llamada[[1]], as.name("chisq.test"))) {
    return(FALSE)
  }
  
  argumentos <- as.list(llamada)[-1]
  nombres <- names(argumentos)
  if (is.null(nombres)) nombres <- rep("", length(argumentos))
  
  indice_x <- which(nombres == "x")
  if (length(indice_x) == 0 && length(argumentos) >= 1 && nombres[1] == "") {
    indice_x <- 1
  }
  indice_p <- which(nombres == "p")
  
  length(indice_x) == 1 && length(indice_p) == 1 &&
    identical(argumentos[[indice_x]], as.name("observados")) &&
    identical(argumentos[[indice_p]], as.name("probabilidades"))
}

#===============================================================================
# Vector de frecuencias observadas
#===============================================================================
test_observados <- function() {
  e <- get("e", parent.frame())
  valor <- e$val
  
  if (is.numeric(valor) && length(valor) != 6) {
    message("El vector debe contener exactamente seis frecuencias, una por cada cara del dado.")
    return(FALSE)
  }
  
  .verifica(
    e, "observados", c(8, 12, 9, 11, 10, 10), 1e-8,
    "Utiliza c() para reunir, en ese orden, las frecuencias 8, 12, 9, 11, 10 y 10."
  )
}

#===============================================================================
# Total de lanzamientos
#===============================================================================
test_total <- function() {
  e <- get("e", parent.frame())
  valor <- e$val
  
  if (is.numeric(valor) && length(valor) == 1 && is.finite(valor) &&
      !.verifica_numero(valor, 60)) {
    message("La suma no coincide con el total de lanzamientos. Recuerda sumar las seis frecuencias de observados con sum().")
    return(FALSE)
  }
  
  .verifica(e, "total", 60, 1e-8,
            "Utiliza sum(observados) para obtener el total de los 60 lanzamientos.")
}

#===============================================================================
# Probabilidades teóricas
#===============================================================================
test_probabilidades <- function() {
  e <- get("e", parent.frame())
  valor <- e$val
  
  if (is.numeric(valor) && length(valor) != 6) {
    message("Necesitas seis probabilidades, una para cada cara del dado. Repite 1/6 seis veces con rep().")
    return(FALSE)
  }
  
  .verifica(
    e, "probabilidades", rep(1/6, 6), 1e-8,
    "Como el dado tiene seis caras igualmente probables, utiliza rep(1/6, 6)."
  )
}

#===============================================================================
# Comprobación de la suma de probabilidades
#===============================================================================
test_suma_prob <- function() {
  e <- get("e", parent.frame())
  valor <- e$val
  
  if (!is.numeric(valor) || length(valor) != 1 || !is.finite(valor)) {
    message("La respuesta debe ser un solo valor numérico. Utiliza sum() sobre el vector probabilidades.")
    return(FALSE)
  }
  
  if (.verifica_numero(valor, 1)) {
    return(TRUE)
  }
  
  if (.verifica_numero(valor, 1/6)) {
    message("Obtuviste la probabilidad de una sola cara. Debes sumar las seis probabilidades con sum(probabilidades).")
    return(FALSE)
  }
  
  if (.verifica_numero(valor, 6)) {
    message("Obtuviste 6. Recuerda que cada probabilidad es 1/6; la suma de las seis debe ser 1, no 6.")
    return(FALSE)
  }
  
  message("La suma debe ser 1, porque las probabilidades de todos los resultados posibles forman una distribución completa. Revisa sum(probabilidades).")
  return(FALSE)
}

#===============================================================================
# Frecuencias esperadas
#===============================================================================
test_esperados <- function() {
  e <- get("e", parent.frame())
  valor <- e$val
  
  if (is.numeric(valor) && length(valor) == 1) {
    message("Casi. Necesitas un vector de seis frecuencias esperadas, no un solo valor. Multiplica total por todo el vector probabilidades.")
    return(FALSE)
  }
  
  if (is.numeric(valor) && length(valor) == 6 &&
      isTRUE(all.equal(as.numeric(valor), rep(60, 6), tolerance = 1e-8))) {
    message("Parece que multiplicaste por 6 en lugar de utilizar las probabilidades 1/6. Cada frecuencia esperada debe ser 60 * (1/6) = 10.")
    return(FALSE)
  }
  
  .verifica(
    e, "esperados", rep(10, 6), 1e-8,
    "Aplica E = n * p: multiplica el total de observaciones por cada probabilidad. En este ejemplo, cada resultado debe ser 60 * (1/6) = 10."
  )
}

#===============================================================================
# Ejecución de la prueba chi-cuadrada
#===============================================================================
test_prueba <- function() {
  e <- get("e", parent.frame())
  es_asignacion <- .es_asignacion(e, "prueba")
  valor <- e$val
  
  if (!inherits(valor, "htest")) {
    if (!es_asignacion) {
      message("Debes guardar el resultado de la prueba en la variable prueba. Utiliza prueba <- chisq.test(observados, p = probabilidades).")
    } else {
      message("El resultado no es un objeto de prueba chi-cuadrada. Revisa la llamada a chisq.test() y sus argumentos observados y p = probabilidades.")
    }
    return(FALSE)
  }
  
  if (!es_asignacion) {
    message("La prueba se ejecutó correctamente, pero no guardaste el resultado en prueba. Utiliza <- para asignarlo.")
    return(FALSE)
  }
  
  if (!.llamada_chisq_correcta(e)) {
    message("La instrucción debe utilizar chisq.test() con observados como frecuencias y p = probabilidades como modelo teórico. Guarda la prueba en prueba.")
    return(FALSE)
  }
  
  estadistico_correcto <- .verifica_numero(valor$statistic, 1, 1e-7)
  gl_correctos <- .verifica_numero(valor$parameter, 5, 1e-8)
  p_correcto <- .verifica_numero(valor$p.value, 0.9625657732472964, 1e-7)
  
  if (estadistico_correcto && gl_correctos && p_correcto) {
    return(TRUE)
  }
  
  message("La prueba se ejecutó, pero sus resultados no son los esperados. Comprueba que uses las seis frecuencias observadas y las seis probabilidades teóricas con chisq.test(observados, p = probabilidades).")
  return(FALSE)
}

#===============================================================================
# Extracción del valor p
#===============================================================================
test_valor_p <- function() {
  e <- get("e", parent.frame())
  .verifica(
    e, "valor_p", 0.9625657732472964, 1e-7,
    "Accede al componente p.value del objeto prueba mediante prueba$p.value. No utilices statistic ni parameter, porque corresponden a otros resultados."
  )
}

#===============================================================================
# Nivel de significancia
#===============================================================================
test_alfa <- function() {
  e <- get("e", parent.frame())
  .verifica(
    e, "alfa", 0.05, 1e-8,
    "El nivel de significancia indicado en el ejercicio es 0.05. Guárdalo en alfa utilizando alfa <- 0.05."
  )
}

#===============================================================================
# Comparación del valor p con alfa
#===============================================================================
test_decision <- function() {
  e <- get("e", parent.frame())
  expr <- e$expr
  valor <- e$val
  
  if (is.call(expr) && length(expr) == 3 &&
      identical(expr[[1]], as.name(">")) &&
      identical(expr[[2]], as.name("alfa")) &&
      identical(expr[[3]], as.name("valor_p"))) {
    message("Invertiste el orden de la comparación. La instrucción pide comprobar si valor_p es mayor que alfa, no al contrario.")
    return(FALSE)
  }
  
  if (is.call(expr) && length(expr) == 3 &&
      identical(expr[[1]], as.name("<")) &&
      identical(expr[[2]], as.name("valor_p")) &&
      identical(expr[[3]], as.name("alfa"))) {
    message("Utilizaste el operador <. En este ejercicio debes comprobar si el valor p es mayor que alfa, utilizando >.")
    return(FALSE)
  }
  
  if (is.logical(valor) && length(valor) == 1 && isTRUE(valor)) {
    if (is.call(expr) && length(expr) == 3 &&
        identical(expr[[1]], as.name(">")) &&
        identical(expr[[2]], as.name("valor_p")) &&
        identical(expr[[3]], as.name("alfa"))) {
      return(TRUE)
    }
    message("El resultado es TRUE, pero utiliza exactamente la comparación solicitada: valor_p > alfa.")
    return(FALSE)
  }
  
  if (identical(valor, FALSE)) {
    message("La comparación devolvió FALSE. Revisa el operador: como el valor p es aproximadamente 0.9626 y alfa es 0.05, valor_p > alfa debe devolver TRUE.")
    return(FALSE)
  }
  
  message("La respuesta debe ser un valor lógico. Compara valor_p con alfa utilizando el operador >.")
  return(FALSE)
}
