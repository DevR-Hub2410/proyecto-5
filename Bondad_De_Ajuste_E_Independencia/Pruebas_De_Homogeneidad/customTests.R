
#===============================================================
# Funciones auxiliares
#===============================================================

# Comprueba que la respuesta sea una asignación a la variable solicitada.
.es_asignacion <- function(e, nombre) {
  expr <- e$expr
  
  if (!is.call(expr)) {
    return(FALSE)
  }
  
  operador <- as.character(expr[[1]])
  
  if (!(operador %in% c("<-", "="))) {
    return(FALSE)
  }
  
  if (!is.symbol(expr[[2]])) {
    return(FALSE)
  }
  
  identical(as.character(expr[[2]]), nombre)
}


# Verifica una asignación y compara su resultado con el valor esperado.
.verifica <- function(e, nombre, esperado, tol = 1e-6,
                      pista = NULL) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      "Debes guardar el resultado en la variable '",
      nombre,
      "' utilizando <-."
    )
    return(FALSE)
  }
  
  if (!is.numeric(e$val)) {
    message("El resultado debe ser numérico.")
    return(FALSE)
  }
  
  if (length(e$val) != length(esperado)) {
    message(
      "El resultado no tiene la cantidad de elementos esperada. ",
      "Revisa los valores que estás utilizando."
    )
    return(FALSE)
  }
  
  if (!isTRUE(all.equal(
    as.numeric(e$val),
    as.numeric(esperado),
    tolerance = tol
  ))) {
    if (!is.null(pista)) {
      message(pista)
    } else {
      message(
        "El resultado no coincide con el valor esperado. ",
        "Revisa la función, sus argumentos y el orden de los valores."
      )
    }
    
    return(FALSE)
  }
  
  TRUE
}


# Verifica una expresión numérica directa, sin asignarla a una variable.
.verifica_num_aprox <- function(e, esperado, tol = 1e-6,
                                pista = NULL) {
  
  if (!is.numeric(e$val) || length(e$val) != 1) {
    message("Debes obtener un único resultado numérico.")
    return(FALSE)
  }
  
  if (!isTRUE(all.equal(
    as.numeric(e$val),
    as.numeric(esperado),
    tolerance = tol
  ))) {
    if (!is.null(pista)) {
      message(pista)
    } else {
      message(
        "El resultado no es correcto. ",
        "Revisa la función y los argumentos utilizados."
      )
    }
    
    return(FALSE)
  }
  
  TRUE
}


# Verifica una asignación cuyo resultado debe ser lógico.
.verifica_logica <- function(e, nombre, esperado = TRUE) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      "Debes guardar la comparación en la variable '",
      nombre,
      "' utilizando <-."
    )
    return(FALSE)
  }
  
  if (!is.logical(e$val) || length(e$val) != 1) {
    message("La comparación debe producir un único valor lógico.")
    return(FALSE)
  }
  
  if (!identical(e$val, esperado)) {
    message(
      "La decisión no coincide con el resultado esperado. ",
      "Revisa la comparación entre el valor p y el nivel de significancia."
    )
    return(FALSE)
  }
  
  TRUE
}


#===============================================================
# Construcción de la tabla de contingencia
#===============================================================

test_tabla_homogeneidad <- function() {
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "tabla_homogeneidad")) {
    message("Debes asignar el resultado a tabla_homogeneidad utilizando <-.")
    return(FALSE)
  }
  
  tabla_esperada <- matrix(
    c(30, 18, 12,
      20, 22, 18,
      15, 20, 25),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("Software", "Industrial", "Administracion"),
      c("Videos", "Lecturas", "Practicos")
    )
  )
  
  tabla <- e$val
  
  if (!is.table(tabla)) {
    message("Debes utilizar table() para construir la tabla de contingencia.")
    return(FALSE)
  }
  
  if (!identical(dim(tabla), dim(tabla_esperada))) {
    message("La tabla debe tener 3 filas y 3 columnas.")
    return(FALSE)
  }
  
  if (!identical(
    unname(dimnames(tabla)),
    unname(dimnames(tabla_esperada))
  )) {
    message("Revisa que hayas utilizado carrera y recurso_estudio en ese orden.")
    return(FALSE)
  }
  
  if (!all(as.matrix(tabla) == tabla_esperada)) {
    message("Las frecuencias de la tabla no coinciden con los datos esperados.")
    return(FALSE)
  }
  
  TRUE
}

#===============================================================
# Aplicación de la prueba chi-cuadrado de homogeneidad
#===============================================================

test_prueba_homogeneidad <- function() {
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "prueba_homogeneidad")) {
    message(
      "Debes guardar el resultado en una variable llamada ",
      "prueba_homogeneidad."
    )
    return(FALSE)
  }
  
  if (!inherits(e$val, "htest")) {
    message(
      "El resultado debe ser una prueba estadística. ",
      "Utiliza chisq.test()."
    )
    return(FALSE)
  }
  
  if (!isTRUE(all.equal(
    unname(e$val$statistic),
    unname(chisq.test(tabla_homogeneidad)$statistic),
    tolerance = 1e-6
  ))) {
    message(
      "Revisa la tabla que proporcionaste a chisq.test(). ",
      "Debes utilizar tabla_homogeneidad."
    )
    return(FALSE)
  }
  
  TRUE
}


#===============================================================
# Extracción y visualización de frecuencias esperadas
#===============================================================

test_esperadas_homogeneidad <- function() {
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "esperadas_homogeneidad")) {
    message(
      "Guarda las frecuencias esperadas en una variable ",
      "llamada esperadas_homogeneidad."
    )
    return(FALSE)
  }
  
  esperado <- chisq.test(tabla_homogeneidad)$expected
  
  if (!isTRUE(all.equal(
    unname(e$val),
    unname(esperado),
    tolerance = 1e-6
  ))) {
    message(
      "Revisa que hayas extraído el elemento expected ",
      "de prueba_homogeneidad."
    )
    return(FALSE)
  }
  
  TRUE
}


#===============================================================
# Interpretación y toma de decisiones
#===============================================================

test_valor_p_homogeneidad <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e,
    "valor_p_homogeneidad",
    chisq.test(tabla_homogeneidad)$p.value,
    tol = 1e-6,
    pista = "Extrae el elemento p.value de prueba_homogeneidad."
  )
}


test_alfa_homogeneidad <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e,
    "alfa_homogeneidad",
    0.05,
    tol = 1e-6,
    pista = "El nivel de significancia de este ejercicio es 0.05."
  )
}


test_decision_homogeneidad <- function() {
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "decision_homogeneidad")) {
    message(
      "Debes guardar la decisión en una variable llamada ",
      "decision_homogeneidad."
    )
    return(FALSE)
  }
  
  if (!identical(e$val, "Rechazar H0")) {
    message(
      "Revisa la condición de ifelse(). El valor p es menor ",
      "que alfa, por lo que debes rechazar H0."
    )
    return(FALSE)
  }
  
  TRUE
}
