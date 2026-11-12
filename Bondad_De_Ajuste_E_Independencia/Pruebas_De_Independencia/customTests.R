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

test_tabla_contingencia <- function() {
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "tabla_contingencia")) {
    message("Debes asignar el resultado a una variable llamada tabla_contingencia.")
    return(FALSE)
  }
  
  tabla_esperada <- table(
    datos_estudiantes$modalidad,
    datos_estudiantes$resultado
  )
  
  if (!is.table(e$val)) {
    message("El resultado debe ser una tabla de contingencia. Utiliza la función table().")
    return(FALSE)
  }
  
  if (!identical(e$val, tabla_esperada)) {
    message("Revisa las variables que proporcionaste a table(). Debes relacionar modalidad y resultado de datos_estudiantes.")
    return(FALSE)
  }
  
  TRUE
}

test_prueba_independencia <- function() {
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "prueba_independencia")) {
    message("Debes guardar el resultado en una variable llamada prueba_independencia.")
    return(FALSE)
  }
  
  if (!inherits(e$val, "htest")) {
    message("El resultado debe ser una prueba estadística. Utiliza chisq.test().")
    return(FALSE)
  }
  
  if (!isTRUE(all.equal(
    unname(e$val$statistic),
    unname(chisq.test(tabla_contingencia)$statistic),
    tolerance = 1e-6
  ))) {
    message("Revisa la tabla que proporcionaste a chisq.test(). Debes utilizar tabla_contingencia.")
    return(FALSE)
  }
  
  TRUE
}


#---------------------------------------------------------------
# Interpretación y toma de decisiones
#---------------------------------------------------------------

test_valor_p_independencia <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e,
    "valor_p_independencia",
    0.0404239793,
    tol = 0.002,
    pista = "Extrae el elemento p.value de prueba_independencia."
  )
}

test_alfa_independencia <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e,
    "alfa_independencia",
    0.05,
    tol = 1e-6,
    pista = "El nivel de significancia de este ejercicio es 0.05."
  )
}

test_decision_independencia <- function() {
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "decision_independencia")) {
    message("Debes guardar la decisión en una variable llamada decision_independencia.")
    return(FALSE)
  }
  
  if (!identical(e$val, "Rechazar H0")) {
    message("Revisa la condición de ifelse(). El valor p es menor que alfa, por lo que debes rechazar H0.")
    return(FALSE)
  }
  
  TRUE
}
