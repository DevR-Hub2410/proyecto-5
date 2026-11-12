
# ==========================================
# Interpretacion y aplicacion practica
# ==========================================


# ------------------------------------------
# FUNCIONES AUXILIARES
# ------------------------------------------

.es_asignacion <- function(e, nombre) {
  
  expr <- e$expr
  
  if (is.expression(expr)) {
    expr <- expr[[1]]
  }
  
  if (!is.call(expr)) {
    return(FALSE)
  }
  
  operador <- as.character(expr[[1]])
  
  if (!operador %in% c("<-", "=")) {
    return(FALSE)
  }
  
  izquierda <- expr[[2]]
  
  if (!is.symbol(izquierda)) {
    return(FALSE)
  }
  
  identical(as.character(izquierda), nombre)
}


.verifica <- function(e, nombre, esperado,
                      tol = 0.0001,
                      pista = NULL) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      paste(
        "Debes asignar el resultado a",
        nombre,
        "utilizando <-."
      )
    )
    return(FALSE)
  }
  
  if (!isTRUE(all.equal(
    as.numeric(e$val),
    as.numeric(esperado),
    tolerance = tol,
    check.attributes = FALSE
  ))) {
    
    if (is.null(pista)) {
      message("Revisa los valores utilizados en tu respuesta.")
    } else {
      message(pista)
    }
    
    return(FALSE)
  }
  
  TRUE
}


.verifica_matriz <- function(e, nombre, esperada) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      paste(
        "Debes asignar la matriz a",
        nombre,
        "utilizando <-."
      )
    )
    return(FALSE)
  }
  
  resultado <- e$val
  
  if (!is.matrix(resultado)) {
    message("Debes utilizar matrix() para construir la matriz.")
    return(FALSE)
  }
  
  if (!identical(dim(resultado), dim(esperada))) {
    message("Revisa la cantidad de filas y columnas de la matriz.")
    return(FALSE)
  }
  
  if (!identical(
    unname(dimnames(resultado)),
    unname(dimnames(esperada))
  )) {
    message("Revisa los nombres y el orden de las filas y columnas.")
    return(FALSE)
  }
  
  if (!all(as.numeric(resultado) == as.numeric(esperada))) {
    message("Las frecuencias de la matriz no coinciden con las del problema.")
    return(FALSE)
  }
  
  TRUE
}


.verifica_prueba <- function(e, nombre, esperada) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      paste(
        "Debes asignar el resultado a",
        nombre,
        "utilizando <-."
      )
    )
    return(FALSE)
  }
  
  resultado <- e$val
  
  if (!inherits(resultado, "htest")) {
    message("Debes utilizar chisq.test() para realizar la prueba.")
    return(FALSE)
  }
  
  if (
    abs(resultado$statistic - esperada$statistic) > 0.0001 ||
    abs(resultado$p.value - esperada$p.value) > 0.0001 ||
    resultado$parameter != esperada$parameter
  ) {
    message("Revisa los datos y los argumentos utilizados en chisq.test().")
    return(FALSE)
  }
  
  TRUE
}


# ------------------------------------------
# CASO 1
# PRUEBA DE BONDAD DE AJUSTE
# ------------------------------------------

test_errores_observados <- function() {
  
  e <- get("e", parent.frame())
  
  .verifica(
    e,
    "errores_observados",
    c(32, 28, 21, 19),
    pista = "Utiliza las frecuencias 32, 28, 21 y 19 en ese orden."
  )
}


test_proporciones_esperadas <- function() {
  
  e <- get("e", parent.frame())
  
  .verifica(
    e,
    "proporciones_esperadas",
    rep(0.25, 4),
    pista = "Utiliza rep() para repetir 0.25 cuatro veces."
  )
}


test_prueba_errores <- function() {
  
  e <- get("e", parent.frame())
  
  esperada <- chisq.test(
    c(32, 28, 21, 19),
    p = rep(0.25, 4)
  )
  
  .verifica_prueba(
    e,
    "prueba_errores",
    esperada
  )
}


test_valor_p_errores <- function() {
  
  e <- get("e", parent.frame())
  
  esperado <- chisq.test(
    c(32, 28, 21, 19),
    p = rep(0.25, 4)
  )$p.value
  
  .verifica(
    e,
    "valor_p_errores",
    esperado
  )
}


test_decision_errores <- function() {
  
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "decision_errores")) {
    message("Debes asignar el resultado a decision_errores.")
    return(FALSE)
  }
  
  if (!identical(e$val, "No rechazar H0")) {
    message("Revisa la comparación del valor p con 0.05.")
    return(FALSE)
  }
  
  TRUE
}


# ------------------------------------------
# CASO 2
# PRUEBA DE INDEPENDENCIA
# ------------------------------------------

test_tabla_dispositivos <- function() {
  
  e <- get("e", parent.frame())
  
  esperada <- matrix(
    c(15, 45,
      30, 30),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("Computadora", "Movil"),
      c("Con_errores", "Sin_errores")
    )
  )
  
  .verifica_matriz(
    e,
    "tabla_dispositivos",
    esperada
  )
}


test_prueba_dispositivos <- function() {
  
  e <- get("e", parent.frame())
  
  tabla <- matrix(
    c(15, 45, 30, 30),
    nrow = 2,
    byrow = TRUE
  )
  
  esperada <- chisq.test(tabla)
  
  .verifica_prueba(
    e,
    "prueba_dispositivos",
    esperada
  )
}


test_valor_p_dispositivos <- function() {
  
  e <- get("e", parent.frame())
  
  esperado <- chisq.test(
    matrix(c(15, 45, 30, 30), nrow = 2, byrow = TRUE)
  )$p.value
  
  .verifica(
    e,
    "valor_p_dispositivos",
    esperado
  )
}


test_decision_dispositivos <- function() {
  
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "decision_dispositivos")) {
    message("Debes asignar el resultado a decision_dispositivos.")
    return(FALSE)
  }
  
  if (!identical(e$val, "Rechazar H0")) {
    message("El valor p es menor que 0.05. Revisa tu decisión.")
    return(FALSE)
  }
  
  TRUE
}


# ------------------------------------------
# CASO 3
# PRUEBA DE HOMOGENEIDAD
# ------------------------------------------

test_tabla_equipos <- function() {
  
  e <- get("e", parent.frame())
  
  esperada <- matrix(
    c(20, 25, 15,
      12, 28, 20,
      18, 17, 25),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("Equipo_A", "Equipo_B", "Equipo_C"),
      c("Criticos", "Moderados", "Leves")
    )
  )
  
  .verifica_matriz(
    e,
    "tabla_equipos",
    esperada
  )
}


test_prueba_equipos <- function() {
  
  e <- get("e", parent.frame())
  
  tabla <- matrix(
    c(20, 25, 15,
      12, 28, 20,
      18, 17, 25),
    nrow = 3,
    byrow = TRUE
  )
  
  esperada <- chisq.test(tabla)
  
  .verifica_prueba(
    e,
    "prueba_equipos",
    esperada
  )
}


test_valor_p_equipos <- function() {
  
  e <- get("e", parent.frame())
  
  tabla <- matrix(
    c(20, 25, 15,
      12, 28, 20,
      18, 17, 25),
    nrow = 3,
    byrow = TRUE
  )
  
  esperado <- chisq.test(tabla)$p.value
  
  .verifica(
    e,
    "valor_p_equipos",
    esperado
  )
}


test_decision_equipos <- function() {
  
  e <- get("e", parent.frame())
  
  if (!.es_asignacion(e, "decision_equipos")) {
    message("Debes asignar el resultado a decision_equipos.")
    return(FALSE)
  }
  
  if (!identical(e$val, "No rechazar H0")) {
    message("Revisa la comparación del valor p con 0.05.")
    return(FALSE)
  }
  
  TRUE
}
