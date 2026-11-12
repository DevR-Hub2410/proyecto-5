
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


# Verifica que se haya guardado una prueba chi-cuadrado
# y que tenga los grados de libertad esperados.
.verifica_prueba <- function(e, nombre, grados_libertad) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      "Debes guardar el resultado de chisq.test() en la variable '",
      nombre,
      "' utilizando <-."
    )
    return(FALSE)
  }
  
  if (!inherits(e$val, "htest")) {
    message(
      "El resultado debe ser un objeto de prueba estadística. ",
      "Revisa que hayas utilizado chisq.test()."
    )
    return(FALSE)
  }
  
  if (!identical(
    as.numeric(e$val$parameter),
    as.numeric(grados_libertad)
  )) {
    message(
      "Los grados de libertad no son los esperados. ",
      "Revisa el número de categorías y las probabilidades utilizadas."
    )
    return(FALSE)
  }
  
  TRUE
}


#===============================================================
# Distribución normal
#===============================================================

# Probabilidad entre 85 y 115, sin guardar en una variable.
test_probabilidad_intervalo <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e,
    pnorm(115, mean = 100, sd = 15) -
      pnorm(85, mean = 100, sd = 15),
    pista = paste(
      "Calcula la probabilidad acumulada hasta 115",
      "y réstale la probabilidad acumulada hasta 85."
    )
  )
}


test_prob_1 <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "prob_1",
    pnorm(85, mean = 100, sd = 15),
    pista = paste(
      "Utiliza pnorm() con q = 85, mean = 100 y sd = 15.",
      "Guarda el resultado en prob_1."
    )
  )
}


test_prob_2 <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "prob_2",
    pnorm(100, mean = 100, sd = 15) -
      pnorm(85, mean = 100, sd = 15),
    pista = paste(
      "Calcula pnorm(100, mean = 100, sd = 15)",
      "y réstale pnorm(85, mean = 100, sd = 15)."
    )
  )
}


test_prob_3 <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "prob_3",
    pnorm(115, mean = 100, sd = 15) -
      pnorm(100, mean = 100, sd = 15),
    pista = paste(
      "Calcula pnorm(115, mean = 100, sd = 15)",
      "y réstale pnorm(100, mean = 100, sd = 15)."
    )
  )
}


test_prob_4 <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "prob_4",
    pnorm(115, mean = 100, sd = 15, lower.tail = FALSE),
    pista = paste(
      "Utiliza pnorm() con q = 115, mean = 100, sd = 15",
      "y lower.tail = FALSE."
    )
  )
}


test_probabilidades_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidades",
    c(
      pnorm(85, mean = 100, sd = 15),
      pnorm(100, mean = 100, sd = 15) -
        pnorm(85, mean = 100, sd = 15),
      pnorm(115, mean = 100, sd = 15) -
        pnorm(100, mean = 100, sd = 15),
      pnorm(115, mean = 100, sd = 15, lower.tail = FALSE)
    ),
    pista = paste(
      "Utiliza c() para reunir prob_1, prob_2, prob_3 y prob_4,",
      "respetando ese orden."
    )
  )
}


test_suma_probabilidades <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e, 1,
    pista = "Utiliza sum(probabilidades) para sumar las cuatro probabilidades."
  )
}


test_esperados_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "esperados",
    200 * c(
      pnorm(85, mean = 100, sd = 15),
      pnorm(100, mean = 100, sd = 15) -
        pnorm(85, mean = 100, sd = 15),
      pnorm(115, mean = 100, sd = 15) -
        pnorm(100, mean = 100, sd = 15),
      pnorm(115, mean = 100, sd = 15, lower.tail = FALSE)
    ),
    pista = paste(
      "Multiplica las probabilidades por 200",
      "y guarda el vector resultante en esperados."
    )
  )
}


test_observados_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "observados",
    c(34, 64, 70, 32),
    pista = paste(
      "Crea el vector c(34, 64, 70, 32)",
      "y guárdalo en observados."
    )
  )
}


test_prueba_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica_prueba(e, "prueba_normal", 3)
}


test_valor_p_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "valor_p",
    0.9242707129,
    tol = 0.002,
    pista = paste(
      "Extrae el componente p.value del objeto prueba_normal",
      "utilizando el operador $."
    )
  )
}


test_alfa_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "alfa", 0.05,
    pista = "Asigna el valor 0.05 a la variable alfa."
  )
}


test_decision_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica_logica(e, "decision", TRUE)
}


#===============================================================
# Distribución binomial
#===============================================================

# Probabilidad de exactamente tres éxitos, sin asignación.
test_probabilidad_tres <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e,
    dbinom(3, size = 4, prob = 0.5),
    pista = paste(
      "Utiliza dbinom() con x = 3, size = 4 y prob = 0.5.",
      "Recuerda que debes escribir la expresión directamente."
    )
  )
}


test_probabilidades_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidades_binomial",
    dbinom(0:4, size = 4, prob = 0.5),
    pista = paste(
      "Utiliza dbinom() con x = 0:4, size = 4 y prob = 0.5.",
      "Guarda el vector en probabilidades_binomial."
    )
  )
}


test_suma_probabilidades_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e, 1,
    pista = "Utiliza sum(probabilidades_binomial) para sumar las probabilidades."
  )
}


test_esperados_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "esperados_binomial",
    200 * dbinom(0:4, size = 4, prob = 0.5),
    pista = paste(
      "Multiplica probabilidades_binomial por 200",
      "y guarda el resultado en esperados_binomial."
    )
  )
}


test_observados_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "observados_binomial",
    c(15, 46, 78, 45, 16),
    pista = paste(
      "Crea el vector c(15, 46, 78, 45, 16)",
      "y guárdalo en observados_binomial."
    )
  )
}


test_prueba_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica_prueba(e, "prueba_binomial", 4)
}


test_valor_p_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "valor_p_binomial",
    0.6590159875,
    tol = 0.002,
    pista = paste(
      "Extrae el componente p.value del objeto prueba_binomial",
      "utilizando el operador $."
    )
  )
}


test_decision_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica_logica(e, "decision_binomial", TRUE)
}


#===============================================================
# Distribución de Poisson
#===============================================================

# Probabilidad de exactamente cuatro eventos, sin asignación.
test_probabilidad_cuatro <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e,
    dpois(4, lambda = 2),
    pista = paste(
      "Utiliza dpois() con x = 4 y lambda = 2.",
      "Escribe la expresión directamente."
    )
  )
}


test_probabilidades_iniciales <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidades_iniciales",
    dpois(0:3, lambda = 2),
    pista = paste(
      "Utiliza dpois() con x = 0:3 y lambda = 2.",
      "Guarda el vector en probabilidades_iniciales."
    )
  )
}


test_probabilidad_cuatro_mas <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidad_cuatro_mas",
    ppois(3, lambda = 2, lower.tail = FALSE),
    pista = paste(
      "Utiliza ppois() con q = 3, lambda = 2",
      "y lower.tail = FALSE."
    )
  )
}


test_probabilidades_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidades_poisson",
    c(dpois(0:3, lambda = 2),
      ppois(3, lambda = 2, lower.tail = FALSE)),
    pista = paste(
      "Utiliza c() para reunir probabilidades_iniciales",
      "y probabilidad_cuatro_mas, en ese orden."
    )
  )
}


test_suma_probabilidades_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e, 1,
    pista = "Utiliza sum(probabilidades_poisson) para sumar las probabilidades."
  )
}


test_esperados_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "esperados_poisson",
    200 * c(
      dpois(0:3, lambda = 2),
      ppois(3, lambda = 2, lower.tail = FALSE)
    ),
    pista = paste(
      "Multiplica probabilidades_poisson por 200",
      "y guarda el resultado en esperados_poisson."
    )
  )
}


test_observados_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "observados_poisson",
    c(30, 50, 56, 35, 29),
    pista = paste(
      "Crea el vector c(30, 50, 56, 35, 29)",
      "y guárdalo en observados_poisson."
    )
  )
}


test_prueba_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica_prueba(e, "prueba_poisson", 4)
}


test_valor_p_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "valor_p_poisson",
    0.9466846566,
    tol = 0.003,
    pista = paste(
      "Extrae el componente p.value del objeto prueba_poisson",
      "utilizando el operador $."
    )
  )
}


test_decision_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica_logica(e, "decision_poisson", TRUE)
}
