
#===============================================================
# Funciones auxiliares
#===============================================================

# Comprueba que el estudiante haya asignado el resultado
# a la variable solicitada.

.es_asignacion <- function(e, nombre) {
  if (length(e$expr) == 0 || !is.call(e$expr[[1]])) {
    return(FALSE)
  }
  
  expresion <- e$expr[[1]]
  operador <- as.character(expresion[[1]])
  
  if (!operador %in% c("<-", "=")) {
    return(FALSE)
  }
  
  variable <- as.character(expresion[[2]])
  
  identical(variable, nombre)
}


# Comprueba asignaciones y resultados numéricos.

.verifica <- function(e, nombre, esperado,
                      tol = 1e-6,
                      pista = NULL) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      "Debes guardar el resultado en la variable '",
      nombre, "' utilizando <-."
    )
    return(FALSE)
  }
  
  if (!is.numeric(e$val)) {
    message("El resultado debe ser numérico.")
    if (!is.null(pista)) message(pista)
    return(FALSE)
  }
  
  correcto <- isTRUE(all.equal(
    as.numeric(e$val),
    as.numeric(esperado),
    tolerance = tol,
    check.attributes = FALSE
  ))
  
  if (!correcto) {
    message("El resultado no coincide con el valor esperado.")
    if (!is.null(pista)) message(pista)
    return(FALSE)
  }
  
  TRUE
}


# Comprueba resultados numéricos sin asignación.

.verifica_num_aprox <- function(e, esperado,
                                tol = 1e-6,
                                pista = NULL) {
  
  if (!is.numeric(e$val)) {
    message("Debes obtener un resultado numérico.")
    if (!is.null(pista)) message(pista)
    return(FALSE)
  }
  
  correcto <- isTRUE(all.equal(
    as.numeric(e$val),
    as.numeric(esperado),
    tolerance = tol,
    check.attributes = FALSE
  ))
  
  if (!correcto) {
    message("El resultado numérico no es el esperado.")
    if (!is.null(pista)) message(pista)
    return(FALSE)
  }
  
  TRUE
}


# Comprueba asignaciones de resultados lógicos.

.verifica_logica <- function(e, nombre, esperado = TRUE) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      "Guarda el resultado lógico en la variable '",
      nombre, "' utilizando <-."
    )
    return(FALSE)
  }
  
  if (!is.logical(e$val) || !identical(e$val, esperado)) {
    message(
      "Revisa la comparación. Comprueba si el valor p es mayor que alfa."
    )
    return(FALSE)
  }
  
  TRUE
}


# Comprueba que se haya guardado correctamente
# el resultado de una prueba chi-cuadrado.

.verifica_prueba <- function(e, nombre, grados_libertad) {
  
  if (!.es_asignacion(e, nombre)) {
    message(
      "Guarda el resultado de chisq.test() en la variable '",
      nombre, "' utilizando <-."
    )
    return(FALSE)
  }
  
  if (!inherits(e$val, "htest")) {
    message(
      "Debes utilizar chisq.test() para realizar la prueba."
    )
    return(FALSE)
  }
  
  if (as.numeric(e$val$parameter) != grados_libertad) {
    message("Revisa las categorías y los grados de libertad.")
    return(FALSE)
  }
  
  TRUE
}


#===============================================================
# DISTRIBUCIÓN NORMAL
#===============================================================

test_probabilidad_intervalo <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e,
    "probabilidad_intervalo",
    pnorm(115, mean = 100, sd = 15) -
      pnorm(85, mean = 100, sd = 15),
    pista = "Resta la probabilidad acumulada hasta 85 de la acumulada hasta 115."
  )
}


test_prob_1 <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "prob_1",
    pnorm(85, mean = 100, sd = 15),
    pista = "Utiliza pnorm(85, mean = 100, sd = 15)."
  )
}


test_prob_2 <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "prob_2",
    pnorm(100, mean = 100, sd = 15) -
      pnorm(85, mean = 100, sd = 15),
    pista = "Resta pnorm(85) de pnorm(100), conservando los parámetros."
  )
}


test_prob_3 <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "prob_3",
    pnorm(115, mean = 100, sd = 15) -
      pnorm(100, mean = 100, sd = 15),
    pista = "Resta pnorm(100) de pnorm(115)."
  )
}


test_prob_4 <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "prob_4",
    pnorm(115, mean = 100, sd = 15,
          lower.tail = FALSE),
    pista = "Utiliza lower.tail = FALSE para obtener la cola derecha."
  )
}


test_probabilidades_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidades",
    c(
      pnorm(85, 100, 15),
      pnorm(100, 100, 15) - pnorm(85, 100, 15),
      pnorm(115, 100, 15) - pnorm(100, 100, 15),
      pnorm(115, 100, 15, lower.tail = FALSE)
    ),
    pista = "Utiliza c() y conserva el orden prob_1, prob_2, prob_3 y prob_4."
  )
}


test_suma_probabilidades <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e, 1,
    pista = "Utiliza sum(probabilidades) para comprobar que las categorías suman 1."
  )
}


test_esperados_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "esperados",
    200 * c(
      pnorm(85, 100, 15),
      pnorm(100, 100, 15) - pnorm(85, 100, 15),
      pnorm(115, 100, 15) - pnorm(100, 100, 15),
      pnorm(115, 100, 15, lower.tail = FALSE)
    ),
    pista = "Multiplica las probabilidades por las 200 observaciones."
  )
}


test_observados_normal <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "observados",
    c(34, 64, 70, 32),
    pista = "Crea el vector con las frecuencias 34, 64, 70 y 32."
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
    0.9243,
    tol = 0.002,
    pista = "Extrae el componente p.value de prueba_normal."
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
# DISTRIBUCIÓN BINOMIAL
#===============================================================

test_probabilidad_tres <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidad_tres",
    dbinom(3, size = 4, prob = 0.5),
    pista = "Utiliza dbinom(3, size = 4, prob = 0.5)."
  )
}


test_probabilidades_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidades_binomial",
    dbinom(0:4, size = 4, prob = 0.5),
    pista = "Utiliza dbinom() con la secuencia 0:4."
  )
}


test_suma_probabilidades_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e, 1,
    pista = "Utiliza sum(probabilidades_binomial)."
  )
}


test_esperados_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "esperados_binomial",
    200 * dbinom(0:4, size = 4, prob = 0.5),
    pista = "Multiplica las probabilidades binomiales por 200."
  )
}


test_observados_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "observados_binomial",
    c(15, 46, 78, 45, 16),
    pista = "Crea el vector con las cinco frecuencias observadas."
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
    0.6590,
    tol = 0.002,
    pista = "Extrae el componente p.value de prueba_binomial."
  )
}


test_decision_binomial <- function() {
  e <- get("e", parent.frame())
  
  .verifica_logica(e, "decision_binomial", TRUE)
}


#===============================================================
# DISTRIBUCIÓN DE POISSON
#===============================================================

test_probabilidad_cuatro <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e,
    dpois(4, lambda = 2),
    pista = "Utiliza dpois(4, lambda = 2)."
  )
}


test_probabilidades_iniciales <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidades_iniciales",
    dpois(0:3, lambda = 2),
    pista = "Utiliza dpois() con la secuencia 0:3 y lambda = 2."
  )
}


test_probabilidad_cuatro_mas <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidad_cuatro_mas",
    ppois(3, lambda = 2, lower.tail = FALSE),
    pista = "Utiliza ppois() con q = 3 y lower.tail = FALSE."
  )
}


test_probabilidades_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "probabilidades_poisson",
    c(dpois(0:3, lambda = 2),
      ppois(3, lambda = 2, lower.tail = FALSE)),
    pista = "Une probabilidades_iniciales y probabilidad_cuatro_mas utilizando c()."
  )
}


test_suma_probabilidades_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica_num_aprox(
    e, 1,
    pista = "Utiliza sum(probabilidades_poisson)."
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
    pista = "Multiplica las probabilidades de Poisson por 200."
  )
}


test_observados_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica(
    e, "observados_poisson",
    c(30, 50, 56, 35, 29),
    pista = "Crea el vector con las cinco frecuencias observadas."
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
    0.9467,
    tol = 0.003,
    pista = "Extrae el componente p.value de prueba_poisson."
  )
}


test_decision_poisson <- function() {
  e <- get("e", parent.frame())
  
  .verifica_logica(e, "decision_poisson", TRUE)
}
