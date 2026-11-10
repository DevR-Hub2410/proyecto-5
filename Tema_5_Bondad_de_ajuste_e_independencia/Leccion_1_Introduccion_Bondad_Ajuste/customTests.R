#===============================================================================
#   Pruebas personalizadas de la lección Introduccion_Bondad_Ajuste
#   swirl carga este archivo automáticamente desde la carpeta de la lección.
#===============================================================================

test_observados <- function() {
  e <- get("e", parent.frame())
  identical(e$val, c(8, 12, 9, 11, 10, 10))
}

test_mostrar_observados <- function() {
  e <- get("e", parent.frame())
  identical(e$val, c(8, 12, 9, 11, 10, 10))
}

test_total <- function() {
  e <- get("e", parent.frame())
  identical(e$val, 60)
}

test_probabilidades <- function() {
  e <- get("e", parent.frame())
  isTRUE(all.equal(e$val, rep(1/6, 6)))
}

test_suma_prob <- function() {
  e <- get("e", parent.frame())
  isTRUE(all.equal(e$val, 1))
}

test_esperados <- function() {
  e <- get("e", parent.frame())
  isTRUE(all.equal(e$val, rep(10, 6)))
}

test_mostrar_esperados <- function() {
  e <- get("e", parent.frame())
  isTRUE(all.equal(e$val, rep(10, 6)))
}

test_prueba <- function() {
  e <- get("e", parent.frame())
  inherits(e$val, "htest")
}

test_mostrar_prueba <- function() {
  e <- get("e", parent.frame())
  inherits(e$val, "htest")
}

test_valor_p <- function() {
  e <- get("e", parent.frame())
  isTRUE(all.equal(
    e$val,
    0.9625657732472964
  ))
}

test_alfa <- function() {
  e <- get("e", parent.frame())
  identical(e$val, 0.05)
}

test_decision <- function() {
  e <- get("e", parent.frame())
  identical(e$val, TRUE)
}

