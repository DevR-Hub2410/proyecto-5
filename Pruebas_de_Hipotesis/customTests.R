# Put custom tests in this file.

# Busca la variable 'val' (lo que el estudiante acaba de escribir, ya
# evaluado) en la pila de llamadas de swirl. No es una funcion nativa
# de swirl, se construye asi porque swirl no siempre la deja en el
# mismo numero de frame.
get_val <- function() {
  for (i in 1:sys.nframe()) {
    env <- parent.frame(i)
    if (exists("val", envir = env, inherits = FALSE)) {
      return(get("val", envir = env))
    }
  }
  stop("No se encontro la variable 'val' en la pila de llamadas de swirl.")
}

# Hints multiples para el calculo manual del estadistico t:
# t = (media_muestral - mu0) / (sd_muestral / sqrt(n))
test_t_formula_hints <- function() {
  val <- get_val()
  correcto <- (media_muestral - mu0) / (sd_muestral / sqrt(n))
  if (isTRUE(all.equal(val, correcto, tolerance = 1e-6))) {
    return(TRUE)
  }
  
  # Olvido aplicar la raiz cuadrada a n en el denominador
  variante_sin_sqrt <- (media_muestral - mu0) / (sd_muestral / n)
  if (isTRUE(all.equal(val, variante_sin_sqrt, tolerance = 1e-6))) {
    swirl_out("Revisa el denominador, te hizo falta aplicar sqrt() a n antes de dividir.")
    return(FALSE)
  }
  
  # Invirtio el orden de la resta en el numerador
  variante_invertida <- (mu0 - media_muestral) / (sd_muestral / sqrt(n))
  if (isTRUE(all.equal(val, variante_invertida, tolerance = 1e-6))) {
    swirl_out("Revisa el orden de la resta en el numerador, es media_muestral menos mu0, no al reves.")
    return(FALSE)
  }
  
  # Olvido dividir entre sqrt(n) por completo
  variante_sin_n <- (media_muestral - mu0) / sd_muestral
  if (isTRUE(all.equal(val, variante_sin_n, tolerance = 1e-6))) {
    swirl_out("Te hizo falta dividir sd_muestral entre sqrt(n), no uses solo la desviacion estandar.")
    return(FALSE)
  }
  
  return(FALSE)
}