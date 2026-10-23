# customTests.R
# Funciones de verificacion para la leccion "Distribuciones muestrales"
# (media, proporcion y varianza)

# Helper: swirl no expone get_val() de forma nativa. Buscamos la variable
# 'val' (el resultado evaluado de lo que el estudiante escribio) recorriendo
# todos los frames de la pila de llamadas.
get_val <- function() {
  for (i in 1:sys.nframe()) {
    env <- parent.frame(i)
    if (exists("val", envir = env, inherits = FALSE)) {
      return(get("val", envir = env))
    }
  }
  stop("No se encontro la variable 'val' en la pila de llamadas de swirl.")
}

# Pregunta con multiples hints: formula de la desviacion tipica de P-gorro
test_formula_prop_hints <- function(){
  val <- get_val()
  esperado <- sqrt(p_real * (1 - p_real) / n)
  
  if (isTRUE(all.equal(val, esperado, tolerance = 0.001))) {
    return(TRUE)
  }
  
  # Error comun 1: olvidar la raiz cuadrada
  sin_raiz <- p_real * (1 - p_real) / n
  if (isTRUE(all.equal(val, sin_raiz, tolerance = 0.001))) {
    swirl_out("Te falto aplicar sqrt() a todo el cociente. La formula es la raiz cuadrada de p*(1-p)/n.")
    return(FALSE)
  }
  
  # Error comun 2: olvidar el factor (1 - p)
  sin_complemento <- sqrt(p_real / n)
  if (isTRUE(all.equal(val, sin_complemento, tolerance = 0.001))) {
    swirl_out("Revisa la formula: falta multiplicar p_real por (1 - p_real) antes de dividir entre n.")
    return(FALSE)
  }
  
  # Error comun 3: usar 0.30 del ejemplo anterior en vez de p_real (0.65)
  con_p_ejemplo <- sqrt(0.30 * 0.70 / n)
  if (isTRUE(all.equal(val, con_p_ejemplo, tolerance = 0.001))) {
    swirl_out("Usaste el p del ejemplo de la oficina (0.30). Aqui debes usar p_real, que corresponde a nuestra poblacion actual.")
    return(FALSE)
  }
  
  swirl_out("Revisa la formula: sqrt(p_real * (1 - p_real) / n).")
  FALSE
}