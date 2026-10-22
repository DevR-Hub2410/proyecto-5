# customTests.R
# Funciones de verificacion para la leccion "Distribuciones muestrales"

# Helper: swirl no expone get_val() de forma nativa. Buscamos la variable
# 'val' (el resultado evaluado de lo que el estudiante escribio) recorriendo
# todos los frames de la pila de llamadas, en vez de adivinar un numero fijo.
get_val <- function() {
  for (i in 1:sys.nframe()) {
    env <- parent.frame(i)
    if (exists("val", envir = env, inherits = FALSE)) {
      return(get("val", envir = env))
    }
  }
  stop("No se encontro la variable 'val' en la pila de llamadas de swirl.")
}

test_media_poblacion <- function(){
  val <- get_val()
  isTRUE(all.equal(val, mean(poblacion_calificaciones), tolerance = 0.01))
}

test_sd_poblacion <- function(){
  val <- get_val()
  isTRUE(all.equal(val, sd(poblacion_calificaciones), tolerance = 0.01))
}

test_medias_muestrales <- function(){
  val <- get_val()
  length(val) == 1000 &&
    abs(mean(val) - mu_poblacion) < 3
}

test_error_estandar <- function(){
  val <- get_val()
  esperado <- sigma_poblacion / sqrt(30)
  isTRUE(all.equal(val, esperado, tolerance = 0.01))
}

# Pregunta con multiples hints: error estandar de la media
test_error_estandar_hints <- function(){
  val <- get_val()
  esperado <- sigma_poblacion / sqrt(30)
  
  if (isTRUE(all.equal(val, esperado, tolerance = 0.01))) {
    return(TRUE)
  }
  
  # Error comun 1: dividir entre n en vez de sqrt(n)
  error_n <- sigma_poblacion / 30
  if (isTRUE(all.equal(val, error_n, tolerance = 0.01))) {
    swirl_out("Recuerda que el error estandar se calcula dividiendo entre la raiz cuadrada de n, no entre n.")
    return(FALSE)
  }
  
  # Error comun 2: olvidar dividir (solo reportar sigma)
  if (isTRUE(all.equal(val, sigma_poblacion, tolerance = 0.01))) {
    swirl_out("Te falto dividir la desviacion estandar poblacional entre la raiz cuadrada del tamano de muestra.")
    return(FALSE)
  }
  
  # Error comun 3: usar la desviacion estandar de una muestra en vez de la poblacional
  error_muestra <- sd(poblacion_calificaciones[1:30]) / sqrt(30)
  if (isTRUE(all.equal(val, error_muestra, tolerance = 0.01))) {
    swirl_out("Usa sigma_poblacion (la desviacion estandar de toda la poblacion), no la de una muestra especifica.")
    return(FALSE)
  }
  
  swirl_out("Revisa la formula: error estandar = sigma_poblacion / sqrt(30).")
  FALSE
}

test_error_proporcion <- function(){
  val <- get_val()
  esperado <- sqrt(p_poblacion * (1 - p_poblacion) / n_encuesta)
  isTRUE(all.equal(val, esperado, tolerance = 0.01))
}

# Pregunta con multiples hints: error estandar de la proporcion
# Pregunta de reflexion abierta: aceptamos cualquier respuesta que el
# estudiante escriba (swirl tiene un bug con AnswerTests: NULL en text_question).
test_reflexion <- function(){
  TRUE
}

test_error_proporcion_hints <- function(){
  val <- get_val()
  esperado <- sqrt(p_poblacion * (1 - p_poblacion) / n_encuesta)
  
  if (isTRUE(all.equal(val, esperado, tolerance = 0.01))) {
    return(TRUE)
  }
  
  # Error comun 1: olvidar la raiz cuadrada
  sin_raiz <- p_poblacion * (1 - p_poblacion) / n_encuesta
  if (isTRUE(all.equal(val, sin_raiz, tolerance = 0.01))) {
    swirl_out("Te falto aplicar sqrt() a todo el cociente. El error estandar es la raiz cuadrada de p*(1-p)/n.")
    return(FALSE)
  }
  
  # Error comun 2: olvidar el factor (1 - p)
  sin_complemento <- sqrt(p_poblacion / n_encuesta)
  if (isTRUE(all.equal(val, sin_complemento, tolerance = 0.01))) {
    swirl_out("Revisa la formula: falta multiplicar p_poblacion por (1 - p_poblacion) antes de dividir entre n_encuesta.")
    return(FALSE)
  }
  
  swirl_out("Revisa la formula: sqrt(p_poblacion * (1 - p_poblacion) / n_encuesta).")
  FALSE
}