#===============================================================================
#   Pruebas personalizadas de la lección Confianza_Precision_Tamano
#   swirl carga este archivo automáticamente desde la carpeta de la lección.
#===============================================================================

#-------------------------------------------------------------------------------
# Función auxiliar. Revisa que el estudiante haya guardado en la variable
# `nombre` un valor numérico igual a `esperado`. No se llama directamente desde
# el lesson.yaml, la usan las pruebas de abajo.
#-------------------------------------------------------------------------------
.verifica <- function(e, nombre, esperado, tol = 1e-4, pista = NULL) {
  # Revisa si el estudiante asignó el resultado a la variable pedida
  es_asignacion <- is.call(e$expr) &&
    (identical(e$expr[[1]], as.name("<-")) ||
       identical(e$expr[[1]], as.name("="))) &&
    identical(as.character(e$expr[[2]]), nombre)
  
  # Revisa si el valor obtenido coincide con el esperado
  valor_correcto <- is.numeric(e$val) &&
    length(e$val) == length(esperado) &&
    isTRUE(all.equal(as.numeric(e$val), as.numeric(esperado), tolerance = tol))
  
  # Respuesta correcta
  if (es_asignacion && valor_correcto) {
    return(TRUE)
  }
  
  # Error 1: el valor es correcto, pero no lo guardó en la variable pedida
  if (!es_asignacion && valor_correcto) {
    message("El resultado es correcto, pero no lo guardaste en la variable ",
            nombre, ". Utiliza <- para asignar el resultado.")
    return(FALSE)
  }
  
  # Error 2: guardó algo en la variable, pero el valor no coincide
  if (es_asignacion) {
    message("Guardaste el resultado en ", nombre, ", pero el valor no coincide. ",
            pista)
    return(FALSE)
  }
  
  # Cualquier otro error
  message("Calcula el resultado y guárdalo en la variable ", nombre,
          " utilizando <-. ", pista)
  return(FALSE)
}

#===============================================================================
#                      Valor z para 95% de confianza
#===============================================================================
test_z_check <- function() {
  e <- get("e", parent.frame())
  .verifica(e, "z_check", qnorm(0.975), 1e-4,
            "Recuerda que el 95% de confianza deja 2.5% en cada cola, así que el área acumulada es 0.975.")
}

#===============================================================================
#                          Margen de error al 95%
#===============================================================================
test_margen_95 <- function() {
  e <- get("e", parent.frame())
  
  # Error específico: dividió entre n en lugar de la raíz cuadrada de n
  if (is.numeric(e$val) && length(e$val) == 1 &&
      isTRUE(all.equal(as.numeric(e$val), z_95 * sd_muestral / n_muestra,
                       tolerance = 1e-4))) {
    message("Casi. El denominador debe ser la raíz cuadrada de n, no n directamente.")
    return(FALSE)
  }
  
  .verifica(e, "margen_95", z_95 * sd_muestral / sqrt(n_muestra), 1e-4,
            "El margen de error es z multiplicado por el error estándar, sd_muestral / sqrt(n_muestra).")
}

#===============================================================================
#                 Márgenes de error para 90%, 95% y 99%
#===============================================================================
test_margenes_comparados <- function() {
  e <- get("e", parent.frame())
  .verifica(e, "margenes_comparados",
            c(z_90, z_95, z_99) * sd_muestral / sqrt(n_muestra), 1e-4,
            "Debe salir un vector con tres márgenes, uno para 90%, 95% y 99%.")
}

#===============================================================================
#       Pregunta numérica aproximada (el estudiante escribe solo un número)
#===============================================================================
test_num_aprox <- function(esperado, tol = 0.01) {
  e <- get("e", parent.frame())
  
  if (!is.numeric(e$val) || length(e$val) != 1 || !is.finite(e$val)) {
    message("Escribe solo un número. Puedes calcularlo antes en la consola de R.")
    return(FALSE)
  }
  
  ok <- abs(e$val - esperado) <= tol
  if (!ok) {
    message("Ese valor no coincide. Revisa tu cálculo y el redondeo que se pide.")
  }
  ok
}

#===============================================================================
#                    Efecto del tamaño de la muestra
#===============================================================================
test_margenes_por_n <- function() {
  e <- get("e", parent.frame())
  .verifica(e, "margenes_por_n", z_95 * sd_muestral / sqrt(c(20, 40, 80)), 1e-4,
            "Debe salir un vector con tres márgenes, uno por cada tamaño de muestra.")
}

test_margen_n160 <- function() {
  e <- get("e", parent.frame())
  .verifica(e, "margen_n160", z_95 * sd_muestral / sqrt(160), 1e-4,
            "Usa la fórmula del margen de error sustituyendo n por 160.")
}

#===============================================================================
#                    Determinación del tamaño de muestra
#===============================================================================
test_n_media <- function() {
  e <- get("e", parent.frame())
  .verifica(e, "n_requerido_media", ceiling((z_95 * sd_muestral / 10)^2), 1e-6,
            "Usa n = (z * sd / E)^2 con z_95 y E = 10, y no olvides ceiling().")
}

# Pregunta con múltiples hints: tamaño de muestra para una proporción
test_n_prop <- function() {
  e <- get("e", parent.frame())
  exacto   <- (z_95^2 * 0.5 * 0.5) / 0.03^2
  correcto <- ceiling(exacto)
  val      <- e$val
  
  cerca <- function(a, b) {
    is.numeric(a) && length(a) == 1 &&
      isTRUE(all.equal(as.numeric(a), b, tolerance = 1e-6))
  }
  
  es_asignacion <- is.call(e$expr) &&
    (identical(e$expr[[1]], as.name("<-")) ||
       identical(e$expr[[1]], as.name("="))) &&
    identical(as.character(e$expr[[2]]), "n_requerido_prop")
  
  if (!is.numeric(val) || length(val) != 1) {
    message("El resultado debe ser un solo número, el tamaño de muestra. Revisa la instrucción completa.")
    return(FALSE)
  }
  
  # Respuesta correcta
  if (cerca(val, correcto)) {
    if (es_asignacion) return(TRUE)
    message("El cálculo es correcto, pero no lo guardaste en n_requerido_prop. Utiliza <- para asignar el resultado.")
    return(FALSE)
  }
  
  # Error 1: no redondeó
  if (cerca(val, exacto)) {
    message("El cálculo está bien, pero un tamaño de muestra debe ser un número entero. Redondea con ceiling().")
    return(FALSE)
  }
  
  # Error 2: redondeó hacia abajo
  if (cerca(val, floor(exacto)) || cerca(val, round(exacto))) {
    message("Redondeaste hacia abajo. El tamaño de muestra siempre se redondea hacia arriba, con ceiling(), para no quedar por debajo del margen de error deseado.")
    return(FALSE)
  }
  
  # Error 3: no elevó E al cuadrado
  if (cerca(val, ceiling((z_95^2 * 0.5 * 0.5) / 0.03)) ||
      cerca(val, (z_95^2 * 0.5 * 0.5) / 0.03)) {
    message("Revisa el denominador. El margen de error E debe ir elevado al cuadrado, E^2.")
    return(FALSE)
  }
  
  message("Revisa la fórmula n = (z^2 * p * (1 - p)) / E^2 con z_95, p = 0.5 y E = 0.03, y redondea hacia arriba con ceiling().")
  return(FALSE)
}

test_margen_30 <- function() {
  e <- get("e", parent.frame())
  .verifica(e, "margen_30", z_95 * sd_muestral / sqrt(30), 1e-4,
            "Usa la misma fórmula del margen de error, sustituyendo n por 30.")
}