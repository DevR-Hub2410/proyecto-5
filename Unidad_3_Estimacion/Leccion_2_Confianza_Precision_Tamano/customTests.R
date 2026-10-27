#===============================================================================
#   Pruebas personalizadas de la lección Confianza_Precision_Tamano
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
  
  # Error 1: el valor correcto, pero no lo guardó en la variable pedida
  if (!es_asignacion && valor_correcto) {
    message("El resultado es correcto, pero no lo guardaste en la variable", nombre, ". Utiliza <- para asignar el resultado.")
    return(FALSE)
  }
  
  # Cualquier otro error 
  message("Calcula el resultado y guárdalo en la variable ", nombre, " utilizando <-. ", pista)
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

