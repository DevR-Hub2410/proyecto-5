test_prop_ci <- function() {
  
  e <- get("e", parent.frame())
  
  expr_text <- texto_expr(e)
  val       <- e$val
  
  # Intervalos de referencia (con y sin correccion de continuidad)
  x <- c(exito_A, exito_B)
  n <- c(total_A, total_B)
  ic_con_correccion <- prop.test(x, n)$conf.int
  ic_sin_correccion <- prop.test(x, n, correct = FALSE)$conf.int
  ic_90 <- prop.test(x, n, conf.level = 0.90)$conf.int
  ic_99 <- prop.test(x, n, conf.level = 0.99)$conf.int
  
  # --- Respuesta correcta: acepta correct = FALSE y argumentos con nombre
  if (es_igual(val, ic_con_correccion) || es_igual(val, ic_sin_correccion)) {
    return(TRUE)
  }
  
  # --- Error 1: uso t.test() en lugar de prop.test() -------------------
  if (grepl("t\\.test\\(", expr_text)) {
    message(
      "t.test() es para medias. Para proporciones debes usar prop.test()."
    )
    return(FALSE)
  }
  
  # --- Error 2: olvido $conf.int (guardo todo el resultado) -------------
  if (inherits(val, "htest") || !grepl("conf\\.int", expr_text)) {
    message(
      "El resultado de prop.test() es una lista con muchos elementos. ",
      "Agrega $conf.int al final para extraer solo el intervalo de confianza."
    )
    return(FALSE)
  }
  
  # --- Error 3: invirtio el orden (totales primero, exitos despues) -----
  if (grepl("prop\\.test\\(c\\(total_A,total_B\\)", expr_text)) {
    message(
      "Invertiste el orden de los vectores: prop.test() espera primero ",
      "el vector de exitos y despues el vector de totales."
    )
    return(FALSE)
  }
  
  # --- Error 4: agrupo exito y total de un mismo grupo ------------------
  if (grepl("prop\\.test\\(c\\(exito_A,total_A\\)", expr_text)) {
    message(
      "Agrupaste el exito y el total de un mismo grupo en el mismo vector. ",
      "Debes agrupar por tipo de dato: un vector con los dos exitos, y otro ",
      "vector con los dos totales."
    )
    return(FALSE)
  }
  
  # --- Error 5: nivel de confianza distinto de 95% ----------------------
  if (es_igual(val, ic_90) || es_igual(val, ic_99)) {
    message(
      "El intervalo se calculo con un nivel de confianza distinto de 95%. ",
      "Revisa el argumento conf.level o eliminalo para usar el valor por defecto."
    )
    return(FALSE)
  }
  
  # --- Error 6: intervalo de proporciones de un solo grupo --------------
  if (is.numeric(val) && !grepl(",", expr_text)) {
    message(
      "Necesitas comparar los dos grupos: incluye los exitos y los totales ",
      "de A y de B en los vectores."
    )
    return(FALSE)
  }
  
  # --- Cualquier otro error: retroalimentacion general ------------------
  message(
    "Revisa la sintaxis: prop.test(c(exito_A, exito_B), c(total_A, total_B))$conf.int. ",
    "El primer vector debe contener los exitos de ambos grupos, y el segundo ",
    "los totales, en ese orden."
  )
  return(FALSE)
}

test_variable_numerica <- function(nombre, correcto, tolerancia = 1e-4) {
  e <- get("e", parent.frame())
  
  # Revisa si el estudiante asignó el resultado a la variable pedida
  es_asignacion <- is.call(e$expr) &&
    (identical(e$expr[[1]], as.name("<-")) ||
       identical(e$expr[[1]], as.name("="))) &&
    identical(e$expr[[2]], as.name(nombre))
  
  # Revisa si el valor obtenido coincide con el valor correcto
  valor_correcto <- is.numeric(e$val) &&
    length(e$val) == length(correcto) &&
    isTRUE(all.equal(as.numeric(e$val), correcto, tolerance = tolerancia))
  
  # Respuesta correcta
  if (es_asignacion && valor_correcto) {
    return(TRUE)
  }
  
  # Error 1: el valor es correcto, pero no lo guardo en la variable pedida
  if (!es_asignacion && valor_correcto) {
    message(
      "El resultado es correcto, pero no lo guardaste en la variable ",
      nombre, ". Utiliza <- para asignar el resultado."
    )
    return(FALSE)
  }
  
  # Error 2: guardó algo en la variable, pero el valor no coincide
  if (es_asignacion) {
    message(
      "Guardaste el resultado en ", nombre, ", pero el valor no coincide. ",
      "Revisa las funciones y los vectores que utilizaste."
    )
    return(FALSE)
  }
  
  # Cualquier otro error
  message(
    "Calcula el resultado y guardalo en la variable ", nombre,
    " utilizando <-."
  )
  return(FALSE)
}

test_se_manual <- function() {
  
  e <- get("e", parent.frame())
  
  expr_text <- texto_expr(e)
  asigna    <- asigna_a(e, "se_manual")
  
  # Valor de referencia calculado con los datos de la leccion
  esperado <- sqrt(var(grupoA) / length(grupoA) + var(grupoB) / length(grupoB))
  
  # --- Respuesta correcta: cualquier forma que produzca el valor ------
  # (acepta <- o =, orden A+B o B+A, ^0.5, sd()^2, espacios, etc.)
  if (asigna && es_igual(e$val, esperado)) {
    return(TRUE)
  }
  
  # --- Error 1: no guardo el resultado en se_manual -------------------
  if (!asigna && es_igual(e$val, esperado)) {
    message(
      "El calculo es correcto, pero debes guardarlo en la variable ",
      "se_manual usando <-."
    )
    return(FALSE)
  }
  
  # --- Error 2: olvido la raiz cuadrada -------------------------------
  if (es_igual(e$val, esperado^2)) {
    message(
      "Calculaste la varianza combinada correctamente, pero olvidaste ",
      "aplicar la raiz cuadrada (sqrt) para obtener el error estandar."
    )
    return(FALSE)
  }
  
  # --- Error 3: uso sd() sin elevarla al cuadrado ---------------------
  if (grepl("sd\\(", expr_text) && !grepl("\\^2|\\^\\(2\\)", expr_text)) {
    message(
      "Estas usando sd() (desviacion estandar), pero la formula requiere ",
      "la varianza: var(). Recuerda que var(x) equivale a sd(x) al cuadrado."
    )
    return(FALSE)
  }
  
  # --- Error 4: no dividio entre el tamano de muestra -----------------
  if (!grepl("length\\(", expr_text)) {
    message(
      "Sumaste las varianzas, pero falta dividir cada una entre su ",
      "tamano de muestra (length) antes de sumarlas."
    )
    return(FALSE)
  }
  
  # --- Error 5: resto en lugar de sumar las varianzas -----------------
  if (grepl("length\\(grupoA\\)-var\\(grupoB\\)", expr_text)) {
    message(
      "Las varianzas de las dos medias se suman, no se restan: la ",
      "incertidumbre de la diferencia se acumula."
    )
    return(FALSE)
  }
  
  # --- Cualquier otro error: retroalimentacion general ----------------
  message(
    "Revisa la formula del error estandar para la diferencia de medias: ",
    "sqrt(var(grupoA)/length(grupoA) + var(grupoB)/length(grupoB)). ",
    "Verifica los nombres de las variables y de las funciones utilizadas."
  )
  return(FALSE)
}