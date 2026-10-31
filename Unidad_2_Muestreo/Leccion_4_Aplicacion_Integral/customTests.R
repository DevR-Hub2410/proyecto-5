# customTests.R - Pruebas personalizadas para la Actividad integradora de Muestreo
#
# 1) Evaluan el RESULTADO (objeto, tamano, valor) y no la forma exacta de escribir la instruccion.
# 2) Dan retroalimentacion especifica ante errores razonables.
# 3) Preguntas con multiples hints: en las preguntas dificiles, cada intento fallido
#    muestra una pista MAS especifica que la anterior (ver .escaleras).
# 4) En las preguntas de opcion multiple, cada distractor tiene su propia explicacion
#    (ver .retro_mc).
# Nota: el texto esta sin acentos a proposito, para evitar problemas de codificacion en Windows.

# ---------------------------------------------------------------
# Contador de intentos (se guarda en options para que persista entre pruebas)
# ---------------------------------------------------------------
.intento <- function(id) {
  lista <- getOption("intentos_swirl", list())
  k <- if (is.null(lista[[id]])) 1 else lista[[id]] + 1
  lista[[id]] <- k
  options(intentos_swirl = lista)
  k
}

.reinicia <- function(id) {
  lista <- getOption("intentos_swirl", list())
  lista[[id]] <- NULL
  options(intentos_swirl = lista)
  invisible(TRUE)
}

# ---------------------------------------------------------------
# Pistas progresivas (de lo conceptual a lo estructural, sin dar la respuesta completa)
# ---------------------------------------------------------------
.escaleras <- list(
  muestra_gratuito = c(
    "Recuerda que sample(x, size) elige size valores de x sin repetirlos. Pregunta clave: de que vector quieres elegirlos? Deben ser las calificaciones de satisfaccion del estrato Gratuito.",
    "Para extraer una columna de un data frame se usa el simbolo $, por ejemplo estrato_gratuito$satisfaccion. El tamano de la muestra de este estrato es 28.",
    "La instruccion tiene la forma muestra_gratuito <- sample(____, ____). Pon la columna del estrato en el primer espacio y el tamano de muestra en el segundo."
  ),
  muestra_premium = c(
    "Recuerda que sample(x, size) elige size valores de x sin repetirlos. Pregunta clave: de que vector quieres elegirlos? Deben ser las calificaciones de satisfaccion del estrato Premium.",
    "Para extraer una columna de un data frame se usa el simbolo $, por ejemplo estrato_premium$satisfaccion. El tamano de la muestra de este estrato es 12.",
    "La instruccion tiene la forma muestra_premium <- sample(____, ____). Pon la columna del estrato en el primer espacio y el tamano de muestra en el segundo."
  ),
  medias_estratificadas = c(
    "replicate(k, expresion) evalua la expresion k veces y junta los resultados en un vector. Cuantas veces quieres repetir el proceso? Ese numero ya esta guardado en num_muestras.",
    "La expresion que se repite es el proceso completo de una muestra estratificada. Una muestra de cada estrato (28 y 12 registros), unidas con c(), y al final su media con mean().",
    "La estructura es replicate(num_muestras, mean(c(sample(____, 28), sample(____, 12)))). Completa los espacios con la columna satisfaccion de cada estrato."
  ),
  error_estandar_teorico = c(
    "Piensa en la formula del error estandar de la media para un muestreo aleatorio simple. Es la desviacion estandar de la poblacion dividida entre algo que depende del tamano de muestra.",
    "La desviacion estandar de la poblacion ya esta guardada en sd_poblacional y el tamano de muestra en n. La raiz cuadrada se calcula con sqrt().",
    "La instruccion tiene la forma error_estandar_teorico <- sd_poblacional / ____(n). En el espacio va la funcion de raiz cuadrada."
  )
)

# ---------------------------------------------------------------
# Retroalimentacion por distractor (preguntas de opcion multiple)
# Cada fragmento debe aparecer en una sola opcion del lesson.yaml.
# ---------------------------------------------------------------
.retro_mc <- list(
  p1 = list(correcta = "800 usuarios registrados",
            incorrectas = list(
              "Solo los usuarios del plan Premium" = "El plan Premium es solo una parte de la poblacion (el 30%), es un estrato. La poblacion es el conjunto completo.",
              "Norte" = "La region Norte es un subconjunto. La poblacion incluye a todos los usuarios sin importar su region.",
              "desarrolladores" = "Los desarrolladores no son las unidades sobre las que se registra la satisfaccion. Los datos son de los usuarios."
            )),
  p2 = list(correcta = "nivel de satisfac",
            incorrectas = list(
              "total de usuarios" = "El numero de usuarios ya se conoce (800), no hace falta una muestra para eso. Lo que se desconoce es la satisfaccion.",
              "La regi" = "Eso se podria contar directamente en la poblacion. Relee cual es el objetivo que planteo la empresa.",
              "desarrolladores" = "No tiene relacion con el objetivo planteado por la empresa."
            )),
  p3 = list(correcta = "estratificado",
            incorrectas = list(
              "aleatorio simple" = "Con muestreo aleatorio simple no se controla cuantos usuarios de cada plan caen en la muestra, y podria quedar desbalanceada. Busca un metodo que asegure la proporcion.",
              "conveniencia" = "El muestreo por conveniencia toma a quien es mas facil de alcanzar y no usa proporciones. No garantiza representar a ambos planes.",
              "sistem" = "El muestreo sistematico elige cada k-esimo elemento de una lista, no separa a los usuarios por plan."
            )),
  p4 = list(correcta = "reduce la variabilidad",
            incorrectas = list(
              "de programar en R" = "La facilidad para programarlo no es una razon estadistica. La justificacion debe explicar que problema del muestreo resuelve el metodo.",
              "siempre da resultados" = "Ninguna tecnica es mejor en todos los casos. El estratificado ayuda cuando los grupos difieren entre si, que es lo que ocurre aqui.",
              "Premium tiene usuarios" = "El muestreo no decide que usuarios importan mas, busca representar a cada grupo segun su proporcion real."
            )),
  p5 = list(correcta = "se aleja del verdadero",
            incorrectas = list(
              "Ninguno" = "Una sola muestra es solo una de muchas muestras posibles, y su media puede diferir de la media de la poblacion por azar.",
              "no sirve para este tipo" = "El muestreo estratificado si es valido para estos datos. El problema de una sola muestra es otro, la variacion por azar entre muestras."
            )),
  p6 = list(correcta = "menos estable",
            incorrectas = list(
              "Ninguno" = "Revisa el paso 8. Los errores estandar fueron distintos, asi que el resultado no habria sido igual de estable.",
              "en R habr" = "La dificultad del codigo no es un riesgo estadistico del problema.",
              "La poblaci" = "El metodo de muestreo no cambia la poblacion, que sigue siendo los 800 usuarios."
            )),
  p7 = list(correcta = "Aumentar el tama",
            incorrectas = list(
              "mismos 40" = "Elegir siempre los mismos usuarios no reduce el error estandar. Solo repetiria una misma muestra y ya no seria aleatoria.",
              "semillas" = "La semilla solo hace reproducible el resultado, no cambia que tan dispersas estan las medias."
            ))
)

# ---------------------------------------------------------------
# Utilidades
# ---------------------------------------------------------------
.pista <- function(msg) {
  cat("\n", msg, "\n", sep = "")
  FALSE
}

# Falla con diagnostico especifico (si lo hay) y, si la pregunta tiene escalera, la pista del intento actual
.falla <- function(id, diagnostico = NULL) {
  k <- .intento(id)
  if (!is.null(diagnostico)) cat("\n", diagnostico, "\n", sep = "")
  esc <- .escaleras[[id]]
  if (!is.null(esc)) {
    j <- min(k, length(esc))
    cat("\nPista ", j, " de ", length(esc), ": ", esc[j], "\n", sep = "")
  }
  FALSE
}

.evalua <- function(texto) eval(parse(text = texto), envir = globalenv())

.existe <- function(nombre) exists(nombre, envir = globalenv(), inherits = FALSE)

# ---------------------------------------------------------------
# Pruebas
# ---------------------------------------------------------------

# Opcion multiple: acepta la correcta y explica por que falla cada distractor
opcion_mc <- function(id) {
  e <- get("e", parent.frame())
  tryCatch({
    elegida <- as.character(e$val)
    cfg <- .retro_mc[[id]]
    if (grepl(cfg$correcta, elegida, fixed = TRUE)) return(TRUE)
    for (patron in names(cfg$incorrectas)) {
      if (grepl(patron, elegida, fixed = TRUE)) return(.pista(cfg$incorrectas[[patron]]))
    }
    FALSE
  }, error = function(err) FALSE)
}

# El valor devuelto por la instruccion coincide con el de la expresion de referencia
val_igual <- function(texto) {
  e <- get("e", parent.frame())
  tryCatch({
    ok <- isTRUE(all.equal(unname(e$val), unname(.evalua(texto))))
    if (!ok) .pista("El valor mostrado no coincide con el esperado. Revisa el nombre de los objetos que escribiste.") else TRUE
  }, error = function(err) FALSE)
}

# El objeto 'nombre' existe y su valor coincide con el de la expresion de referencia
var_igual_a <- function(nombre, texto) {
  tryCatch({
    if (!.existe(nombre)) return(.pista(paste0("Todavia no existe el objeto '", nombre, "'. Recuerda guardar el resultado con <-")))
    ok <- isTRUE(all.equal(unname(get(nombre, envir = globalenv())), unname(.evalua(texto))))
    if (!ok) return(.falla(nombre, paste0("'", nombre, "' existe, pero su valor no es el esperado. Revisa la operacion.")))
    .reinicia(nombre)
    TRUE
  }, error = function(err) FALSE)
}

# Muestra valida: existe, es numerica, tiene el tamano pedido y proviene de la poblacion
muestra_valida <- function(nombre, tam, pob, func = NULL) {
  e <- get("e", parent.frame())
  tryCatch({
    if (!.existe(nombre)) return(.pista(paste0("Todavia no existe el objeto '", nombre, "'. Recuerda guardar el resultado con <-")))
    x <- get(nombre, envir = globalenv())
    tam <- .evalua(tam)
    pob <- .evalua(pob)
    if (!is.numeric(x)) return(.falla(nombre, "Tu muestra debe ser un vector numerico de calificaciones. Selecciona la columna satisfaccion con $."))
    if (length(x) != tam) return(.falla(nombre, paste0("Tu muestra tiene ", length(x), " valores y deberia tener ", tam, ". Revisa el tamano que le das a sample().")))
    if (!all(x %in% pob)) return(.falla(nombre, "Algunos valores no provienen del estrato o poblacion indicados. Revisa de que objeto estas muestreando."))
    if (!is.null(func) && !any(func %in% all.names(e$expr))) return(.falla(nombre, paste0("Usa la funcion ", paste(func, collapse = " o "), " para resolver este paso.")))
    .reinicia(nombre)
    TRUE
  }, error = function(err) FALSE)
}

# Estrato valido: data frame filtrado por plan, con todos sus usuarios
estrato_valido <- function(nombre, plan) {
  tryCatch({
    if (!.existe(nombre)) return(.pista(paste0("Todavia no existe el objeto '", nombre, "'. Recuerda guardar el resultado con <-")))
    x <- get(nombre, envir = globalenv())
    if (!is.data.frame(x)) return(.pista("El estrato debe ser un data frame completo (no solo una columna). Usa subset() sobre usuarios_app."))
    if (!all(x$plan == plan)) return(.pista(paste0("El estrato debe contener solo usuarios del plan ", plan, ". Revisa la condicion de subset().")))
    if (nrow(x) != sum(usuarios_app$plan == plan)) return(.pista("Faltan usuarios en el estrato. Revisa la condicion de subset()."))
    TRUE
  }, error = function(err) FALSE)
}

# Muestra estratificada: combina las dos muestras por estrato (sin importar el orden)
estratificada_valida <- function() {
  tryCatch({
    if (!.existe("muestra_estratificada")) return(.pista("Todavia no existe 'muestra_estratificada'. Recuerda guardar el resultado con <-"))
    x <- get("muestra_estratificada", envir = globalenv())
    ref <- c(muestra_gratuito, muestra_premium)
    if (length(x) != length(ref)) return(.pista("La muestra final debe incluir TODOS los valores de muestra_gratuito y de muestra_premium."))
    isTRUE(all.equal(sort(x), sort(ref)))
  }, error = function(err) FALSE)
}

# Simulacion con replicate(): estructura, rango y cercania a la media poblacional
replicas_validas <- function(nombre, n_rep, sd_ref = NULL, tol = 0.35) {
  e <- get("e", parent.frame())
  tryCatch({
    if (!.existe(nombre)) return(.pista(paste0("Todavia no existe el objeto '", nombre, "'. Recuerda guardar el resultado con <-")))
    x <- get(nombre, envir = globalenv())
    n_rep <- .evalua(n_rep)
    usadas <- all.names(e$expr)
    if (!all(c("replicate", "sample", "mean") %in% usadas)) return(.falla(nombre, "Dentro de replicate() debes combinar sample() para tomar la muestra y mean() para calcular su media."))
    if (!is.numeric(x) || length(x) != n_rep) return(.falla(nombre, paste0("Debes obtener ", n_rep, " medias. Revisa el primer argumento de replicate().")))
    if (any(x < min(usuarios_app$satisfaccion) | x > max(usuarios_app$satisfaccion))) return(.falla(nombre, "Alguna media queda fuera del rango de la escala. Revisa que muestreas satisfaccion."))
    if (abs(mean(x) - media_poblacional) > 0.1) return(.falla(nombre, "El promedio de tus medias se aleja demasiado de la media poblacional. Revisa los tamanos de muestra que usas dentro de sample()."))
    if (!is.null(sd_ref)) {
      sd_ref <- .evalua(sd_ref)
      if (abs(sd(x) - sd_ref) > tol * sd_ref) return(.falla(nombre, "La variabilidad de tus medias no es la esperada. Verifica que cada muestra tenga el tamano indicado."))
    }
    .reinicia(nombre)
    TRUE
  }, error = function(err) FALSE)
}