# initLesson.R METODO SISTEMATICO
# ------------------------------------------------------------------

# Semilla para que la población simulada sea reproducible
set.seed(123)

# Población: altura (en metros) de 200 árboles medidos de forma
# CONSECUTIVA a lo largo de una transecta forestal de 1000 m
# (un árbol medido aproximadamente cada 5 metros).
alturas_arboles <- round(rnorm(200, mean = 15, sd = 3), 1)

# Tamaño de la población (N)
N <- length(alturas_arboles)

# Tamaño de muestra que el equipo de campo puede medir con el
# tiempo y personal disponible (n)
n <- 20

# Promedio real de la población (solo para comparar al final,
# el estudiante en la vida real NO tendría este dato)
promedio_poblacional <- round(mean(alturas_arboles), 2)


# initLesson.R METODO POR CONGLOMERADOS
#Creacion de un vector para poder usarlo en el ejemplo de muestreo conglomerado de dos etapas
alum_ICB <- c(1:4500)

#definimos el valor de sucurcal_3
sucursal_3 <- c(1:2000)