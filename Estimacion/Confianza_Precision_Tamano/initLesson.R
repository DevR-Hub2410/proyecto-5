# Datos de la lección Confianza_Precision_Tamano
# set.seed() hace que los datos simulados sean siempre los mismos 
set.seed(123)

# Muestra de referencia: tiempos de respuesta de un servidor (ms), n = 40
tiempos_servidor <- round(rnorm(40, mean = 250, sd = 40), 1)

media_muestral <- mean(tiempos_servidor)
sd_muestral <- sd(tiempos_servidor)
n_muestra <- length(tiempos_servidor)

# Valores z para los niveles de confianza más usados 
z_90 <- qnorm(0.95)
z_95 <- qnorm(0.975)
z_99 <- qnorm(0.995)
