# Curso Estadistica

Curso interactivo de Estadistica para R, construido con el paquete **swirl**. Se aprende dentro de la consola de RStudio: cada lección explica un concepto, hace preguntas, pide ejecutar código en R y da retroalimentación y pistas (*hints*).

Proyecto 6 de la materia Estadística (Universidad Autónoma de Ciudad Juárez, Ingeniería de Software).
Docente: Dra. Liliana Orizel Martínez Martínez.

## Integrantes 

Proyecto realizado por la unión del Equipo K y del Equipo D.

| Equipo | Integrantes |
|---|---|
| K | Zyanya Tajani Mireles Gaspar, Rafael Castillo Villalba |
| D | Gabriela Carrillo Navarrete, Yamileth Jalil Calderon, Amanda Sofía López Escárcega |

## Descripción general

El curso integra y mejora los productos de los Proyecto 1, 2 y 3 en un solo recorrido, ademas de agregar conceptos mas avanzados de estadistica como las pruebas de hipotesis, bondad de ajuste e independencia, correlacion y regrecion lineal y ANOVA de un factor. Está pensado para que lo pueda tomar una persona **sin experiencia previa en R ni RStudio**: la primera lección enseña lo mínimo necesario para poder seguir las demás. 

Todas las lecciones siguen la secuencia **explicación, pregunta, práctica en R, interpretación**, y usan ejemplo de ingeniería de software (tiempos de respuesta, usuarios, errores en módulos, etc.).

## Contenido del curso 

| Orden | Lección | Qué se aprende |
|---|---|---|
| 0 | `Conociendo_R_Y_RStudio` | Qué son R y RStudio, las áreas de la interfaz, la consola, operaciones básicas, objetos, vectores, funciones, cómo funciona swirl y cómo usar los *hints* |
| 1 | `Unidad_1_Estadistica_Descriptiva/Leccion_1_Datos_y_variables` | Población, muestra, parámetro, estadístico, datos y tipos de variables |
| 2 | `Unidad_1_Estadistica_Descriptiva/Leccion_2_Analisis_de_datos` | Frecuencias, tablas de frecuencias, gráficas y medidas de tendencia central |
| 3 | `Unidad_1_Estadistica_Descriptiva/Leccion_3_Dispersion_de_datos` | Medidas de dispersión, cuartiles, diagramas de caja, valores atípicos y comparación de conjuntos de datos |
| 4 | `Unidad_2_Muestreo/Leccion_1_Introduccion_al_muestreo` | Por qué se muestrea y términos básicos |
| 5 | `Unidad_2_Muestreo/Leccion_2_Metodos_de_muestreo` | Muestreo aleatorio simple, sistemático, estratificado, por conglomerados y por conveniencia |
| 6 | `Unidad_2_Muestreo/Leccion_3_Distribuciones_Muestrales` | Distribución muestral de la media y de la proporción |
| 7 | `Unidad_2_Muestreo/Leccion_4_Aplicacion_Integral` | Actividad integradora de muestreo estratificado y distribución muestral |
| 8 | `Unidad_3_Estimacion/Leccion_1_Estimacion_Puntual_Intervalos` | Parámetros y estimadores, estimación puntual, intervalos de confianza para una media y una proporción |
| 9 | `Unidad_3_Estimacion/Leccion_2_Confianza_Precision_Tamano` | Nivel de confianza, margen de error, precisión y tamaño de muestra |
| 10 | `Unidad_3_Estimacion/Leccion_3_Estimacion_Dos_Poblaciones` | Intervalos de confianza para la diferencia de medias, la diferencia de proporciones y la razón de varianzas |
| 11 | `Pruebas_De_Hipotesis` | Pruebas de hipótesis para medias, proporciones y varianzas |
| 12 | `Bondad_De_Ajuste_E_Independencia/Introduccion_Bondad_Ajuste` | Introducción a las pruebas de bondad de ajuste e independencia |
| 13 | `Bondad_De_Ajuste_E_Independencia/Bondad_Ajuste_Distribuciones_De_Probabilidad` | Evaluación del ajuste a distribuciones de probabilidad |
| 14 | `Bondad_De_Ajuste_E_Independencia/Pruebas_De_Independencia` | Análisis de la independencia entre variables |
| 15 | `Bondad_De_Ajuste_E_Independencia/Pruebas_De_Homogenidad` | Comparación de proporciones entre poblaciones |
| 16 | `Bondad_De_Ajuste_E_Independencia/Interpretacion_Y_Aplicacion_Practica` | Interpretación y aplicación de pruebas estadísticas |
| 17 | `Correlacion_Y_Regresion_Lineal` | Análisis de correlación y regresión lineal |
| 18 | `ANOVA_De_Un_Factor/Introduccion_ANOVA_De_Un_Factor` | Introducción al análisis de varianza de un factor |
| 19 | `ANOVA_De_Un_Factor/Supuesto_Del_ANOVA` | Supuestos de independencia, normalidad y homogeneidad |
| 20 | `ANOVA_De_Un_Factor/Tabla_ANOVA` | Componentes e interpretación de la tabla ANOVA |
| 21 | `ANOVA_De_Un_Factor/Comparaciones_Multiples` | Comparaciones múltiples y prueba de Tukey |

### Lección inicial: Conociendo R y RStudio

Es una lección breve, práctica e interactiva para quien nunca ha usado R. Explica qué es R y qué es RStudio, cómo reconocer las áreas de la interfaz y dónde está la consola, y cómo escribir y ejecutar instrucciones. Después enseña a crear objetos y vectores, ejecutar funciones básicas e interpretar lo que aparece en la consola. También explica cómo iniciar una lección de swirl, cómo responder y cómo pedir ayduar con los *hints*. No pretende ser un curso de programación en R.

## Estructura del repositorio

```
proyecto-5/
├── README.md
├── MANIFEST                        orden del curso
├── instalar_curso.R                instala el curso en swirl
├── proyecto-5.Rproj
│
├── Conociendo_R_Y_RStudio/         lección inicial
│   ├── lesson.yaml
│   ├── initLesson.R
│   ├── customTests.R
│   └── dependson.txt
│
├── Unidad_1_Estadística descriptiva/                      
│   ├── MANIFEST
│   ├── Leccion_1_Datos_y_variables/
│   ├── Leccion_2_Analisis_de_datos/
│   └── Leccion_3_Dispersion_de_datos/
│
├── Unidad_2_Muestreo/  
│   ├── MANIFEST
│   ├── Leccion_1_Introduccion_al_muestreo/
│   ├── Leccion_2_Metodos_de_muestreo/
│   ├── Leccion_3_Distribuciones_Muestrales/
│   └── Leccion_4_Aplicacion_Integral/
│
└── Unidad_3_Estimacion/
│   ├── MANIFEST
│   ├── Leccion_1_Estimacion_Puntual_Intervalos/
│   ├── Leccion_2_Confianza_Precision_Tamano/     (incluye fig_efecto_n.R)
│   └── Leccion_3_Estimacion_Dos_Poblaciones/
│
├── Pruebas_De_Hipotesis/  
│   ├── lesson.yaml
│   ├── initLesson.R
│   └── customTests.R
│
├── Bondad_De_Ajuste_E_Independencia/  
│   ├── Introduccion_Bondad_Ajuste/
│   ├── Bondad_Ajuste_Distribuciones_De_Probabilidad/
│   ├── Pruebas_De_Independencia/
│   ├── Pruebas_De_Homogenidad/
│   └── Interpretacion_Y_Aplicacion_Practica/
│
├── Correlacion_Y_Regresion_Lineal/  
│   ├── lesson.yaml
│   ├── initLesson.R
│   ├── customTests.R
│   └── dependson.txt
│
├── ANOVA_De_Un_Factor/  
│   ├── MANIFEST
│   ├── Introduccion_ANOVA_De_Un_Factor/
│   ├── Supuestos_Del_ANOVA/
│   ├── Tabla_ANOVA/
│   └── Comparaciones_Multiples/
│
```

Cada lección es autónoma y contiene:

| Archivo | Función |
|---|---|
| `lesson.yaml` | Contenido de la lección: textos, preguntas, ejercicios, pistas y retroalimentación |
| `initLesson.R` | Crea los datos que usa la lección (son simulados, no hay archivos de datos externos) |
| `customTests.R` | Funciones de evaluación que reconocen respuestas correctas y dan mensajes específicos por tipo de error |
| `dependson.txt` | Paquetes adicionales que requiere la lección (opcional; está vacío porque el curso no necesita otros paquetes) |

Por eso el repositorio no tiene carpetas `funciones/` ni `datos/` separadas: las funciones auxiliares viven en el `customTests.R` de cada lección y los datos se generan en su `initLesson.R`, con `set.seed()` para que sean siempre los mismos.

## Requisitos 

- R (versión 4.0 o superior)
- RStudio
- Paquete **swirl** (versión 2.4.5 o superior)
- Conexión a internet solo para instalar R, RStudio y el paquete

El curso no requiere otros paquetes.

## Instalacion 

1. Instala R y RStudio.
2. Descarga o clona este repositorio (en RStudio: *File → New Project → Version Control → Git* y pega la URL del repositorio).
3. Abre el archivo `proyecto-5.Rproj`, para que el directorio de trabajo sea la carpeta raíz del repositorio.
4. En la consola de RStudio, ejecuta:

```r
install.packages("swirl")        # solo la primera vez
source("instalar_curso.R")
```

`instalar_curso.R` arma una copia del curso con todas las lecciones en orden, porque swirl solo muestra como lecciones las carpetas de primer nivel, y la instala como **Curso_Estadistica**. Si ya estaba instalado, lo reemplaza.

## Ejecución


```r
library(swirl)
swirl()
```

1. Escribe tu nombre cuando swirl lo pida.
2. Elige el curso **Curso_Estadistica**.
3. Elige una lección. **Si nunca has usado R, empieza por `Conociendo R y RStudio`** y sigue el orden de la lista.

Comandos útiles dentro de swirl:

| Comando | Qué hace |
|---|---|
| `skip()` | Salta la pregunta actual |
| `info()` | Muestra las opciones disponibles |
| `main()` | Regresa al menú principal |
| `bye()` | Sale de swirl y guarda tu avance |

Si respondes mal, swirl muestra una pista. En las preguntas de código, además, el curso explica qué errores son comunes y cómo corregirlos.

## Para quien desarrolle o modifique el curso 

- Cada lección se prueba con el paquete `swirlify` (`set_lesson()` y `test_lesson()`), pero esa prueba solo revisa la estructura. Además, hay que recorrer la lección como estudiante con `demo_lesson()` y equivocarse a propósito en las preguntas de código.
- Después de cambiar `customTests.R` o `initLesson.R`, hay que salir de swirl y volver a iniciar la lección, porque solo se cargan al comenzar.
- Flujo de trabajo con Git: al empezar la sesión, *Pull*. Después de modificar, *Stage → Commit → Push*, con mensajes que describan el cambio.

## Estudio piloto

El curso se probó con 10 personas antes de generar esta versión. Los resultados, las decisiones y los cambios realizados están en el documento del proyecto. 