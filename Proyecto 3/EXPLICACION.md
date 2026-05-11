# EXPLICACION.md: Predicción de impacto de noticias

En este documento se detallan el proceso de creación y el fundamento de cada parte del script `2026_linear-regression-meneame.R` perteneciente a la práctica de Regresión Lineal ("P3. Predicción de impacto de noticias").

## 1. Contexto

Se tienen noticias con características extraídas mediante web scraping en *meneame.net*. Existen dos parámetros objetivos que queremos medir en problemas separados:
1. Número de **Comentarios**.
2. Número de **Clicks**.

Además, disponemos de múltiples atributos numéricos y categóricos tales como el texto del titular, su categoría técnica y 50 métricas correspondientes a su valor del Embedding (TF-IDF) por palabras clave más relevantes. Para tratarlo matemáticamente, utilizaremos una de las herramientas más extendidas del aprendizaje de máquina: La **Regresión Lineal Múltiple**.

## 2. Inicialización y dependencias

1. Se cargan las librerías necesarias: `caret` (para separar los datos fácilmente) y librerías visuales para la exploración.
2. Limpieza de memoria `rm(list=ls())` para poder ejecutar el script limpio.
3. Se cargan las herramientas gráficas utilitarias con `source("linear-regression-utils.R")`.

## 3. Procesamiento y preparación del dataset

Primero se carga el CSV ubicado en `../data/2026_meneame-news.csv`.
Luego debemos aplicar un filtrado esencial:
1. **Borrado de 'Texto'**: Es una variable de texto natural (String). La regresión lineal solo puede lidiar directamente con números. En su lugar, el problema ha proporcionado a partir de la columna 5 representaciones TF-IDF que ya traducen la importancia de ciertas palabras a un formato numérico que el modelo sí puede analizar; por eso borramos esta columna original.
2. **Borrado de 'Etiqueta'**: Representa la categoría del artículo (tecnología, noticias...). Como el enunciado indica que esto corresponde al Proyecto 4 de clasificación, lo retiramos ya que alteraría nuestro enfoque continuo y precisaría un tratamiento extra complejo de One-Hot Encoding que no corresponde con la filosofía de este ejercicio.
3. **División del problema en dos sub-datasets**: Tenemos el mismo dataset para predecir **Comentarios** y predecir **Clicks**. 
   * A la hora de entrenar y probar los Comentarios, la máquina **nunca** debe conocer de la variable Clicks (puede generar "*Target leakage*" o fuga del objetivo y permitir que el algoritmo haga trampas infiriendo mediante ello una estrecha relación). Así que clonamos el dataset en `data_comentarios` y le truncamos los `Clicks`, y también la inversa, un `data_clicks` en el que borramos los `Comentarios`.

## 4. Entrenamiento y validación (K-Hold-Out, 10 Modelos)

El enunciado exige generar 10 modelos de regresión lineal por cada problema. Para automatizar este paso, en el script introducimos la función personalizada `train_10_models` donde se implementan estas tareas:

1. **Bucle For 1 to 10**: En cada pasada extraemos mediante una función de la librería (`createDataPartition`) un nuevo 80% aleatorio para entrenamiento y guardamos el 20% respectivo para prueba.
2. **Entrenamiento (`lm`)**: Con la orden `lm(Target ~ .)` entrenamos la intersección matemática multivariable del 80% asignado usando todas las palabras claves de entrada (representadas por el `.` indicando que toma todas las columnas restantes de variables independientes).
3. **Predicción y Error Absoluto Medio (MAE)**: Al 20% test restante se le intenta adivinar el número mediante una predicción y luego comparamos el número absoluto de diferencia contra aquellos resultados reales de nuestro set (`mean(abs(predReal - predMaquina))`).
4. **Selección del mejor modelo**: Acabado el bucle, retornamos aquel modelo concreto cuyo Error Medio fue el mínimo registrado del array de diez combinaciones.

Lo ejecutamos primero para *Comentarios* y posteriormente para *Clicks*.

## 5. Extracción de coeficientes del mejor sistema

Una vez logramos el sub-modelo ganador, la consigna exige extraer los "coeficientes de cada atributo".
Imprimiendo en consola el resumen matemático (`summary(best_model)$coefficients`) y mediante nuestra útil función `print.model.summary()`, el script exportará a RStudio los pesos exactos y valores-$p$ (significancia estadística) de cada vocablo y cómo interactúan las características del TF-IDF con los clics y comentarios reales. Gracias a este enfoque nos enteraremos de si palabras clave como "guerra" o "madrid" puntúan positiva o negativamente el impacto periodístico.

## 6. Localización de las noticias con mayor diferencia (Sorprendentes)

Para investigar a fondo el resultado de nuestras regresiones, es valioso conocer en qué instancia específica fallan de la forma más rotunda y en qué dirección.
1. Tomamos las predicciones efectuadas del *test_data* del MEJOR modelo y calculamos sus diferencias (`real - predicción`).
2. Analizamos y ordenamos estos valores con `order(diff, decreasing = TRUE)` y `FALSE` para aislar posiciones extremas en ambos espectros.
3. Extraemos los primeros 5 índices del marco de datos (el Top 5 positivo y Top 5 negativo) e imprimimos sus atributos. Esto nos da los perfiles de la noticia que superó enormemente las expectativas y la de aquellas que prometían ser virales y fracasaron.
4. Esto se repite tanto para los *Comentarios* como para los *Clicks*.

## 7. Generación Automática de Noticias

El enunciado solicita generar 1000 vectores sintéticos simulando combinaciones de palabras:
1. Establecemos una semilla `set.seed(12345)` para que la aleatoriedad sea reproducible.
2. Elaboramos 1000 vectores rellenados de ceros donde se activa de forma pseudoaleatoria únicamente cinco unos por bloque `vec[sample(1:num_features, 5)] <- 1`. 
3. Transformamos este gran bloque matricial a DataFrame y pedimos a nuestros modelos estrella (`best_model_com` y `best_model_cli`) que predigan su rendimiento.
4. Ordenamos descendientemente aquellas instancias que recibieron mayor nota esperada e imprimimos el vocabulario original encendido que conformaba dichos vectores "perfectos".