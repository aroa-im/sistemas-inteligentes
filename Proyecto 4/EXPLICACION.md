# EXPLICACION.md: Categorización de noticias con Árboles de Decisión

En este documento se detalla el flujo de trabajo para el proyecto número 4 ("P4. Categorización de noticias"), desarrollado analizando la base de datos `2026_meneame-news.csv` empleando la técnica de aprendizaje supervisado conocida como Árboles de Decisión (`rpart`).

## 1. Procesamiento y Preparación del Dataset

El enunciado exige clasificar y predecir automáticamente a qué **Etiqueta** (categoría temática) corresponde una noticia basándose en el TF-IDF y el comportamiento.

1. **Borrado de la columna de texto libre (`Texto`)**: Al igual que en problemas anteriores, el texto libre del titular no se puede introducir de forma plana en un modelo estadístico. Aislamos las características apoyándonos exclusivamente en los contadores y las variables representacionales.
2. **Conversión a valores cualitativos**: Los Árboles de Decisión operan intrínsecamente separando condiciones ("splits"), por la cual las clases necesitan formatos factoriales. 
   - Se aplicó un redondeo por cuantiles (la función `cut` con 4 roturas) a las variables continuas numéricas originales como **Comentarios** y **Clicks**, catalogando las noticias de un rango de impacto "Low" a "VeryHigh". 
   - Tras este truncamiento, forzamos toda la tabla a transformarse a formato Factor de R (`lapply(data, as.factor)`).

## 2. Generación iterativa de modelos (Múltiples Etiquetas)

El dataset se ha introducido en un bucle que valida el entrenamiento hasta 10 veces:
1. Extraemos particiones K-Hold-Out del `75%` del dataset para Training y `25%` para validación Test (`createDataPartition`).
2. Generamos el modelo de árbol con `rpart()`. Para cumplir el requisito de control y prevenir *Overfitting* (Sobreentrenamiento), recortamos la profundidad máxima con el argumento `maxdepth = 5`.
3. Validamos cada versión contra el set de pruebas computando la *Matrix de Confusión*. El algoritmo captura aquella iteración cuyo valor puro de `Accuracy` (Precisión Global) es el máximo obtenido.

## 3. Extracción de Métricas y Reglas 

Una vez tenemos el mejor de los 10 modelos construidos, generamos su disección:
1. **Accuracy**: El porcentaje general de instancias que el nodo predijo bien. 
2. **Precision & Recall por Clases**: Para obtener cuánta exactitud de Falsos Positivos o Falsos Negativos tuvo concretamente clasificando si era de "Cultura", "Política" o "Tecnología".
3. **Ploteo Visual**: Exportamos y plasmamos el árbol con un alto detalle visual (`rpart.plot()`).
4. **Relevant Attributes**: Extraemos la relevancia variable subyacente del sistema para saber qué palabras o número de comentarios fueron las palancas divisorias principales que hicieron saltar a un nodo a decantarse por una categoría.

## 4. Análisis de Fallos ("Mal etiquetadas")
El script filtra exclusivamente de la matriz de prueba (test de 25%) y del mejor modelo aquellas instancias donde `Test != Prediction`. Toma esas diferencias, se mapea devuelta al CSV original con los textos integrados y se imprime la lista de artículos que despistaron completamente a la IA.

## 5. El problema de Aprendizaje Binario (Política / No-Política)
Finalmente, adaptamos la etiqueta para polarizar su naturaleza. Usando el transformante `ifelse()`, todos los casos de *Tecnología* y *Cultura* se renombran como "no-politica". La estructura de datos procesa una variable dicotómica donde repetimos los mismos procesos anteriores (Training, modelaje e impresión de *Precision* / *Recall*) para validar si este agrupamiento simplificado facilita o empeora el comportamiento matemático subyacente de nuestro Árbol.