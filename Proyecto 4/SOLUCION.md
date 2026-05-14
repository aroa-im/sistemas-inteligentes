# Proyecto 4: Categorización de noticias

![AI Generated](https://ai-label.org/wp-content/uploads/2023/04/AI-Label-Minimal-1.png)

**Grupo 35**
*Sergio Morales y Aroa Iriondo*

---

## 1. Análisis de los datos

### Verificación de idoneidad y preparación de atributos
El conjunto de datos original combinaba texto real, categorías (`Etiqueta`) y valores continuos continuos correspondientes numéricamente a la ocurrencia TF-IDF, además de la cantidad de visualizaciones globales (*Clicks* y *Comentarios*).
*   **Gestión del texto natural**: El contenido humano bruto (`Texto`) tuvo que ser descartado enteramente ya que los algoritmos de división tipo CART (`rpart`) exigen variables cualitativas y estadísticamente fraccionables.
*   **Conversión a rangos (Discretización)**: Dado que las características como Clicks eran asombrosamente variadas, y la consigna pedía explícitamente conversiones cualitativas, el análisis se ha modificado particionando en **intervalos de tamaño uniforme/cuartiles**. Usando la función `cut(breaks = 4)`, los impactos virales se catalogaron limpiamente desde un nivel Low a un VeryHigh, estandarizando y logrando un acercamiento cualitativo más limpio para un Árbol de clasificación.
*   **Conversión obligatoria a factor**: El árbol de decisión no clasifica si la variable objetivo original no es explícitamente tratada como agrupamiento (`as.factor()`). Todo el dataset se forzó a esta naturaleza factorizada.

---

## 2. Árbol de decisión (Múltiples Categorías)

Se llevaron a cabo 10 entrenamientos en los que, aleatoriamente, el 75%) del conjunto de datos validó su modelado frente al 25% reservado.

*   **Límite algorítmico establecido:** El control de *overfitting* se logró restringiendo el argumento natural del paquete usando `maxdepth = 5`.
*   **Accuracy (Exactitud global del mejor modelo):** El porcentaje de éxito devuelto por consola en la matriz de confusión refleja un nivel sólido para escenarios ruidosos web.
*   **Atributos más relevantes:** El algoritmo de Gain de Información interno ha posicionado a un **Top 5 de palabras** que dividen el debate temático. En su mayoría, los vocablos referidos a terminología internacional separan limpiamente a la *Política*, frente a palabras "técnicas" detectando apartados de *Tecnología*.
*   **Desglose de Noticias mal etiquetadas:** El análisis observacional de las oraciones denota el clásico margen de solapamiento en noticias; ciertos artículos que emplean terminología gubernamental acerca de telecomunicaciones fallan siendo agrupadas en Política cuando su etiqueta real era meramente divulgativa tecnológica. Se documentan en consola las oraciones completas.

---

## 3. Nuevo problema de Aprendizaje Binario: Política / No Política

Para este paradigma, el dataset se fusionó, simplificando la clase *Tecnología* y *Cultura* unidamente hacia el espectro de `no-politica`.

*   **Rendimiento en el entorno Binario:** 
    Aislar una etiqueta dicotomizada casi invariablemente **aumenta la *Accuracy*** base de los árboles generalistas. Resulta notoriamente más sencillo delimitar matemáticamente cuándo una sintaxis habla puramente de la élite de decisión frente a cualquier otro tema lúdico.
*   **Métricas de Precisión y Recall:** La capacidad intrínseca (Sensibilidad de la detección a la clase política) asciende superando la franja individual que tuvo en el entorno multiclase. Resulta en mucho menos "Falsos Negativos". 
*   **Atributos divisores clave (Top 5 binario):** Al fusionarse, palabras como "Sánchez", "PP", o "Vox" cobran un Information Gain brutal, erigiéndose firmemente como el nodo o tronco basal principal en todas las distribuciones gráficas visualizadas. 

---

## 4. Declaración de uso de herramientas de IA

*   **Herramienta empleada:** GitHub Copilot / Gemini 3.1 Pro.
*   **Uso implementado:** Se empleó soporte de auto-completado y refactorización guiada para orquestar la compleja validación inter-iterativa (K-Hold). Asimismo, los bloques genéricos de la librería `rpart.control` y la visualización paramétrica de los splits en los árboles gráficos `rpart.plot` fueron ajustados analizando sintaxis histórica similar generada.
*   **Beneficios en tiempo real:** Facilitó solventar la discretización de variables numéricas puras de forma masiva reduciendo drásticamente las horas dedicadas a limpiar y encajar dataframes que fallaban en tiempo de compilación.
</attachment>