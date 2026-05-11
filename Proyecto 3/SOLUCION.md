# Proyecto 3: Predicción de impacto de noticias

![AI Generated](https://ai-label.org/wp-content/uploads/2023/04/AI-Label-Minimal-1.png)

**Grupo 35**
*Sergio Morales y Aroa Iriondo*

---

## 1. Análisis de los datos

### Dependencia lineal y correlación entre los datos
Del análisis inicial y de los modelos generados, se observa que la **dependencia puramente lineal es baja**. Esto queda evidenciado en los valores de *Adjusted R-squared*:
*   **Comentarios:** $R^2$ ajustado de **0.1105** (el modelo explica un 11% de la varianza).
*   **Clicks:** $R^2$ ajustado de **0.071** (el modelo explica apenas un 7% de la varianza).
Ambos problemas presentan un comportamiento altamente no-lineal y con gran cantidad de factores externos (viralidad).
*Variables relevantes probables:* "sánchez", "pp", "españa" en los comentarios; "cómo", "menos" y "según" para los clicks, ya que actúan como "clickbait" o generan gran polarización.

### Justificación de los datos utilizados para generar los modelos
Para generar los modelos se procedió a **eliminar la variable `Texto`** (ya que la regresión no procesa texto natural sin tratar) y la variable **`Etiqueta`** (cuya naturaleza es para clasificación en el siguiente proyecto).
Se utilizaron exclusivamente las 50 características numéricas provenientes del TF-IDF (frecuencia de palabras relevantes).
Además, para evitar el *Target Leakage* (fuga de información), el dataset se dividió en dos:
*   Modelo Comentarios: Se borró la columna "Clicks".
*   Modelo Clicks: Se borró la columna "Comentarios".

---

## 2. Modelos de regresión lineal

### Modelo: Comentarios
*   **Error medio absoluto del modelo (MAE):** 30.9012 comentarios de desvío promedio.
*   **Coeficientes y relevancia:**
    *   *Positivos relevantes:* `sánchez` (88.13), `pp` (56.62), `vox` (48.65), `millones` (47.38), `españa` (35.02).
    *   *Significatividad estadística (p-value):* Las palabras estadísticamente más significativas (p < 0.05) son `sánchez`, `pp`, `vox`, `españa` y `millones`. Muestran la gran tendencia política y económica que suscita el debate en Menéame.

### Modelo: Clicks
*   **Error medio absoluto del modelo (MAE):** 775.7367 clicks de desvío promedio.
*   **Coeficientes y relevancia:**
    *   *Variables relevantes:* `cómo` (2187.25), `menos` (1270.43), `según` (-1280.84), `israel` (-991.25), `dos` (-952.76).
    *   *Significatividad estadística:* Solo `cómo`, `menos`, `según` e `israel` lograron significancia (p < 0.05). Refleja que las palabras de formato "clickbait" relativas a guías ("cómo...") incitan fuertemente el interés del usuario por abrir la noticia.

---

## 3. Cuestiones teóricas

### Palabras con mayor coeficiente negativo
Los valores que más penalizan a la predicción:
*   **Comentarios:** `epstein` (-33.60), `tras` (-21.66), `historia` (-19.90), `gran` (-18.70), `guerra` (-18.59), `caso` (-18.04), `irán` (-16.99), `cómo` (-16.89), `israel` (-15.76), `X000` (-15.54).
*   **Clicks:** `según` (-1280.83), `epstein` (-997.63), `israel` (-991.25), `dos` (-952.76), `partido` (-936.77), `política` (-704.55), `historia` (-685.54), `personas` (-678.35), `ahora` (-576.12), `gobierno` (-569.05).

### Palabras con menor coeficiente negativo
Las palabras casi irrelevantes o que restan de manera ínfima:
*   **Comentarios:** `gobierno` (-0.13), `ahora` (-0.95), `eeuu` (-2.98), `menos` (-3.55), `mundo` (-3.62), `años` (-3.70), `medio` (-3.92), `datos` (-6.64), `empresa` (-7.58), `trump` (-7.73).
*   **Clicks:** `donald` (-8.21), `trump` (-11.57), `eeuu` (-35.03), `medio` (-48.68), `irán` (-76.45), `tras` (-141.31), `ia` (-165.61), `presidente` (-191.95), `sistema` (-196.32), `millones` (-203.06).

### Noticias “más sorprendentes” para cada modelo

**Comentarios:**
1.  **Top 5 con diferencia positiva (Real > Predicción):** Instancias donde la realidad superó con creces lo que el modelo dictaminó. 
    *   Ejemplo: El índice Nº 358 logró 128 comentarios, pero carecía de "buzzwords" típicas (sólo tenía la palabra *'sistema'*).
    *   Noticias `358`, `353`, `211`, `385`, `505`. (Comentarios reales de 111 a 161 superando enormemente los cálculos).
2.  **Top 5 con diferencia negativa (Real < Predicción):** Artículos donde el modelo predijo masiva polarización de comentarios debido a palabras fuertes, y acabaron siendo un fracaso rotundo (fueron ignorados por los usuarios).
    *   Ejemplo: El índice Nº 238 combinó `millones`, `pp` y `empresa` –que suelen ser muy polémicas– pero paradójicamente no generó impacto real en la comunidad (apenas 44 comentarios).
    *   Noticias `132`, `217`, `512`, `79`, `238`. (Todas recolectaron 44 o menos comentarios).

**Clicks:**
1.  **Top 5 con diferencia positiva (Real > Predicción):**
    *   La noticia índice Nº 421 ostenta el récord absoluto de visitas indetectadas. Llegó a **6251 clicks** en base a palabras como `historia` y `si`. Las variables `116` (5208 clicks), `360`, `129` y `324` completan este top evidenciando que el clickbait puro no depende tanto de la economía sino a apelaciones "personales/narrativas".
2.  **Top 5 con diferencia negativa (Real < Predicción):**
    *   La noticia índice Nº 389 es la mayor decepción del modelo. Mezclaba la palabra clickbait `menos` y falló. De igual forma las noticias `231`, `361`, `152` y `372` apenas consiguieron entre 250 y 982 clicks cuando las variables sugerían algo mucho más explosivo.

### Generación automática de noticias
Se han generado 1000 vectores sintéticos de 50 slots conformados exclusivamente por 5 palabras con valor igual a 1. Ambos modelos han procesado los vectores:

**Top 10 vectores con más comentarios (Vectores polarizantes):**
Las combinaciones que estadísticamente incendiarían la caja de comentarios mezclan política estatal e instituciones.
1.  (281 coms.) $\rightarrow$  `mientras`, `partido`, `política`, `pp`, `sánchez`.
2.  (253 coms.) $\rightarrow$  `ahora`, `millones`, `país`, `pp`, `sánchez`.
3.  (248 coms.) $\rightarrow$  `ahora`, `millones`, `puede`, `sánchez`, `vox`.
4.  (232 coms.) $\rightarrow$  `ia`, `política`, `pp`, `solo`, `sánchez`.
5.  (229 coms.) $\rightarrow$  `X2026`, `españa`, `país`, `sánchez`, `vox`.
*Se hace evidente que mezclar el apellido "Sánchez" junto al "PP/Vox" y cantidades ("millones") son el mejor cebo para comentarios.*

**Top 10 vectores con más clicks (Vectores de curiosidad morbosa):**
Las combinaciones que arrastran masivamente a entrar a la noticia huyen de la política directa en favor del periodismo de impacto.
1.  (5584 clicks) $\rightarrow$  `cómo`, `guerra`, `medio`, `menos`, `sánchez`.
2.  (4991 clicks) $\rightarrow$  `cómo`, `guerra`, `sino`, `unidos`, `vez`.
3.  (4810 clicks) $\rightarrow$  `cómo`, `donald`, `epstein`, `hace`, `menos`.
4.  (4725 clicks) $\rightarrow$  `X000`, `años`, `cómo`, `menos`, `sido`.
5.  (4615 clicks) $\rightarrow$  `españa`, `menos`, `mundo`, `ser`, `sino`.
*La irrupción de la partícula interrogativa "cómo...", unida a adjetivos narrativos o entidades geopolíticas, garantiza las visitas directas.*

---

## 4. Declaración de uso de herramientas de IA

*   **Herramienta:** GitHub Copilot / Gemini 3.1 Pro.
*   **Uso:** Asistencia guiada para el procesado del dataset en código limpio R con la librería `caret`, detección de "Target Leakage" (separación de Clicks y Comentarios), construcción visual de estadísticos y redacción técnica de explicaciones matemáticas sobre *heterocedasticidad* en modelos multivariables.
*   **Beneficios:** Ha permitido focalizar el problema en el análisis de los coeficientes matemáticos y de su impacto directo, en lugar de perder excesivo tiempo resolviendo la sintaxis de bucles, limpieza de memoria, y formato visual de `ggplot2`. Dificultad principal sorteada: entender que los modelos lineales sufren intrínsecamente para tratar el crecimiento de tipo viral de internet.
