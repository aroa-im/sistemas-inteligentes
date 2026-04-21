# -----------------------------------------------------------------------------
# Este fragmento de código ha sido generado con fines educativos como parte de la
# asignatura de Sistemas Inteligentes en la Universidad de Deusto.
# -------------------------------------------------------------------------
#
# Autores / Mantenedores:
#   - Roberto Carballedo
#   - Fernando Boto
#   - Enrique Onieva
#
# Última actualización: Marzo de 2026
# Código generado y revisado usando: Gemini Pro.
#
# Uso exclusivamente educativo — Universidad de Deusto
# =========================================================================

# Cargar librería para visualización de datos
library(ggplot2)

# --- LIMPIEZA DEL ENTORNO ---
cat("\014") # Limpia la consola
if(!is.null(dev.list())) dev.off() # Cierra las ventanas de gráficos
rm(list=ls()) # Limpia las variables del entorno de trabajo

# Establece el directorio de trabajo (requiere rstudioapi)
tryCatch({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, error = function(e) message("Aviso: Guarda el script antes de ejecutar rstudioapi."))


# --- 1. PREPARACIÓN DE LOS DATOS (Training Set) ---

training_set    <- data.frame(x1 = 1:20)
training_set$x0 <- 1 # Variable auxiliar para el "Bias" (término independiente)

# Definimos el "Target" (variable objetivo) basándonos en la función: y = 2x - 3
training_set$target  <- 2 * training_set$x1 - 3

# Añadimos ruido aleatorio uniforme para simular un dataset real
set.seed(123) 
training_set$target <- training_set$target + runif(nrow(training_set), min = -2, max = 2)

# --- 2. INICIALIZACIÓN DEL MODELO ---

# Inicializamos los "Weights" (pesos) del modelo a 0
w0 <- 0 # Bias (punto de corte con el eje Y)
w1 <- 0 # Weight principal (pendiente)

# "Learning rate" (alpha): Tamaño del paso durante el Gradient Descent
alpha <- 0.01 

# Calculamos la "Hypothesis" (predicción) inicial
training_set$hypothesis <- w0 * training_set$x0 + w1 * training_set$x1
training_set$error <- training_set$target - training_set$hypothesis

# Plot 1: Situación inicial
plot_inicial <- ggplot(training_set) +
  geom_point(aes(x = x1, y = target, color = "Target (Datos Reales)"), size = 2) +
  geom_line(aes(x = x1, y = hypothesis, color = "Hypothesis (Modelo)"), linewidth = 1.2) +
  labs(title = "Initial State (Before Training)",
       x = "Feature (x1)", y = "Target",
       caption = sprintf("Weights: w1 = %.3f | w0 (Bias) = %.3f", w1, w0)) +
  scale_color_manual(name = "Leyenda", 
                     values = c("Target (Datos Reales)" = "red", 
                                "Hypothesis (Modelo)" = "blue")) +
  theme_minimal()

print(plot_inicial)

# --- 3. PROCESO DE APRENDIZAJE (Gradient Descent) ---

epochs <- 2000
m <- nrow(training_set)

cat("\n--- Iniciando el Training ---\n")

for (i in 1:epochs) {
  
  # A. ACTUALIZACIÓN DE WEIGHTS (Gradient Descent)
  w1_nuevo <- w1 + alpha * sum(training_set$error * training_set$x1) / m
  w0_nuevo <- w0 + alpha * sum(training_set$error * training_set$x0) / m
  
  w1 <- w1_nuevo
  w0 <- w0_nuevo
  
  # B. RECALCULAR HYPOTHESIS Y ERROR
  training_set$hypothesis <- w0 * training_set$x0 + w1 * training_set$x1
  training_set$error <- training_set$target - training_set$hypothesis
  
  # C. MOSTRAR EL PROGRESO (Logging)
  if (i %% 100 == 0 || i == 1) {
    # Usamos el Mean Absolute Error (MAE) para el log por su sencillez interpretativa
    mae <- mean(abs(training_set$error))
    cat(sprintf("Epoch %4d | w1 = %5.3f | w0 (Bias) = %5.3f | Error (MAE) = %5.3f\n", 
                i, w1, w0, mae))
  }
}

cat("--- Training Completado ---\n\n")

# --- 4. VISUALIZACIÓN FINAL ---

# Plot 2: Modelo ajustado tras el training
plot_final <- ggplot(training_set) +
  geom_point(aes(x = x1, y = target, color = "Target (Datos Reales)"), size = 2) +
  geom_line(aes(x = x1, y = hypothesis, color = "Hypothesis (Modelo)"), linewidth = 1.2) +
  labs(title = paste0("Model after ", epochs, " Epochs"),
       x = "Feature (x1)", y = "Target",
       caption = sprintf("Final Weights: w1 = %.3f | w0 (Bias) = %.3f", w1, w0)) +
  scale_color_manual(name = "Leyenda", 
                     values = c("Target (Datos Reales)" = "red", 
                                "Hypothesis (Modelo)" = "blue")) +
  theme_minimal()

print(plot_final)
