# =============================================
# Proyecto EDA - Impacto de la IA en el mercado laboral (2024-2030)
# Dataset: https://www.kaggle.com/datasets/sahilislam007/ai-impact-on-job-market-20242030
# =============================================


# ---- 1. Carga de paquetes ----
library(tidyverse)     # Libreria recomendada en clase
library(readr)         # Para poder leer CSV
library(janitor)       # Limpiar el dataser
library(ggplot2)       # Para poder visulizar los datos con graficas


# ---- 2. Carga de datos ----
df <- read_csv("ai_job_market.csv") |> 
  clean_names()


# ---- 3. Análisis inicial ----

# Ver dimensiones
dim(df)  # número de filas y columnas

# Primeras filas
head(df)

# Ver estructura de los datos
str(df)

# ---- 4. Detección de duplicados ----
df <- df |> distinct()


# ---- 5. Detección de valores faltantes ----
colSums(is.na(df))


# ---- 6. Cuadro de frecuencias (variable categórica) ----
#Tomamos de ejemplo la variables categorica Industry
industry_freq <- df |> 
  count(industry) |> 
  mutate(relative_freq = round(n / sum(n), 3))

print(industry_freq)


# ---- 7. Estadísticos descriptivos (variable continua) ----
#Tomamos de ejemplo porcentaje de automatizacion
summary(df$percentage_of_automation)
sd(df$percentage_of_automation, na.rm = TRUE)


# ---- 8. Gráficos con ggplot2 ----

# Gráfico de barras: industrias con más registros
ggplot(df, aes(x = fct_infreq(industry))) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Frecuencia por industria",
       x = "Industria", y = "Cantidad")


# Gráfico de dispersión: % automatización vs crecimiento
ggplot(df, aes(x = percentage_of_automation, y = projected_job_growth)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Automatización vs Crecimiento Proyectado",
       x = "% de automatización",
       y = "Crecimiento de empleo proyectado")


# Correlación
cor(df$percentage_of_automation, df$projected_job_growth, use = "complete.obs")


# Boxplot: automatización por nivel de riesgo
ggplot(df, aes(x = risk_level, y = percentage_of_automation)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Automatización según nivel de riesgo",
       x = "Nivel de riesgo",
       y = "% de automatización")

# ---- 9. Guardar gráficos (Si es que usted desea, igual en el repositorio van a estar las imagenes) ----
ggsave("grafico_barras_industria.png")
ggsave("grafico_dispersion.png")
ggsave("boxplot_riesgo.png")