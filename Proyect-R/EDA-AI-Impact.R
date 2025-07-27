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
df <- read_csv("../ai_job_trends_dataset.csv") |> 
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
#Tomamos de ejemplo el salario medio
summary(df$median_salary_usd)
sd(df$median_salary_usd, na.rm = TRUE)


# ---- 8. Gráficos con ggplot2 ----

# Gráfico de barras: industrias con más registros
grafico_industria_impacto <- df |>
  count(industry, ai_impact_level) |>
  ggplot(aes(x = reorder(industry, -n), y = n, fill = ai_impact_level)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Nivel de impacto de la IA por industria",
    x = "Industria",
    y = "Cantidad de puestos",
    fill = "Impacto IA"
  )

#Para guardar la imagen del grafico
ggsave("grafico1_industria_impacto.png", grafico_industria_impacto, width = 9, height = 6)


# Grafico de salario por nivel de impacto
grafico_salario_impacto <- ggplot(df, aes(x = ai_impact_level, y = median_salary_usd, fill = ai_impact_level)) +
  geom_boxplot() +
  labs(
    title = "Salario mediano según nivel de impacto de la IA",
    x = "Nivel de impacto",
    y = "Salario mediano (USD)"
  ) +
  theme(legend.position = "none")

ggsave("grafico2_salario_impacto.png", grafico_salario_impacto, width = 7, height = 5)


# Grafico de impacto de la ia segund nivel educativo
grafico_educacion_impacto <- df |>
  count(required_education, ai_impact_level) |>
  ggplot(aes(x = reorder(required_education, -n), y = n, fill = ai_impact_level)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Distribución del impacto IA según educación requerida",
    x = "Nivel educativo",
    y = "Proporción de trabajos",
    fill = "Impacto IA"
  )

ggsave("grafico3_educacion_impacto.png", grafico_educacion_impacto, width = 9, height = 6)
