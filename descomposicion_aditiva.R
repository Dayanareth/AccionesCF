# DESCOMPOSICION ADITIVA CLASICA
# TOAQUIZA YAJAIRA

library(fpp3)
library(slider)

# 1. Leer los datos
datos <- read_csv("data/data.csv")
# Comprobar los saltos entre fechas

datos |>
  arrange(FECHA) |>
  mutate(
    dias_entre_fechas = as.integer(FECHA - dplyr::lag(FECHA))
  ) |>
  dplyr::filter(dias_entre_fechas > 1) |>
  select(FECHA, dias_entre_fechas) |>
  head(10)

# 2. Ordenar por fecha y numerar las observaciones
# Como la base presenta saltos entre las fechas, se crea la
# columna observacion para obtener un indice consecutivo.
# Para la descomposicion se supone un periodo m = 5,
# correspondiente a cinco observaciones consecutivas.
# Esto no representa necesariamente una semana calendario,
# ya que existen dias sin registros. Se conserva FECHA
# para identificar las fechas originales.

datos <- datos |>
  arrange(FECHA) |>
  mutate(observacion = row_number()) |>
  as_tsibble(index = observacion)

datos

# PASO 1: Estimar la tendencia-ciclo
# Se utiliza una media movil centrada de orden 5 porque
# se ha supuesto un periodo de cinco observaciones (m = 5).
# Como m es impar, se toman dos valores anteriores,
# el valor actual y dos posteriores.
datos_ma <- datos |>
  mutate(
    `5-MA` = slider::slide_dbl(
      PRECIO_PROMEDIO,
      mean,
      .before = 2,
      .after = 2,
      .complete = TRUE
    )
  )

datos_ma
# Grafica de la serie original y la tendencia
datos_ma |>
  autoplot(PRECIO_PROMEDIO) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(
    x = "Número de observación",
    y = "Precio (USD)",
    title = "Media móvil de orden 5 - Corporación Favorita"
  )


# PASO 2: Eliminar la tendencia

datos_ma <- datos_ma |>
  mutate(
    sin_tendencia = PRECIO_PROMEDIO - `5-MA`
  )

datos_ma

datos_ma |>
  autoplot(sin_tendencia) +
  labs(
    x = "Número de observación",
    y = "Precio sin tendencia (USD)",
    title = "Serie sin tendencia - Corporación Favorita"
  )

# PASO 3: Calcular la componente estacional

# Identificar las 5 posiciones del ciclo
datos_ma <- datos_ma |>
  mutate(
    estacion = (observacion - 1) %% 5 + 1
  )

# Calcular el promedio estacional
datos_ma <- datos_ma |>
  group_by(estacion) |>
  mutate(
    media_estacional = mean(sin_tendencia, na.rm = TRUE)
  ) |>
  ungroup()

datos_ma

# Ajustar las medias estacionales para que sumen cero

# Obtener los 5 promedios estacionales
medias <- datos_ma |>
  distinct(estacion, media_estacional)

# Calcular el promedio de esos 5 valores
ajuste <- mean(medias$media_estacional)

# Obtener la componente estacional ajustada
datos_ma <- datos_ma |>
  mutate(
    estacional = media_estacional - ajuste
  )

# Revisar los resultados
datos_ma |>
  distinct(estacion, estacional)

# Comprobar que las componentes estacionales sumen cero

datos_ma |>
  distinct(estacion, estacional) |>
  summarise(suma = sum(estacional))

# PASO 4: Calcular la componente restante

datos_ma <- datos_ma |>
  mutate(
    restante = sin_tendencia - estacional
  )

datos_ma
# Grafica de la componente restante

datos_ma |>
  autoplot(restante) +
  labs(
    x = "Número de observación",
    y = "Componente restante (USD)",
    title = "Componente restante - Corporación Favorita"
  )



