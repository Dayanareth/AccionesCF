library(fpp3)
library(slider)

#Leer los datos y crear el indice consecutivo
#Como hay dias sin registro, se numera cada observacion y se
#Supone un periodo m = 5 (cinco observaciones consecutivas).
datos_mu <- read_csv("data/data.csv") |>
  arrange(FECHA) |>
  mutate(observacion = row_number()) |>
  as_tsibble(index = observacion)

datos_mu

#La descomposicion multiplicativa exige valores positivos
min(datos_mu$PRECIO_PROMEDIO)


#PASO 1: Estimar la tendencia-ciclo
#media movil de 2
datos_mu <- datos_mu |>
  mutate(
    tendencia = slider::slide_dbl(
      PRECIO_PROMEDIO, mean,
      .before = 2, .after = 2,
      .complete = TRUE
    )
  )

datos_mu |>
  autoplot(PRECIO_PROMEDIO, colour = "gray") +
  geom_line(aes(y = tendencia), colour = "#D55E00") +
  labs(title = "Tendencia del precio promedio - Corporacion Favorita",
    x = "Numero de observacion",
    y = "Precio (USD)")


#PASO 2: Eliminar la tendencia
#En el modelo multiplicativo se DIVIDE la serie entre la tendencia.
datos_mu <- datos_mu |> mutate(sin_tend = PRECIO_PROMEDIO / tendencia)

datos_mu |>
  autoplot(sin_tend) +
  labs(title = "Serie sin tendencia (multiplicativa) - Corporacion Favorita",
    x = "Numero de observacion",
    y = "Precio / tendencia")


#PASO 3: Calcular la componente estacional
#Posicion de cada observacion dentro del ciclo de 5
datos_mu <- datos_mu |>
  mutate(posicion = (observacion - 1) %% 5 + 1)

#Promedio de la serie sin tendencia para cada posicion
prom_pos <- datos_mu |>
  as_tibble() |>
  group_by(posicion) |>
  summarise(promedio = mean(sin_tend, na.rm = TRUE))

prom_pos

#Ajuste: se divide entre el promedio de los 5 valores
# ara que los indices promedien 1 (y sumen 5)
indices_mu <- prom_pos |>
  mutate(estacional = promedio / mean(promedio))

indices_mu

#Comprobar que los indices sumen 5
sum(indices_mu$estacional)

#Agregar la componente estacional a la tabla principal
datos_mu <- datos_mu |>
  left_join(indices_mu |> select(posicion, estacional), by = "posicion")

datos_mu |>
  autoplot(estacional) +
  labs(title = "Componente estacional (indices) - Corporacion Favorita",
    x = "Numero de observacion",
    y = "Indice estacional")


#PASO 4: Calcular la componente restante
#Se divide la serie original entre (tendencia x estacional).
datos_mu <- datos_mu |>
  mutate(restante = PRECIO_PROMEDIO / (tendencia * estacional))

datos_mu |>
  autoplot(restante) +
  labs(title = "Componente restante (multiplicativa) - Corporacion Favorita",
    x = "Numero de observacion",
    y = "Componente restante (alrededor de 1)")

