library(fpp3)

datos <- read_csv("data/data.csv") |> as_tsibble(index = FECHA) 

#1.Gráfico temporal
datos |>
  autoplot(PRECIO_PROMEDIO) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Precio Promedio diario por acción de Corporación La Favorita (2025)",
       x = "Mes", y = "Precio (USD)")

 
datos |> fill_gaps() |>
  gg_season(PRECIO_PROMEDIO, period = "week") +
  theme(legend.position = "none") +
  labs(title = "Precio por día de la semana (2025)",
       y = "Precio (USD)")
