library(readxl)
library(fpp3)

datos_acciones <- read_excel("data/acciones.xls", sheet = 1, skip = 8, n_max = 7296)
names(datos_acciones)[6] <- "NUMERO_ACCIONES"

datos_acciones2 <- datos_acciones |>
  mutate(FECHA = ymd(FECHA)) |>
  dplyr::filter(!is.na(FECHA)) |>
  group_by(FECHA, EMISOR) |>
  summarise(PRECIO_PROMEDIO = mean(PRECIO)) |> 
  tsibble::as_tsibble(index = FECHA, key = EMISOR)

datos_acciones2

