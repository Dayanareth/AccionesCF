library(readxl)
library(fpp3)
library(here)

datos_acciones <- read_excel("data/acciones.xls", sheet = 1, skip = 8, n_max = 7296)
names(datos_acciones)[6] <- "NUMERO_ACCIONES"

class(datos_acciones)#ya es un dataframe

datos_acciones2 <- datos_acciones |>
  mutate(FECHA = ymd(FECHA)) |>
  dplyr::filter(!is.na(FECHA)) |>filter(EMISOR == "CORPORACION FAVORITA C.A.") |> #filtrar solo para la corporación la favorita
  group_by(FECHA, EMISOR) |>
  summarise(PRECIO_PROMEDIO = mean(PRECIO)) |> 
  tsibble::as_tsibble(index = FECHA, key = EMISOR)

datos_acciones2

