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


#REVISOR: DANIELA CUZCO 
#Estacionalidad
#Tenemos que no tiene porque en el gráfico de semanal vemos que no hay patrones repetitivos, y que las líneas estpan muy dispersas
#además hubo una caída grande en martes de 2025 por lo que vemos con más claridad que no tiene estacionalidad

#Cíclico
# No podemos ver dado que no tenemos varios años, solo tenemos de un año.
# Según el libro esto es visible cuando tenemos al menos 2 años 

#Tendencia
#Sí tiene, vemos que tiende a bajar en la primera mita del año que es de enero a julio y luego hay un cambio de dirección desde agosto 
#Y para diciembre tenemos que subió bastante siendo los más altos 

#Estacionariedad
#No es estacionaria pues vemos que la media cambia a lo largo del tiempo (los precios suben en diciembre)
