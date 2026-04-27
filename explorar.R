library(surveydown)
library(dplyr)
library(stringr)

# surveydown::sd_db_config()

# conexión a la base de datos de la encuesta
db <- sd_db_connect()

# obtener datos desde la base y ordenar
data <- sd_get_data(db) |> 
  tibble() |> 
  arrange(desc(time_end))

# revisar datos
glimpse(data)

data |> count(sesiones)

# personas inscritas y que pidieron notificación
data |> 
  filter(time_start > "2026-03-18" | sesiones == "siguiente_sesi_n_y_sesiones_futuras") |> 
  filter(!is.na(correo)) |> 
  pull(correo) |> 
  cat(sep = "\n")

# personas inscritas desde último correo
data |> 
  filter(time_start >= "2026-04-24") |> 
  filter(!is.na(correo)) |> 
  # select(time_start, correo)
  pull(correo) |> 
  cat(sep = "\n")
