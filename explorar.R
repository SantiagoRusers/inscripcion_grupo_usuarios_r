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
