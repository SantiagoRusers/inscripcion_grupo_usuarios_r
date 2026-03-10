library(surveydown)
library(dplyr)
library(stringr)

# conexión a la base de datos de la encuesta
db <- sd_db_connect()

# obtener datos desde la base y ordenar
data <- sd_get_data(db) |> 
  tibble() |> 
  arrange(desc(time_end))

# revisar datos
glimpse(data)


# limpieza de datos ----
resultados <- data |> 
  # solamente con correo
  filter(!is.na(correo)) |> 
  # excluir pruebas
  filter(nombre != "Bastián Olea") |> 
  # seleccionar columnas
  select(-starts_with("time"), time_end, -session_id) |> 
  # convertir fechas
  mutate(time_end = lubridate::as_datetime(time_end)) |> 
  arrange(time_end) |> 
  # limpiar nombres
  mutate(nombre = str_squish(nombre)) |> 
  distinct(nombre, .keep_all = TRUE) |> 
  # filtrar fecha de cierre
  # filter(time_end <= lubridate::ymd_hms("2026-01-14 20:59:59")) |> 
  # limpiar respuestas
  mutate(across(where(is.character),
                ~case_when(str_detect(.x, "^s$") ~ "sí",
                           .default = .x)))
resultados


# revisar datos ----
resultados |> glimpse()

resultados |> count(genero)
resultados |> count(nivel_r)
resultados |> count(pais)
resultados |> count(trans)
resultados |> distinct(areas) |> print(n=Inf)

# puntajes ----
# crear puntajes de selección
resultados_p <- resultados |> 
  mutate(puntaje = 1) |> 
  # sumar puntos a grupos de interés
  mutate(puntaje = if_else(trans == "sí", puntaje + 3, puntaje),
         puntaje = if_else(genero == "femenino", puntaje + 3, puntaje),
         puntaje = if_else(lgbt == "sí", puntaje + 2, puntaje),
         puntaje = if_else(genero %in% c("no_binario", "queer_ag_nero_otro"), puntaje + 2, puntaje),
         puntaje = if_else(pais == "chile", puntaje + 2, puntaje),
         puntaje = if_else(disca == "s_con_credencial_o_pensi_n_de_invalidez", puntaje + 2, puntaje),
         puntaje = if_else(disca == "s_sin_credencial_o_pensi_n_de_invalidez", puntaje + 1, puntaje)) |> 
  # penalizar grupos
  mutate(puntaje = if_else(nivel_programacion == "sí", puntaje - 1, puntaje),
         puntaje = if_else(nivel_r %in% c("intermedio", "avanzado"), puntaje - 1, puntaje)) |> 
  mutate(puntaje = if_else(puntaje <= 0, 1, puntaje)) |>
  # criterios excluyentes   
  filter(!str_detect(areas, "otra_que_no_es_de_ciencias_sociales_o_humanidades")) |>
  # crear ranking de postulantes
  arrange(desc(puntaje)) |> 
  mutate(id = row_number()) |> 
  relocate(puntaje, .after = nombre)

# revisar
resultados_p |> 
  # filter(pais == "chile") |> 
  select(nombre, pais, genero, disca, trans, lgbt, puntaje) |> 
  print(n=Inf)

resultados_p |> select(nombre, puntaje, id) |> head()
resultados_p |> select(nombre, puntaje, id) |> tail()


# selección ----

# separar personas trans, porque todxs tendrán cupo
personas_trans <- resultados_p |> 
  filter(trans != "no")

# cupos guardados para personas interesadas
cupos_especiales <- readLines("cupos_especiales.txt") |> 
  paste(collapse = "|")

personas_especiales <- resultados_p |> 
  filter(str_detect(tolower(nombre), cupos_especiales))

# excluir cupos trans y cupos especiales de la tabla de inscritos
resultados_p_2 <- resultados_p |> 
  filter(trans == "no") |> 
  filter(!nombre %in% personas_especiales$nombre)

# cantidad de cupos disponibles
n_cupos <- 45

# # selección aleatoria sin probabilidad
# seleccion <- sample(seq_len(nrow(resultados_p_2)), n_cupos)

# selección aleatoria con probabilidad
seleccion <- sample(resultados_p_2$id, 
                    size = n_cupos, 
                    prob = resultados_p_2$puntaje,
                    replace = FALSE)

# dejar seleccionadxs
resultados_cupo <- resultados_p_2 |> 
  slice(seleccion)

resultados_cupo |> count(genero)
resultados_cupo |> count(pais, sort = T)
resultados_cupo |> glimpse()


# revisar seleccionados
resultados_cupo |> 
  select(nombre, puntaje, pais, genero, disca, trans, lgbt) |> 
  print(n=Inf)

# agregar personas trans y cupos especiales
resultados_cupo_2 <- resultados_cupo |> 
  bind_rows(personas_trans) |> 
  bind_rows(personas_especiales)


# resultados ----
resultados_cupo_2 |> 
  select(nombre, pais, genero, disca, trans, lgbt, puntaje) |> 
  arrange(desc(puntaje)) |> 
  print(n=Inf)

resultados_cupo_2 |> count(genero)
resultados_cupo_2 |> count(pais)
resultados_cupo_2 |> count(nivel_r)
resultados_cupo_2 |> count(educacion)
mean(as.numeric(resultados_cupo_2$edad))
resultados_cupo_2 |> count(pais, trans)

# correos
resultados_cupo_2$correo |> 
  paste(collapse = ", ") |> 
  clipr::write_clip()
