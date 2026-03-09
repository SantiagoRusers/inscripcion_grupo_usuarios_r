library(dplyr)

cupos <- readr::read_csv("seleccionados_anonimos.csv")

cupos |> count(genero)
cupos |> count(trans)
cupos |> count(pais)
