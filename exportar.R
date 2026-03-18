data |> 
  filter(!is.na(correo)) |> 
  select(nombre, genero, pais, correo) |> 
  print(n=Inf)

data |> 
  filter(!is.na(correo)) |> 
  pull(correo) |> 
  cat(sep = ", ")
