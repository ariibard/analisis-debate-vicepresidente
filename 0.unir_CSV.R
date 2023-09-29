# Carga el paquete dplyr
library(dplyr)
library(readr)

# Especifica la carpeta donde se encuentran tus archivos CSV
carpeta <- "noticias"

# Lista para almacenar los DataFrames individuales
dfs <- list()

# Lista de archivos en la carpeta
archivos <- list.files(path = carpeta, pattern = "*.csv", full.names = TRUE)

# Itera a través de los archivos y carga cada CSV en un DataFrame
for (archivo in archivos) {
  df <- read.csv(archivo)
  dfs <- append(dfs, list(df))
}

# Combina todos los DataFrames en uno solo
df_final <- bind_rows(dfs) 
# Guardo

write.csv(df_final, 'data/bases/0-notis-0809-2023.csv')


# Importo el csv final 
notis <- read_csv("data/bases/0-notis-0809-2023.csv") |> 
  select(-...1)

notis <- notis |> 
  mutate(id_noticia = 1:nrow(notis)) |> 
  select(id_noticia, everything())

write.csv(notis, 'data/bases/0-notis-0809-2023.csv', row.names = FALSE)
