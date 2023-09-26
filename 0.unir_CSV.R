# Carga el paquete dplyr
library(dplyr)

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

write.csv(df_final, 'data/notis_09-2023.csv')
