

library(tidyverse)
library(youtubecaption)
library(reticulate)
library(googlesheets4)
library(reticulate)

options(scipen = 999)

# DESCARGO SUBTITULOS -----------------------

# Link del debate
 
link <- "https://www.youtube.com/watch?v=NLb9WkfrvUY" 

tictoc::tic()

# Me traigo los subtitulos del debate
sub_1<- get_caption(url = link, language = "es")

# Cierra tictoc
tictoc::toc()

# Guardo subtitulos
write.csv(sub_1, file = "data/subtitulos.csv", row.names = FALSE)

#importo subtitulos
library(readr)
subtitulos <- read_csv("data/subtitulos.csv")
View(subtitulos)

# Identificar: 
## Los momentos donde hablan de los temas

# A) economía, inflación y trabajo; 
# B) rol del estado y conflictividad social 
# C) política de seguridad, defensa y justicia

## Ver los tiempos en los que habla cada candidato


# FILTRO POR INTERVALOS ----------------------------

# Crear columna con tópico y candidato


# 2 - filtro los intervalos que me interesan ####
 
## primero genero lista de videos de los que pude obtener sub ####
id_videos<-unique(df_sub$vid)

## limpio y ordeno data ####

# filtro la lista de links y me quedo con aquellos para los que obtuve subs, 
# luego paso a numericos los valores "desde" y "hasta"

patron <- "(?<=v=)([^&#]*)"

df_links2 <- df_links%>%
  mutate(id_video = str_extract(link, patron))%>%
  filter(id_video %in% id_videos)%>%
  filter(!is.na(desde))%>%
  filter(!is.na(hasta))%>%
  mutate(desde = as.numeric(seconds(ms(desde))), 
         hasta = as.numeric(seconds(ms(hasta))))

## creo df vacío ####
df_final <- data.frame()

## loopeo ####
for (id in id_videos) {
  
  tiempos <- df_links2%>%
    filter(id_video == id)
  
  desde<-tiempos$desde
  
  hasta <-tiempos$hasta
  
  df_filtrado<- df_sub%>%
    filter(vid == id &
             desde <= start &
             hasta >= start)
  
  df_final <- rbind(df_final, df_filtrado)
  
}
length(unique(df_final$vid))

# guardo la data final con la que voy a trabajar
write.csv(df_final, "./data/df_subs_completo.csv", fileEncoding = "UTF-8", row.names = F)

# 3 - junto todo ####

df_final_2<-read.csv("./data/df_subs_completo.csv", fileEncoding = "UTF-8")

df_final_2<-df_final_2%>%
  group_by(id_video = vid)%>%
  summarise(inicio = min(start),
            duracion = sum(duration),
            text = toString(text))%>%
  mutate(caracteres = str_count(text))

df_final_2<-df_links2%>%
  left_join(df_final_2, by= "id_video")

write.csv(df_final_2, "./data/df_entrevistas_final.csv", fileEncoding = "UTF-8", row.names = F)


## Agrego al df los discursos de cristina
library(readr)
library(readxl)
df_entrevistas_final <- read_csv("data/df_entrevistas_final.csv")

cfk_entrevistas <- read_excel("data/cristina.xlsx")

df_entrevistas_final_final <- df_entrevistas_final %>% 
  rbind(cfk_entrevistas)

cfk_entrevistas <- cfk_entrevistas %>%
  mutate(caracteres = str_count(text))

write.csv(df_entrevistas_final_final, "./data/df_entrevistas_final_final.csv", fileEncoding = "UTF-8", row.names = F)
