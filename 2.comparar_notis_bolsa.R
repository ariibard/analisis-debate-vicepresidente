
################## SETEO LIBRERIAS ##########################################

library(tidyverse)
library(tidytext)
library(stringi)
library(stopwords)
library(quanteda)
library(quanteda.textstats)
library(googlesheets4)

####################### CARGO BASES ############################################

notis_economia <- read_csv("data/bases/01-noticias-filtro-economia.csv") |> 
  mutate(bolsa = "economía, inflación y trabajo",
         n_bolsa = 1) 
notis_estado <- read_csv("data/bases/01-noticias-filtro-estado.csv") |> 
  mutate(bolsa = "Rol del Estado y Conflictividad Social",
         n_bolsa = 2)
notis_seguridad <- read_csv("data/bases/01-noticias-filtro-seguridad.csv") |> 
  mutate(bolsa = "Política de seguridad, defensa y justicia",
         n_bolsa = 3)


options(scipen = 999)

# Traigo la bolsa de palabras
# gs4_deauth() # Para autenticarse en google

bolsas <- read_sheet("https://docs.google.com/spreadsheets/d/1E_UkEt11QqgwfLZLcS0xfyt_rb8brnSuh2ghcyowzpk/edit#gid=0",sheet = "bolsa_palabras")


###################### LIMPIEZA NOTICIAS ####################################

base_notis_economia_limpio <- notis_economia %>%
  mutate(texto= stri_trans_general(texto, "Latin-ASCII"), #una forma rápida de quitar acentos y ñ
         texto= str_replace_all(texto,"www\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto,"https\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto, "[[:punct:]]", " "), #reemplazo puntuaciones con REGEX
         texto= str_replace_all(texto, "[[:digit:]]+", " "),#reemplazo numeros con REGEX
         texto = str_squish(texto),
         texto= str_to_lower(texto)) 

base_notis_estado_limpio <- notis_estado %>%
  mutate(texto= stri_trans_general(texto, "Latin-ASCII"), #una forma rápida de quitar acentos y ñ
         texto= str_replace_all(texto,"www\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto,"https\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto, "[[:punct:]]", " "), #reemplazo puntuaciones con REGEX
         texto= str_replace_all(texto, "[[:digit:]]+", " "),#reemplazo numeros con REGEX
         texto = str_squish(texto),
         texto= str_to_lower(texto)) 

base_notis_seguridad_limpio <- notis_seguridad %>%
  mutate(texto= stri_trans_general(texto, "Latin-ASCII"), #una forma rápida de quitar acentos y ñ
         texto= str_replace_all(texto,"www\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto,"https\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto, "[[:punct:]]", " "), #reemplazo puntuaciones con REGEX
         texto= str_replace_all(texto, "[[:digit:]]+", " "),#reemplazo numeros con REGEX
         texto = str_squish(texto),
         texto= str_to_lower(texto)) 

rm(notis_seguridad, notis_estado, notis_economia)

# creo vector con frasese repetidas

frases_repe_filtro<-c(
  "este proyecto lo hacemos en grupo sostene a el destape con un click aca sigamos haciendo historia suscribite a el destape",
  "este contenido se hizo gracias al apoyo de la comunidad de el destape sumate sigamos haciendo historia suscribite a el destape",
  "con informacion de efe seguir leyendo",
  "el contenido al que quiere acceder es exclusivo para suscriptores")

# saco frases repetidas de la base limpia
for (frase in frases_repe_filtro) {
  base_notis_economia_limpio<-base_notis_economia_limpio%>%
    mutate(texto = str_replace_all(texto, frase, ""))}

for (frase in frases_repe_filtro) {
  base_notis_seguridad_limpio<-base_notis_seguridad_limpio%>%
    mutate(texto = str_replace_all(texto, frase, ""))}

for (frase in frases_repe_filtro) {
  base_notis_estado_limpio<-base_notis_estado_limpio%>%
    mutate(texto = str_replace_all(texto, frase, ""))}

rm(frases_repe_filtro)

############ SIMILITUD DE DISCURSOS ######################################

library(readxl)
minutos <- read_excel("data/bases/minutos.xlsx", 
                      sheet = "bolsa_palabras") |> 
  rename("word" = palabras)

bolsa_economia <- minutos |> 
  filter(bolsa == 1) 

bolsa_seguridad <- minutos |> 
  filter(bolsa == 3)

bolsa_estado <-  minutos |> 
  filter(bolsa == 2)

rm(minutos)

## ECONOMIA ####

bolsa_economia<-bolsa_economia%>%
  mutate(word= str_to_lower(stri_trans_general(word, "Latin-ASCII")),
         group = "bolsa") #creo una variable par después agrupar

corpus_bolsa_economia<-corpus(bolsa_economia%>%
                                   rename(text=word))
corpus_bolsa_economia<-dfm(corpus_bolsa_economia)
corpus_bolsa_economia<-dfm_group(corpus_bolsa_economia, groups = group)



# creo filtro de palabras en castellano
filtro_palabras<-stri_trans_general(stopwords("es"), "Latin-ASCII")

#armo corpus de las notis

# Filtro noticias de economía 
base_notis_economia_limpio <- base_notis_economia_limpio |> 
  mutate(id_noticia = 1:nrow(base_notis_economia_limpio))

id_eco <- max(base_notis_economia_limpio$id_noticia)

base_notis_limpio_unnest_eco <-base_notis_economia_limpio%>%
  unnest_tokens(input = texto, output = word)%>%
  filter(!word%in%filtro_palabras)

corpus_df_economia<-corpus(base_notis_limpio_unnest_eco%>%
                    rename(text=word))

dfm_df_tidy_economia<-dfm(corpus_df_economia)

dfm_df_tidy_by_diario<-dfm_group(dfm_df_tidy_economia, groups = id_noticia)


# comparo discursos
df_similitud_diarios_economia<-textstat_simil(dfm_df_tidy_by_diario, corpus_bolsa_economia,
                                     method = "cosine", #c("correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamman","simple matching")
                                     margin = "documents") %>%#al poner documents comparo entre autores
  as.data.frame()%>%
  mutate(id_noticia = as.numeric(as.character(document1)))

#chequeo

# guardo data en base_notis_limpio 
df_economia <-base_notis_economia_limpio%>%
  left_join(df_similitud_diarios_economia, by = "id_noticia")

## SEGURIDAD #########

# Limpiar la bolsa
bolsa_seguridad<-bolsa_seguridad%>%
  mutate(word= str_to_lower(stri_trans_general(word, "Latin-ASCII")),
         group = "bolsa") #creo una variable par después agrupar

# Creamos los corpus de la bolsa que después servirán para comparar con las noticias
corpus_bolsa_seguridad<-corpus(bolsa_seguridad%>%
                                rename(text=word))
corpus_bolsa_seguridad<-dfm(corpus_bolsa_seguridad)
corpus_bolsa_seguridad<-dfm_group(corpus_bolsa_seguridad, groups = group)


# Filtro noticias
base_notis_seguridad_limpio <- base_notis_seguridad_limpio |> 
  mutate(id_noticia = 1:nrow(base_notis_seguridad_limpio))

#id_seg <- max(base_notis_seguridad_limpio$id_noticia)


# creo un vector de las noticias por palabra
base_notis_limpio_unnest_seg <-base_notis_seguridad_limpio%>%
  unnest_tokens(input = texto, output = word)%>%
  filter(!word%in%filtro_palabras)

# Armo el corpus de las noticias
corpus_df_seguridad<-corpus(base_notis_limpio_unnest_seg%>%
                             rename(text=word))
dfm_df_tidy_seguridad<-dfm(corpus_df_seguridad)
dfm_df_tidy_by_diario_seg<-dfm_group(dfm_df_tidy_seguridad, groups = id_noticia)


# comparo discursos
df_similitud_diarios_seguridad <-textstat_simil(dfm_df_tidy_by_diario_seg, corpus_bolsa_seguridad,
                                     method = "cosine", #c("correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamman","simple matching")
                                     margin = "documents") %>%#al poner documents comparo entre autores
  as.data.frame()%>%
  mutate(id_noticia = as.numeric(as.character(document1)))

#chequeo

# guardo data en base_notis_limpio 
df_seguridad <-base_notis_seguridad_limpio%>%
  left_join(df_similitud_diarios_seguridad, by = "id_noticia")


## ESTADO ####
# Limpiar la bolsa
bolsa_estado<-bolsa_estado%>%
  mutate(word= str_to_lower(stri_trans_general(word, "Latin-ASCII")),
         group = "bolsa") #creo una variable par después agrupar

# Creamos los corpus de la bolsa que después servirán para comparar con las noticias
corpus_bolsa_estado<-corpus(bolsa_estado%>%
                                 rename(text=word))
corpus_bolsa_estado<-dfm(corpus_bolsa_estado)
corpus_bolsa_estado<-dfm_group(corpus_bolsa_estado, groups = group)


# Filtro noticias

base_notis_estado_limpio <- base_notis_estado_limpio |> 
  mutate(id_noticia = 1:nrow(base_notis_estado_limpio))


# creo un vector de las noticias por palabra
base_notis_limpio_unnest_estado <-base_notis_estado_limpio%>%
  unnest_tokens(input = texto, output = word)%>%
  filter(!word%in%filtro_palabras)

# Armo el corpus de las noticias
corpus_df_estado<-corpus(base_notis_limpio_unnest_estado%>%
                              rename(text=word))
dfm_df_tidy_estado<-dfm(corpus_df_estado)
dfm_df_tidy_by_diario_estado<-dfm_group(dfm_df_tidy_estado, groups = id_noticia)


# comparo discursos
df_similitud_diarios_estado <-textstat_simil(dfm_df_tidy_by_diario_estado, corpus_bolsa_estado,
                                                method = "cosine", #c("correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamman","simple matching")
                                                margin = "documents") %>%#al poner documents comparo entre autores
  as.data.frame()%>%
  mutate(id_noticia = as.numeric(as.character(document1)))

#chequeo

# guardo data en base_notis_limpio 
df_estado <-base_notis_estado_limpio%>%
  left_join(df_similitud_diarios_estado, by = "id_noticia")

################ GUARDO CSV ###################################################
write.csv(df_economia,"./data/2-noticias-economia-coseno.csv", fileEncoding = "UTF-8")
write.csv(df_seguridad,"./data/2-noticias_seguridad-coseno.csv", fileEncoding = "UTF-8")
write.csv(df_estado,"./data/noticias-estado-coseno.csv", fileEncoding = "UTF-8")



################ MUESTRA ####################################################
set.seed(1234)



df_sample_economia <- df_economia %>%
  sample_n(size = nrow(df_economia)*0.03) |> 
  select(-(sustantivos:document2))

df_sample_seguridad <- df_seguridad %>%
  sample_n(size = nrow(df_seguridad)*0.03)|> 
  select(-(sustantivos:document2))

df_sample_estado <- df_estado %>%
  sample_n(size = nrow(df_estado)*0.03)|> 
  select(-(sustantivos:document2))


library(openxlsx)

# Crea un nuevo libro de Excel
wb <- createWorkbook()

addWorksheet(wb, sheetName = "muestra_economia", gridLines = TRUE)
writeData(wb, sheet = "muestra_economia", x = df_sample_economia, startCol = 1, startRow = 1)

addWorksheet(wb, sheetName = "muestra_seguridad", gridLines = TRUE)
writeData(wb, sheet = "muestra_seguridad", x = df_sample_seguridad, startCol = 1, startRow = 1)

addWorksheet(wb, sheetName = "muestra_estado", gridLines = TRUE)
writeData(wb, sheet = "muestra_estado", x = df_sample_estado, startCol = 1, startRow = 1)
# Guarda el libro de Excel en un archivo
saveWorkbook(wb, file = "data/2-muestra-notis.xlsx", overwrite = TRUE)
