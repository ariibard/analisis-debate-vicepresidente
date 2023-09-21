# Limpiar bases tweets y noticias

# Librerías ####

library(readr)
library(tidytext)
library(stringi)
library(tidyverse)


df_notis <- read_csv("data/base_noticias_final.csv")
df_entrevistas <- read_csv("data/base_entrevistas_final.csv")

# limpio ####
df_notis_limpio<-df_notis%>%
  mutate(texto= stri_trans_general(texto, "Latin-ASCII"), #una forma rápida de quitar acentos y ñ
         texto= str_replace_all(texto,"www\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto,"https\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto, "[[:punct:]]", " "), #reemplazo puntuaciones con REGEX
         texto= str_replace_all(texto, "[[:digit:]]+", " "),#reemplazo numeros con REGEX
         texto = str_squish(texto),
         texto= str_to_lower(texto))

# no armé el unnest tokens porque en 2.comparar_notis_bolsa ya supimos cuales eran

# creo vector con frasese repetidas
frases_repe_filtro<-c(
  "este proyecto lo hacemos en grupo sostene a el destape con un click aca sigamos haciendo historia suscribite a el destape",
  "este contenido se hizo gracias al apoyo de la comunidad de el destape sumate sigamos haciendo historia suscribite a el destape",
  "con informacion de efe seguir leyendo",
  "el contenido al que quiere acceder es exclusivo para suscriptores")

# saco frases repetidas de la base limpia
for (frase in frases_repe_filtro) {
  df_notis_limpio<-df_notis_limpio%>%
    mutate(texto = str_replace_all(texto, frase, ""))}

# Base Entrevistas
df_entrevistas_limpio<-df_entrevistas%>%
  mutate(texto= stri_trans_general(texto, "Latin-ASCII"), #una forma rápida de quitar acentos y ñ
         texto= str_replace_all(texto,"www\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto,"https\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto, "[[:punct:]]", " "), #reemplazo puntuaciones con REGEX
         texto= str_replace_all(texto, "[[:digit:]]+", " "),#reemplazo numeros con REGEX
         texto = str_squish(texto),
         texto= str_to_lower(texto))

rm(df_notis, df_entrevistas, frase, frases_repe_filtro)
# guardo el csv
write.csv(df_entrevistas_limpio, "./data/df_entrevistas_limpio.csv", fileEncoding = "UTF-8")
write.csv(df_notis_limpio, "./data/df_notis_limpio.csv", fileEncoding = "UTF-8")


# Similitud de discursos ####

# creo filtro de palabras en castellano
filtro_palabras<-stri_trans_general(stopwords("es"), "Latin-ASCII")
#armo corpus de la bolsa de palabras
corpus_entrevistas<-df_entrevistas_limpio%>%
  unnest_tokens(input = texto, output = word)%>%
  filter(!word%in%filtro_palabras)

corpus_entrevistas<-corpus_entrevistas%>%
  mutate(word= str_to_lower(stri_trans_general(word, "Latin-ASCII")),
         group = politico) #creo una variable par después agrupar

#agrupo por político

corpus_entrevistas<-corpus(corpus_entrevistas%>%
                                   rename(text=word))
corpus_entrevistas<-dfm(corpus_entrevistas)
corpus_entrevistas<-dfm_group(corpus_entrevistas, groups = group)



#armo corpus de las noticias
base_notis_limpio_unnest<-df_notis_limpio%>%
  unnest_tokens(input = texto, output = word)%>%
  filter(!word%in%filtro_palabras)

corpus_df<-corpus(base_notis_limpio_unnest%>%
                    rename(text=word))

dfm_df_tidy<-dfm(corpus_df)

dfm_df_tidy_by_diario<-dfm_group(dfm_df_tidy, groups = id_noticia)


# comparo discursos
df_similitud_diarios<-textstat_simil(dfm_df_tidy_by_diario, corpus_bolsa_inseguridad,
                                     method = "cosine", #c("correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamman","simple matching")
                                     margin = "documents") %>%#al poner documents comparo entre autores
  as.data.frame()%>%
  mutate(id_noticia = as.numeric(as.character(document1)))

#chequeo
base_notis$texto[base_notis$id_noticia==198657]

# 4 guardo data ####
base_notis<-base_notis%>%
  left_join(df_similitud_diarios, by = "id_noticia")%>%
  select(!c(document1))

write.csv(base_notis,"./data/noticias_inseguridad_coseno.csv", fileEncoding = "UTF-8")


