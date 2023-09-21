library(tidyverse)
library(tidytext)
library(stringi)
library(stopwords)
library(quanteda)
library(quanteda.textstats)

options(scipen = 999)

# 1 levanto data ####
base_notis_2023<-read.csv("./data/noticias_inseguridad.csv", encoding = "UTF-8")
noticias_inseguridad_colgadas <- read_csv("data/noticias-inseguridad-colgadas.csv")

#uno ambos dfs
noticias_inseguridad_colgadas <- noticias_inseguridad_colgadas |> 
  mutate(fecha = as.character(fecha))

base_notis <- base_notis_2023 |> 
  rbind(noticias_inseguridad_colgadas)

rm(noticias_inseguridad_colgadas, base_notis_2023)

bolsa_inseguridad<-readxl::read_xlsx("./data/cronograma.xlsx", sheet = "Lista de palabras inseguridad", col_names = "word")

# 2 limpio ####
base_notis_limpio<-base_notis%>%
  mutate(texto= stri_trans_general(texto, "Latin-ASCII"), #una forma rápida de quitar acentos y ñ
         texto= str_replace_all(texto,"www\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto,"https\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto, "[[:punct:]]", " "), #reemplazo puntuaciones con REGEX
         texto= str_replace_all(texto, "[[:digit:]]+", " "),#reemplazo numeros con REGEX
         texto = str_squish(texto),
         texto= str_to_lower(texto))

# saco frases repetidas

frases_repe<-base_notis_limpio%>%
  unnest_tokens(input = texto, output = word, token = "ngrams", n=5)%>%
  group_by(diario, word)%>%
  summarise(cantidad=n())

#chequeo la frase completa
base_notis_limpio$text[55]
base_notis_limpio$text[159]
base_notis_limpio$text[1978]
base_notis_limpio$text[3890]

#creo vector con frasese repetidas
frases_repe_filtro<-c(
  "este proyecto lo hacemos en grupo sostene a el destape con un click aca sigamos haciendo historia suscribite a el destape",
  "este contenido se hizo gracias al apoyo de la comunidad de el destape sumate sigamos haciendo historia suscribite a el destape",
  "con informacion de efe seguir leyendo",
  "el contenido al que quiere acceder es exclusivo para suscriptores")

# saco frases repetidas de la base limpia
for (frase in frases_repe_filtro) {
  base_notis_limpio<-base_notis_limpio%>%
    mutate(texto = str_replace_all(texto, frase, ""))}

# 3 similitud de discursos ####

# bolsa_inseguridad<-str_to_lower(stri_trans_general(unique(bolsa_inseguridad$word), "Latin-ASCII"))

#armo corpus de la bolsa de palabras
bolsa_inseguridad<-bolsa_inseguridad%>%
  mutate(word= str_to_lower(stri_trans_general(word, "Latin-ASCII")),
         group = "bolsa") #creo una variable par después agrupar

corpus_bolsa_inseguridad<-corpus(bolsa_inseguridad%>%
                                   rename(text=word))
corpus_bolsa_inseguridad<-dfm(corpus_bolsa_inseguridad)
corpus_bolsa_inseguridad<-dfm_group(corpus_bolsa_inseguridad, groups = group)



# creo filtro de palabras en castellano
filtro_palabras<-stri_trans_general(stopwords("es"), "Latin-ASCII")

#armo corpus de las notis
base_notis_limpio_unnest<-base_notis_limpio%>%
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

#armo sample para clasificación manual ####
set.seed(1234)

df_sample<-read.csv("./data/noticias_inseguridad_coseno.csv", encoding = "UTF-8")%>%
  select(id_noticia, texto)%>%
  sample_n(size = nrow(df_sample)/10)


writexl::write_xlsx(df_sample, "./data/notis_inseguridad_sample.xlsx")

