# Librerías y cargo csvs ####

library(readr)
library(tidytext)
library(stringi)
library(tidyverse)
library(stopwords)
library(quanteda)
library(quanteda.textstats)

df_notis_limpio <- read_csv("data/df_notis_limpio.csv")
df_entrevistas_limpio <- read_csv("data/df_entrevistas_limpio.csv")

#saco este porque es medio indirecto lo que habla de inseguridad
df_entrevistas_limpio <- df_entrevistas_limpio %>% 
  filter(!(fecha == "2022-06-20" & politico == "Cristina Kirchner" & tipo == "entrevista")) 

# Similitud de discursos ####

# creo filtro de palabras en castellano
filtro_palabras<-stri_trans_general(stopwords("es"), "Latin-ASCII")
#armo corpus de la bolsa de palabras
corpus_entrevistas<-df_entrevistas_limpio%>%
  unnest_tokens(input = texto, output = word)%>%
  filter(!word%in%filtro_palabras)

corpus_entrevistas<-corpus_entrevistas%>%
  mutate(word= str_to_lower(stri_trans_general(word, "Latin-ASCII")),
         group = politico) #agrupo por político

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

dfm_df_tidy_by_diario<-dfm_group(dfm_df_tidy, groups = diario) #agrupo por diario


# comparo discursos
df_similitud_diarios<-textstat_simil(dfm_df_tidy_by_diario, corpus_entrevistas,
                                     method = "cosine", #c("correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamman","simple matching")
                                     margin = "documents") %>%#al poner documents comparo entre autores
  as.data.frame()


summary(df_similitud_diarios)

#guardo el csv
write.csv(df_similitud_diarios, "./data/df_similitud_diarios.csv", fileEncoding = "UTF-8")

# Grafico ####
df_grafico <- df_similitud_diarios %>% 
  filter(!document1 %in% c("casarosada","diariodeleuco")) %>%
  rename(diario = document1) %>% 
  mutate(diario = case_when(diario == "todonoticias" ~ "Todo Noticias",
                            diario == "perfil" ~ "Perfil",
                            diario == "paginadoce" ~ "Pagina 12",
                            diario == "lanacion" ~ "La Nacion",
                            diario == "infobae" ~ "Infobae",
                            diario == "eldestape" ~ "El Destape",
                            diario == "clarin" ~ "Clarin",
                            diario == "ambito" ~ "Ambito"))
library(viridis) 
library(RColorBrewer)

#windowsFonts()
#serif es Times New Roman, sans arial y mono courier

ggplot(df_grafico, aes(x = document2, y = diario, fill = cosine)) +
  geom_tile(color = "white", size = 0.2) +
  geom_text(aes(label = round(cosine, 2)), color = "white", size = 2, nudge_y = 0.2) +
  labs(x = " ", y = " ", fill = "Cos") +
  scale_fill_viridis(option = "magma",  direction = -1 )  +
  ggtitle("Similitud de discursos sobre seguridad entre \nprecandidatos a presidente y diarios digitales") +
  theme_minimal() +
  labs(caption = "Fuente: Elaboración propia") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, family = "serif"),
    plot.title.position = "panel",
    text = element_text(color = "black", size = 10, family = "serif"),
    plot.caption = element_text(hjust = 1.4)
  )
  



