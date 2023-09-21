library(dplyr)
library(spacyr)

# Cargar los datos
discursos <- fread("data/df_entrevistas_limpio.csv") %>%
  select(-...1)

# Inicializar Spacy con el modelo 'es_core_news_sm'

spacy_initialize(model = 'es_core_news_sm')

parsedtxt <- spacy_parse(discursos$texto)
discursos_parsed <- cbind(discursos, parsedtxt) 

discursos_procesados <- discursos_parsed|>
  select(-doc_id, -sentence_id, -lemma) |> 
  rename("tipo_palabra" = pos) |> 
  mutate(tipo_palabra= case_when(tipo_palabra == "VERB" ~ "verbo",
                                 tipo_palabra == "NOUN" ~ "sustantivo",
                                 tipo_palabra == "ADJ" ~ "adjetivo",
                                 tipo_palabra == "PROPN" ~ "nombres")) |> 
  drop_na() #elimino los que no son verbo, adj, nombres o sustantivos

# Guardar el dataframe procesado en un archivo CSV
fwrite(discursos_procesados, 'data/discursos_procesados.csv')
