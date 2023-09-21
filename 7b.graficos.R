library(readr)
library(viridis) 
library(RColorBrewer)
library(tidyverse)
library(stringi)
library(janitor)
library(tidytext)

options(scipen = 999)

df_notis <- read_csv("data/df_notis_limpio.csv")
df_discursos <- read_csv("data/df_entrevistas_limpio.csv")
df_discursos_procesados <- read_csv("data/discursos_procesados.csv")

bolsa_inseguridad<-readxl::read_xlsx("./data/cronograma.xlsx", sheet = "Lista de palabras inseguridad", col_names = "word")%>%
  mutate(word= str_to_lower(stri_trans_general(word, "Latin-ASCII")))

filtro_palabras<-stri_trans_general(c(stopwords::stopwords("es")), "Latin-ASCII")

# adj, sust, ent de noticias y candidatos ####


#divido las columnas "sustantivos, adjetivos, verbos, organizaciones, lugares
df_transformado <- df_notis |> 
  select(fecha, diario, sustantivos:verbos, organizaciones, lugares) |> 
  gather(tipo, palabra, sustantivos:lugares) |> 
  separate_rows(palabra, sep = ",") |> 
  separate(palabra, into = c("palabra", "frecuencia"), sep = "-", convert = TRUE)

#Me quedo con las 15 palabras más mencionadas por gráfico 
palabras_diarios <- df_transformado %>%
  group_by(diario, palabra, tipo) %>%
  mutate(frecuencia = as.numeric(frecuencia)) %>%
  summarise(N = sum(frecuencia, na.rm = TRUE)) %>%
  arrange(desc(N)) %>%
  group_by(diario) %>%
  top_n(15, N)

# Grafico
plot_palabras <- palabras_diarios %>%
  filter(!(diario %in% c("diariodeleuco", "casarosada"))) |> 
  mutate(N = N/1000000) |>
  mutate(diario = case_when(diario == "todonoticias" ~ "Todo Noticias",
                            diario == "perfil" ~ "Perfil",
                            diario == "paginadoce" ~ "Pagina 12",
                            diario == "lanacion" ~ "La Nacion",
                            diario == "infobae" ~ "Infobae",
                            diario == "eldestape" ~ "El Destape",
                            diario == "clarin" ~ "Clarin",
                            diario == "ambito" ~ "Ambito")) |>
  ggplot() +
  aes(x = reorder(palabra,N), y = N, fill = tipo) +
  geom_col() +
  scale_fill_manual(values = c(adjetivos = "#F8766D", 
                               lugares = "#7400AA", organizaciones = "#00C19F", sustantivos = "#619CFF", verbos = "#E5518E")) +
  labs(x = " ", 
       y = " ", title = "Palabras más utilizadas por cada portal de noticias", subtitle = "En millones. Del 01 de abril de 2022 al 30 de abril de 2023", 
       caption = "Fuente: Elaboración propia en base a Dicen los medios", fill = "Tipo de palabra") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 20L, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 16L, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 10L),
        text = element_text(color = "black", size = 11, family = "serif"),
        axis.text.x = element_text(hjust = 1)) +
  facet_wrap(vars(diario), scales = "free")

ggsave("graficos/plot_palabrasc.png", width = 9, height = 6)

#### DISCURSOS
 discursos <- df_discursos_procesados |> 
   select(politico, fecha, tipo, token, tipo_palabra)
 
 palabras_discursos <- discursos %>% 
   mutate(token = as.character(token)) %>%
   filter(nchar(token) > 2) %>%
   group_by(politico, token, tipo_palabra) %>%
   summarise(N = n()) %>%
   arrange(desc(N)) %>%
   group_by(politico) 
   slice_max(N, n = 5)

palabras_discursos |> 
  ggplot() +
  aes(x = reorder(token,N), y = N, fill = tipo_palabra) +
  geom_col() +
  scale_fill_manual(values = c(adjetivo = "#F8766D", 
                               nombres = "#7400AA",
                               sustantivo = "#619CFF",
                               verbo = "#E5518E")) +
  labs(x = " ", 
       y = " ", title = "Palabras más utilizadas por cada portal de noticias", subtitle = "En millones. Del 01 de abril de 2022 al 30 de abril de 2023", 
       caption = "Fuente: Elaboración propia en base a Dicen los medios", fill = "Tipo de palabra") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 20L, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 16L, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 10L),
        text = element_text(color = "black", size = 11, family = "serif"),
        axis.text.x = element_text(hjust = 1)) +
  facet_wrap(vars(politico), scales = "free")

# Grafico similitud coseno ####
df_grafico <- read_csv("./data/df_similitud_diarios.csv")

df_grafico <- df_grafico|>
  filter(!document1 %in% c("casarosada","diariodeleuco")) |> 
  rename(diario = document1)|>
  mutate(diario = case_when(diario == "todonoticias" ~ "Todo Noticias",
                            diario == "perfil" ~ "Perfil",
                            diario == "paginadoce" ~ "Pagina 12",
                            diario == "lanacion" ~ "La Nacion",
                            diario == "infobae" ~ "Infobae",
                            diario == "eldestape" ~ "El Destape",
                            diario == "clarin" ~ "Clarin",
                            diario == "ambito" ~ "Ambito"))


#windowsFonts()
#serif es Times New Roman, sans arial y mono courier

p_sc <- ggplot(df_grafico, aes(x = document2, y = diario, fill = cosine)) +
  geom_tile(color = "white", size = 0.2) +
  geom_text(aes(label = round(cosine, 2)), color = "white", size = 2, nudge_y = 0.2) +
  labs(x = " ", y = " ", fill = "Cos") +
  scale_fill_viridis(option = "magma",  direction = -1 )  +
  scale_x_discrete(labels = scales::wrap_format(20))+
  ggtitle("Gráfico 7: Similitud de discursos sobre seguridad entre \nprecandidatos a presidente y diarios digitales") +
  theme_minimal() +
  labs(caption = "Fuente: Elaboración propia en base a Dicen los Medios") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
    axis.text = element_text(size = 7),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, family = "serif"),
    plot.title.position = "panel",
    text = element_text(color = "black", size = 10, family = "serif"),
    plot.caption = element_text(hjust = 1.4)
  )
p_sc
ggsave("graficos/plot_sc.png", width = 4, height = 4)

# Porcentaje de noticias referidas a inseguridad ####


noticias <- c(13243, 21060, 19163, 85702, 19002, 16179, 16916, 90, 10235, 402)
diario <- c('perfil', 'lanacion', 'eldestape', 'infobae', 'clarin', 'todonoticias', 'ambito', 'casarosada', 'paginadoce', 'diariodeleuco')

# Crea el dataframe
df <- data.frame(diario,noticias)

notis <- df_notis |>
  tabyl(diario) |> 
  select(-percent) |> 
  full_join(df) |> 
  mutate(porcentaje = (n/noticias)) |>
  filter(!diario %in% c("casarosada","diariodeleuco")) |> 
  mutate(diario = case_when(diario == "todonoticias" ~ "Todo Noticias",
                            diario == "perfil" ~ "Perfil",
                            diario == "paginadoce" ~ "Pagina 12",
                            diario == "lanacion" ~ "La Nacion",
                            diario == "infobae" ~ "Infobae",
                            diario == "eldestape" ~ "El Destape",
                            diario == "clarin" ~ "Clarin",
                            diario == "ambito" ~ "Ambito"))

library(gt)
library(comunicacion)
tabla <- notis |> 
  gt() |> 
  gt_theme_dnmye() |> 
  tab_header(
    title = md("Cantidad de noticias por diario"),
    subtitle = "Fuente: Elaboración propia en base a Dicen los Medios"
  ) |> 
  cols_label( n = "Noticias sobre Inseguridad",
              diario ="Diarios",
              noticias = "Noticias totales",
              porcentaje = "%") 

gtsave(tabla, file ="graficos/tabla_noticias.html")


library(scales)
p_1 <- ggplot(notis) +
    aes(x = reorder(diario,porcentaje), y = porcentaje) +
    geom_col(fill = "#C93E83") +
    labs(
      x = " ",
      y = " ",
      title = "Gráfico 1: Noticias sobre Inseguridad ",
      subtitle = "Porcentaje por diario",
      caption = "Fuente: Elaboración propia en base a Dicen los Medios"
    ) +
    coord_flip() +
    theme_light() +
    theme(plot.title = element_text(size = 16L, face = "bold"),
          text = element_text(color = "black", size = 12, family = "serif"))+
  scale_y_continuous(labels = scales::percent)
p_1

ggsave("graficos/p_1.png", width = 6, height = 4)


# Linea de tiempo ####

class(df_notis$fecha)

df_linea <- df_notis |> 
  group_by(fecha) |> 
  summarise(cantidad_noticias = n())


p_2 <- ggplot(df_linea) +
 aes(x = fecha, y = cantidad_noticias) +
 geom_line( lineend = "round",colour = "#C93E83") +
 labs(x = " ", 
 y = " ", title = "Noticias sobre inseguridad ", subtitle = "Desde el 1 de abril de 2022 hasta el 30 de abril de 2023", 
 caption = "Fuente: Elaboración propia en base a Dicen los Medios") +
 theme_light() +
 theme(plot.title = element_text(size = 18L, 
 face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14L, face = "italic", hjust = 0.5), 
 plot.caption = element_text(size = 10L),
 text = element_text(color = "black", size = 12, family = "serif")) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")

ggsave("graficos/p_2.png", width = 8, height = 4)


# 4- Nubes de palabras (Por candidato? Por espacio?)####

unique(df_discursos_procesados$politico)

df_discursos_procesados<-df_discursos_procesados%>%
  mutate(espacio_politico = ifelse(politico %in% c("Daniel Scioli", "Cristina Kirchner", "Sergio Massa", "Wado de Pedro", "Juan Grabois"), "Unión por la Patria", ""),
         espacio_politico = ifelse(politico %in% c("Horacio Rodríguez Larreta","Patricia Bullrich"), "Juntos por el Cambio", espacio_politico),
         espacio_politico = ifelse(politico %in% c("Myriam Bregman", "Nicolas del Caño"), "Frente de Izquierda", espacio_politico),
         espacio_politico = ifelse(politico %in% c("Javier Milei"), "La Libertad Avanza", espacio_politico))

df_discursos_nube<-df_discursos_procesados%>%
  group_by(espacio_politico, token)%>%
  summarise(freq = n())%>%
  arrange(desc(freq))%>%
  # filter(!grepl("[[:punct:]]", token))%>%
  filter(nchar(token)>1)%>% # me quedo con palabras mayores a 1 caracter
  filter(!token %in% c(unique(bolsa_inseguridad$word), filtro_palabras, "anos", "tener", "dia", "dias","hacer","hace"))%>% # saco las palabras de la bolsa
  slice_max(order_by = freq, n = 15)

# 5- Sentiment analisys?####
# 6- Diversidad de léxico####