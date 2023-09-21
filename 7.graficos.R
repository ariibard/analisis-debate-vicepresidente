library(readr)
library(viridis) 
library(RColorBrewer)
library(tidyverse)
library(janitor)
library(tidytext)
library(gt)
library(comunicacion)

options(scipen = 999)

df_notis <- read_csv("data/df_notis_limpio.csv")
df_discursos <- read_csv("data/df_entrevistas_limpio.csv")
discursos <- read_csv("data/discursos_parsed.csv")

df_discursos |> 
  group_by(politico) |> 
  summarise(N=n()) |> 
  arrange(desc(N)) |> 
  gt() |> 
  gt_theme_dnmye() |> 
  tab_header(
    title = md("Tabla 2: Cantidad de discursos por precandidato"),
    subtitle = "Fuente: Elaboración propia en base a Dicen los Medios"
  ) |> 
  cols_label( N = "Discursos sobre Inseguridad",
              politico ="Precandidatos") 
# Grafico 1: Porcentaje de noticias referidas a inseguridad ####


noticias <- c(26157, 54195, 42821, 248588, 35550, 29284, 32793, 155, 21579, 1104)
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
    title = md("Tabla 3: Cantidad de noticias por diario"),
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

rm(noticias, diarios,df,tabla,p_1)

# Porcentaje de Tweets
nombres <- c('Cristina Kirchner', 'Daniel Scioli', 'Juan Grabois', 'Horacio Rodríguez Larreta', 'Javier Milei', 'Juan Schiaretti', 'Myriam Bregman', 'Nicolas del Caño', 'Patricia Bullrich', 'Sergio Massa', 'Wado de Pedro')

# Crear el vector de valores de los discursos
discursos <- c(348, 1092, 394, 1277, 43435, 1426, 5781, 2409, 1301, 1163, 1627)

# Crear el dataframe
df <- data.frame(politico = nombres, tweets = discursos)

df_discursos |> 
  group_by(tipo, politico) |> 
  summarise(entrevistas = n()) |> 
  pivot_wider(names_from = tipo, values_from = entrevistas) |> 
  left_join(df)|> 
  mutate(proporcion = (tweet/tweets)) |> 
  arrange(desc(proporcion))|> 
  gt() |> 
  gt_theme_dnmye() |> 
  tab_header(
    title = md("Tabla 2: Cantidad de discursos por precandidato"),
    subtitle = "Fuente: Elaboración propia en base a Dicen los Medios"
  ) |> 
  cols_label( entrevista = "Discursos sobre Inseguridad",
              politico ="Precandidatos a presidente",
              tweet = "Tweets sobre inseguridad",
              tweets = "Tweets totales",
              proporcion = "% de tweets sobre inseguridad") 

# Grafico 2: Linea de tiempo ####

class(df_notis$fecha)

df_linea <- df_notis |> 
  group_by(fecha) |> 
  summarise(cantidad_noticias = n())


ggplot(df_notis) +
  aes(x = fecha) +
  geom_freqpoly( lineend = "round",colour = "#C93E83") +
  labs(x = " ", 
       y = " ", title = "Grafico 2: Noticias sobre inseguridad ", subtitle = "Desde el 1 de abril de 2022 hasta el 30 de abril de 2023", 
       caption = "Fuente: Elaboración propia en base a Dicen los Medios") +
  theme_light() +
  theme(plot.title = element_text(size = 18L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14L, face = "italic", hjust = 0.5), 
        plot.caption = element_text(size = 10L),
        text = element_text(color = "black", size = 14, family = "serif")) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month",limits = as.Date(c("2022-04-01", "2023-04-30")))

ggsave("graficos/p_2.png", width = 10, height = 4)

rm(df_linea,p_2)
# Grafico 3 y 4: adj, sust, ent de noticias y candidatos ####


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
  arrange(desc(N)) 

palabras_diarios_top <- palabras_diarios %>% #elimino las palabras que estan en todos
  filter(! palabra %in% c("0221","año", "ser", "tener", "decir", "día", "haber", "hacer", "estar")) |> 
  group_by(diario) %>%
  top_n(10, N)

# Grafico
plot_palabras <- palabras_diarios_top %>%
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
  mutate(palabra = reorder_within(palabra, N, diario)) |> 
  ggplot() +
  aes(x = reorder(palabra,N), y = N, fill = tipo) +
  geom_col() +
  scale_fill_manual(values = c(adjetivos = "#F8766D", 
                               lugares = "#7400AA", organizaciones = "#00C19F", sustantivos = "#619CFF", verbos = "#E5518E")) +
  labs(x = " ", 
       y = " ", title = "Gráfico 3: Palabras más utilizadas por cada portal de noticias", subtitle = "En millones. Del 01 de abril de 2022 al 30 de abril de 2023", 
       caption = "Fuente: Elaboración propia en base a Dicen los medios", fill = "Tipo de palabra") +
  coord_flip() +
  scale_x_reordered() +
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

discursos <- read_csv("data/discursos_parsed.csv")

df_transformado_disc <- discursos %>%
  select(politico, tipo, adjetivos:lugares) %>%
  gather(tipo, palabra, adjetivos:lugares) %>%
  separate_rows(palabra, sep = ",") %>%
  separate(palabra, into = c("palabra", "frecuencia"), sep = ":", convert = TRUE, extra = "merge") %>%
  mutate(palabra = str_replace_all(palabra, "Counter|\\{|\\}", ""),
         palabra = str_replace_all(palabra, "\\(|\\)", ""),
         palabra = str_replace_all(palabra, "'|'", ""),
         palabra = str_replace_all(palabra, "\\s+", ""),
         frecuencia = str_replace_all(frecuencia, "\\}|\\)", ""),
         palabra = str_replace_all(palabra, "milei", "")) |> 
  filter(nchar(palabra) > 1)


palabras_discursos <- df_transformado_disc|> 
  mutate(palabra = as.character(palabra),
         frecuencia = as.numeric(frecuencia)) |> 
  drop_na(palabra) |> 
  drop_na(frecuencia )|> 
  filter(!palabra == "") |> 
  group_by(politico, palabra, tipo) |> 
  summarise(N = sum(frecuencia)) |> 
  arrange(desc(N)) |>  
  group_by(politico) %>%
  slice_max(n = 5, order_by = N) %>%
  ungroup()



plot_palabras_discursos <- palabras_discursos |> 
  mutate(palabra = reorder_within(palabra, N, politico)) |> 
  ggplot() +
  aes(x = reorder(palabra,N), y = N, fill = tipo) +
  geom_col() +
  scale_fill_manual(values = c(adjetivos = "#F8766D", 
                               lugares = "#7400AA",
                               sustantivos = "#619CFF",
                               verbos = "#E5518E",
                               organizaciones = "#00C19F")) +
  labs(x = " ", 
       y = " ", title = "Gráfico 4: Palabras más utilizadas por cada precandidato", subtitle = "Del 01 de abril de 2022 al 30 de abril de 2023", 
       caption = "Fuente: Elaboración propia en base a Dicen los medios", fill = "Tipo de palabra") +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 20L, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 16L, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 8L),
        text = element_text(color = "black", size = 9, family = "serif"),
        axis.text.x = element_text(hjust = 1, face = "bold")) +
  facet_wrap(vars(politico), scales = "free")


ggsave("graficos/plot_palabras_discursos.png", width = 9, height = 6)


# 5 y 6- Diversidad de léxico####
diversidad <- df_transformado |> 
  mutate(frecuencia = as.numeric(frecuencia)) |> 
  group_by(diario) |> 
  summarise(N = n(),
            total = sum(frecuencia, na.rm = TRUE),
            diversidad = N/sum(frecuencia, na.rm =TRUE))
#cantidad de palabras únicas /palabras totales

diversidad |>
  filter(!(diario %in% c("diariodeleuco", "casarosada"))) |> 
  mutate(diario = case_when(diario == "todonoticias" ~ "Todo Noticias",
                            diario == "perfil" ~ "Perfil",
                            diario == "paginadoce" ~ "Pagina 12",
                            diario == "lanacion" ~ "La Nacion",
                            diario == "infobae" ~ "Infobae",
                            diario == "eldestape" ~ "El Destape",
                            diario == "clarin" ~ "Clarin",
                            diario == "ambito" ~ "Ambito")) |> 
  ggplot() +
  aes(x = reorder(diario, diversidad), weight = diversidad) +
  geom_bar(fill = "#C93E83") +
  labs(x = " ", 
       y = " ", title = "Gráfico 5: Diversidad de léxico por portal de noticias",
       subtitle = "En noticias sobre inseguridad (Abril 2022 - Abril 2023)", 
       caption = "Fuente: Elaboración Propia en base a Dicen los medios") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12L,
                                     face = "italic", hjust = 0.5))
#esquisse::esquisser(diversidad)
ggsave("graficos/plot_diversidad_notis.png", width = 8, height = 4)


#### discursos precandidatos ### 
diversidad_politicos <- df_transformado_disc |> 
  mutate(frecuencia = as.numeric(frecuencia)) |> 
  group_by(politico) |> 
  summarise(N = n(),
            total = sum(frecuencia, na.rm = TRUE),
            diversidad = N/sum(frecuencia, na.rm =TRUE)) 
  

diversidad_politicos |>
  ggplot() +
  aes(x = reorder(politico, diversidad), weight = diversidad) +
  geom_bar(fill = "#C93E83") +
  labs(x = " ", 
       y = " ", title = "Gráfico 6: Diversidad de léxico por precandidato",
       subtitle = "En discursos sobre inseguridad (Abril 2022 - Abril 2023)", 
       caption = "Fuente: Elaboración Propia en base a Dicen los medios") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12L,
                                     face = "italic", hjust = 0.5))
#esquisse::esquisser(diversidad)
ggsave("graficos/plot_diversidad_politicos.png", width = 8, height = 4)

# 7  Grafico similitud coseno ####
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

#windowsFonts()
#serif es Times New Roman, sans arial y mono courier

p_sc <- ggplot(df_grafico, aes(x = document2, y = diario, fill = cosine)) +
  geom_tile(color = "white", size = 0.2) +
  geom_text(aes(label = round(cosine, 2)), color = "black", size = 3, nudge_y = 0.2) +
  labs(x = " ", y = " ", fill = "Cos") +
  scale_fill_distiller(palette = "RdPu",  direction = 1 )  +
  scale_x_discrete(labels = scales::wrap_format(20))+
  ggtitle("Gráfico 7: Similitud de discursos sobre seguridad entre \nprecandidatos a presidente y diarios digitales") +
  theme_minimal() +
  labs(caption = "Fuente: Elaboración propia en base a Dicen los Medios") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
    axis.text = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, family = "serif"),
    plot.title.position = "panel",
    text = element_text(color = "black", size = 12, family = "serif"),
    plot.caption = element_text(hjust = 1.4)
  )
p_sc
ggsave("graficos/plot_sc.png", width = 6, height = 4)


