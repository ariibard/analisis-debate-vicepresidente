library(tidyverse)
library(lubridate)
library(youtubecaption)
library(reticulate)
library(googlesheets4)

options(scipen = 999)
 
# 1 - levanto lista de links ####
df_links<-read_sheet(ss = "https://docs.google.com/spreadsheets/d/1vLeXj6CqiYcJNAFtH9KPXoOi2OrL0mALBkkD7CSShjc/edit#gid=0",
                     sheet =  "Hoja 1")

df_links<-df_links%>%
  filter(str_detect(link, "youtube"))

## creo lista de links para descargar subs ####
lista_links<-unique(df_links$link)


## creo los df vacios para el loop ####
df_subtitulos<-data.frame()
lista_links_sin_subs<-c()

## loopeo ####
tictoc::tic()
for (link in lista_links) {

  print(paste0("voy por el link ", link)) #para que en consola me diga qué link está levantando
  
  # genero un trycatch para quedarme con links de los que no pude descargar subs
  tryCatch({
    
    sub_1<-get_caption(url = link, language = "es")
    
    df_subtitulos<-rbind(df_subtitulos, sub_1)
    
    },
    
    error = function(e){
      
      lista_links_sin_subs<<-c(lista_links_sin_subs, link)
      print(e)
      
    })
    
  
  
}
tictoc::toc()

# me fijo cuántos links pude descargar
length(unique(df_subtitulos$vid))

#guardo la data por las dudas
write.csv(df_subtitulos, "./data/df_subtitulos3.csv", fileEncoding = "UTF-8", row.names = F)

#creo un df con los links que no pude descargar, 
#luego corro el loop de vuelta pero reemplazo la lista_links por lista_links_sin_subs
df_sin_subs<-df_links%>%
  filter(link%in%lista_links_sin_subs)

lista_links_sin_subs<-unique(df_sin_subs$link)

# guardo toda la info
df_sub1<-read.csv("./data/df_subtitulos2.csv", fileEncoding = "UTF-8")
df_sub2<-read.csv("./data/df_subtitulos3.csv", fileEncoding = "UTF-8")
df_sub<-rbind(df_sub1, df_sub2)
length(unique(df_sub$vid))

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
