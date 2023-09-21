library(tidyverse)
library(tidytext)
library(stringi)
library(stopwords)
library(quanteda)
library(quanteda.textstats)
library(data.table)

# 1- levanto data de políticos ####

list.files("./data")

base_entrevistas <- read.csv("./data/df_entrevistas_final_final.csv", fileEncoding = "UTF-8")%>%
  mutate(tipo = "entrevista")

# base_entrevistas$text[25]


# base_tweets <- read.csv("./data/tweets_inseguridad.csv",fileEncoding = "UTF-8")
base_tweets <- fread("./data/tweets_inseguridad.csv",encoding = "UTF-8",quote = "\"")%>%
  mutate(tipo = "tweet")

# pego los dos df y luego agrupo nombres

base_politicos<-rbind(
  base_entrevistas%>%
    select(politico = Politico, fecha, texto = text, tipo),
  base_tweets%>%
    select(politico = cuenta, fecha, texto, tipo))

unique(base_politicos$politico)


base_politicos <- base_politicos %>%
  mutate(politico = ifelse(str_detect(politico, "Patricia"), "Patricia Bullrich", politico),
         politico = ifelse(str_detect(politico, "Daniel"), "Daniel Scioli", politico),
         politico = ifelse(str_detect(politico, "Wado"), "Wado de Pedro", politico),
         politico = ifelse(str_detect(politico, "Nicolás"), "Nicolas del Caño", politico)
  )

unique(base_politicos$politico)

#saco a Schiaretti

base_politicos <- base_politicos %>% 
  filter(! politico == "Juan Schiaretti")

# guardo el csv
write.csv(base_politicos, "./data/base_entrevistas_final.csv", fileEncoding = "UTF-8")


#chequeo nombres
base_politicos %>% 
  group_by(politico) %>% 
  summarise(N=n())

