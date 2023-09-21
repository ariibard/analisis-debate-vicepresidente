library(tidyverse)
library(readxl)

options(scipen = 999)

#levanto data ####
base<-read.csv("./data/noticias_inseguridad_coseno.csv", fileEncoding = "UTF-8")
base_clas<-read_xlsx("./data/notis_inseguridad_sample_clas.xlsx")

#joineo lo que clasificamos ####
base_log<-base%>%
  inner_join(base_clas%>%
               select(!texto),by = "id_noticia")%>%
  filter(!is.na(`es_seguridad?`))%>%
  mutate(es_seguridad = as.numeric(as.factor(`es_seguridad?`))-1)

#ploteo ####

plot<-ggplot(base_log, aes(cosine, es_seguridad))+
  geom_point()
plot

# intento ####
set.seed(1234)

# genero sets de training - testing
training<-sample_frac(base_log, size = 0.8)
range(training$es_seguridad)

testing<-base_log%>%
  anti_join(training)

# entreno modelo con el set de training
glm_modelo<-glm(data = training, formula =  es_seguridad ~ cosine, family = binomial)

plot(glm_modelo) # no puedo interpretar los plots jaja

# coef(glm_modelo)
# summary(glm_modelo) # el pvalue da bajisimo jojojo!!!!

testing$model_prob<-predict(object = glm_modelo, newdata = testing, type = "response")

plot_2<-ggplot(testing, aes(model_prob, es_seguridad))+
  geom_point()
plot_2

# medimos accurate del modelo sobre el set de testing 

testing<-testing%>%
  mutate(model_pred = ifelse(model_prob > 0.38, 1, 0),# vamos variando el threshold para ver dónde clasifica mejor
         accurate = ifelse(model_pred == es_seguridad, 1, 0)) # creamos variable para medir los casos donde clasificó bien

# medimos
sum(testing$accurate)/nrow(testing)

# intento 2 ####
# install.packages("xgboost")
library(xgboost)

# clasifico todo el set de noticias ####
base$model_prob<-predict(object = glm_modelo, newdata = base, type = "response")

base_final<-base%>%
  mutate(es_seguridad = ifelse(model_prob > 0.38 , "sí", "no"))%>%
  filter(es_seguridad == "sí")

write.csv(base_final, "./data/base_noticias_final.csv", fileEncoding = "UTF-8")



  
