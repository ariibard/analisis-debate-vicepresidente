lista = [
'inseguridad',
'crimen',
'crímenes',
'criminal',
'robo',
'robar',
'homicidio',
'narco',
'policía',
'gendarme',
'prefectura',
'taser',
'detenidos',
'femicidio',
'violencia',
'cárcel',
'prisión',
'clandestino'
]

import csv
import spacy
from datatable import dt, f

# noticias = dt.fread('tweets.csv')
# textos = noticias[:, 'texto'].to_list()[0]

nlp = spacy.load('es_core_news_md')

i = 0

df = dt.Frame(id=[], fecha=[], cuenta=[], texto=[], 
              sustantivos=[], adjetivos=[], verbos=[], 
              personas=[], organizaciones=[], lugares=[], 
              matcheos=[])

f = open("tweets.csv", 'rt', encoding="utf-8")
f.readline() # leo y descarto la primer fila que solo tiene la info de columnas.

# csv.field_size_limit(sys.maxsize) # config para que lea todo el contenido de la fila
# csv.field_size_limit(1310720000) # config para que lea todo el contenido de la fila

# filas = csv.reader(f) # lo abro como un csv para poder iterarlo

tweets = f.readlines()

total = str(len(tweets))
conteo_terminos = {}
for tweet in tweets:
    [id, fecha, cuenta, texto] = tweet.split(',', 3)

    print(str(i) + ' de ' + total)

    if len(texto.strip()) is 0 or len(texto.strip()) < 10:
        i += 1
        continue

    conteo = 0
    matcheos = []
    for termino in lista:
        conteo += int(termino in texto)
        if termino in texto:
            matcheos.append(termino)

    if conteo < 1:
        i += 1
        continue

    cuerpo = nlp(texto.strip())

    # id = noticias[i,0]
    # fecha = noticias[i,1]
    # cuenta = noticias[i,2]

    sustantivos = ','.join([t.lemma_.lower() + '-' + str(t.idx) for t in cuerpo if t.pos_ == 'NOUN'])
    adjetivos = ','.join([t.lemma_.lower() + '-' + str(t.idx)  for t in cuerpo if t.pos_ == 'ADJ'])
    verbos = ','.join([t.lemma_.lower() + '-' + str(t.idx)  for t in cuerpo if t.pos_ == 'VERB'])

    personas = ','.join([e.text + '-' + str(e.start_char) for e in cuerpo.ents if e.label_ == 'PER'])
    organizaciones = ','.join([e.text + '-' + str(e.start_char)  for e in cuerpo.ents if e.label_ == 'ORG'])
    lugares = ','.join([e.text + '-' + str(e.start_char)  for e in cuerpo.ents if e.label_ == 'LOC'])
    # fechas = ','.join([e.text for e in oracion.ents if e.label_ == 'DATE'])
    # geopoliticos = ','.join([e.text for e in oracion.ents if e.label_ == 'GPE'])
    # eventos = ','.join([e.text for e in oracion.ents if e.label_ == 'EVENT'])

    fila = dt.Frame({"id": [id], "fecha": [fecha], "cuenta": [cuenta], "texto" : [cuerpo.text.strip()],
                    "sustantivos" : [sustantivos], "adjetivos" : [adjetivos], "verbos" : [verbos],
                    "personas" : [personas], "organizaciones" : [organizaciones], "lugares" : [lugares],
                    "matcheos" : [','.join(matcheos)]})

    df.rbind(fila)

    i += 1

df.to_csv('tweets_inseguridad.csv')

