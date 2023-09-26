# Bolsa de palabras

lista = [
'inseguridad',
'crimen',
'crímenes',
'criminal',
'delito',
'robo',
'robar',
'homicidio',
'narcotráfico',
'narco',
'policía',
'policial',
'gendarme',
'prefectura',
'taser',
'detenidos',
'femicidio',
'violencia',
'cárcel',
'prisión',
'denuncia',
'clandestino']


import spacy
from datatable import dt, f

noticias = dt.fread('data/notis_09-2023.csv',  header = True)

import spacy
from datatable import dt, f
import pandas as pd

noticias = dt.fread('data/notis_09-2023.csv',  header = True)

noticias.head()

nlp = spacy.load('es_core_news_md')

i = 0
textos = noticias[:, 'texto'].to_list()[0]

df = dt.Frame(id_noticia=[], diario=[], seccion=[], fecha=[], texto=[],
              sustantivos=[], adjetivos=[], verbos=[],
              personas=[], organizaciones=[], lugares=[], matcheos=[])

total = str(len(textos))
conteo_terminos = {}
for texto in textos:

    print(str(i) + ' de ' + total)

    # oraciones = list(nlp(texto).sents)
    # for oracion in oraciones:

    if len(texto.strip()) == 0 or len(texto.strip()) < 10:
        i += 1
        continue

    conteo = 0
    matcheos = []
    for termino in lista:
        conteo += int(termino in texto)
        if termino in texto:
            matcheos.append(termino)

    if conteo < 4 or conteo / len(texto) < 0.001:
        i += 1
        continue

    cuerpo = nlp(texto)

    diario = noticias[i,0]
    seccion = noticias[i,1]
    fecha = noticias[i,2][:10]
    titulo = noticias[i,3]

    sustantivos = ','.join([t.lemma_.lower() + '-' + str(t.idx) for t in cuerpo if t.pos_ == 'NOUN'])
    adjetivos = ','.join([t.lemma_.lower() + '-' + str(t.idx)  for t in cuerpo if t.pos_ == 'ADJ'])
    verbos = ','.join([t.lemma_.lower() + '-' + str(t.idx)  for t in cuerpo if t.pos_ == 'VERB'])

    personas = ','.join([e.text + '-' + str(e.start_char) for e in cuerpo.ents if e.label_ == 'PER'])
    organizaciones = ','.join([e.text + '-' + str(e.start_char)  for e in cuerpo.ents if e.label_ == 'ORG'])
    lugares = ','.join([e.text + '-' + str(e.start_char)  for e in cuerpo.ents if e.label_ == 'LOC'])
    # fechas = ','.join([e.text for e in oracion.ents if e.label_ == 'DATE'])
    # geopoliticos = ','.join([e.text for e in oracion.ents if e.label_ == 'GPE'])
    # eventos = ','.join([e.text for e in oracion.ents if e.label_ == 'EVENT'])

    fila = dt.Frame({"id_noticia": [i], "diario": [diario], "seccion": [seccion], "fecha" : [fecha], "texto" : [cuerpo.text.strip()],
                        "sustantivos" : [sustantivos], "adjetivos" : [adjetivos], "verbos" : [verbos],
                        "personas" : [personas], "organizaciones" : [organizaciones], "lugares" : [lugares],
                        "matcheos" : [','.join(matcheos)]})

    df.rbind(fila)

    i += 1

df.to_csv('data/noticias_inseguridad.csv')



