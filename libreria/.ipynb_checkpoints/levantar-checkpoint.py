import os, csv, sys, platform

def levantar():
    barra = '\\'
    if platform.system() == 'Linux':
        barra = '/'

    archivos = [archivo for archivo in os.listdir('.' + barra + 'noticias') if archivo.endswith('.csv')]
    noticias = []

    for archivo in archivos:
        with open('.' + barra + 'noticias' + barra + archivo, 'rt', encoding="utf-8") as f:
            f.readline()  # Leer y descartar la primera fila que contiene información de columnas.

            csv.field_size_limit(1310720000)  # Configurar para que lea todo el contenido de la fila
            filas = csv.reader(f)  # Abrir el archivo como un CSV para poder iterarlo

            for diario, seccion, fecha, titulo, texto in filas:
                noticias.append((diario, seccion, fecha, titulo, texto))

    return noticias