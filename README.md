
<!-- README.md is generated from README.Rmd. Please edit that file -->

# argendataR

<!-- badges: start -->
<!-- badges: end -->

Este paquete es un conjunto de funciones para facilitar el flujo de
trabajo con fuentes y outputs en Argendata usando R.

## Installation

You can install the development version of argendataR like so:

``` r
# devtools::install_github("argendata/argendataR")
```

## Fuentes

Para el trabajo con fuentes hay dos grandes set de funciones`raw` y
`clean`. Las primeras se usan para el proceso con fuentes en su estado
original, las segundas para trabajar con fuentes ya limpias.

Con `agregar_fuente_raw()` se registra una fuente en la sheet de fuentes
raw y carga en la carpeta de `BASES DE DATOS/Fuentes/raw` el archivo de
path_raw. A cada fuente se le asigna automaticamente un id numerico que
permite seleccionar la fuente segun aparece en el sheet. Para consultar
ids usar `fuentes_raw()`.

    library(argendataR)
    ## basic example code
    agregar_fuente_raw(url = "https://www.ejemplo.com",
                       nombre = "Ejemplo",
                       institucion = "Ejemplo",
                       actualizable = TRUE,
                       fecha_descarga = "2021-01-01",
                       fecha_actualizar = "2022-01-01",
                       path_raw = "ejemplo.csv",
                       script = "ejemplo.R")

Un bloque de código análogo se espera que se use la primera vez que se
ejecute un script de descarga de una fuente desde su origen. Sucesivas
descargas de una fuente ya registrada deberían usar en cambio:


    actualizar_fuente_raw(id_fuente = 1,
                          fecha_descarga = "2022-01-01",
                          fecha_actualizar = "2023-01-01")

Esta función actualiza ‘fecha_descarga’ y ‘fecha_actualizar’ de una
fuente en la sheet de fuentes en el drive de Argendata. Hace
`google::drive_upload()` de la fuente con overwrite = T pisando la
version anterior en el drive. Las fuentes raw se pueden consultar con
`fuentes_raw()`.

Existen funciones analogas para el trabajo con fuentes clean. Las
fuentes clean refieren a cada csv disponibilizado a partir de una fuente
descargada que debería cumplir con una minima limpieza de nombres de
columnas y estar en formato longer. Las funciones no evalúan esas
condiciones pero ayudan en su registro sistematico. Vean los siguientes.

Bloque 1


    agregar_fuente_clean(id_fuente_raw = 1,
                         path_clean = "ejemplo.csv",
                         nombre = "Ejemplo 1",
                         script = "ejemplo1.R"
                         )

Bloque 2


    agregar_fuente_clean(id_fuente_raw = 1,
                         path_clean = "ejemplo.csv",
                         nombre = "Ejemplo 2",
                         script = "ejemplo2.R"
                         )

Muchas fuentes clean se pueden relacionar a una misma fuente raw pero
cada una debería ser unica. Un bloque de codigo como los anteriores
debería aparecer dentro del script que genera cada fuente limpia y
usarse solo la primera vez que se ejecute el script. Las fuentes raw se
pueden consultar con `fuentes_clean()`. Sucesivas actualizaciones de una
fuente clean ya registrada deberían usar en cambio:

    actualizar_fuente_clean(id_fuente_clean = 1,
                            fecha = "2022-01-01")

Esta función actualiza ‘fecha’ de una fuente en la sheet de fuentes
clean en el drive de Argendata y hace `google::drive_upload()` de la
fuente con overwrite = T pisando la version anterior en el drive.

## Outputs

El paquete tambien incluye funciones para el trabajo con outputs. La
principal funcion es `write_output()`. Esta funcion genera un json con
la data y metadata que el proyecto de Argendata requiere para cada
dataset. Opcionalmente también escribe un csv usando el estándar de
Argendata (UTF-8, double quote agresivo, eol “\n”, na = ““). Esta
función debería aparecer al final de cada script de generación de
outputs.

    write_output( data = df_output,
                    extension = 'csv',
                    output_name = 'A1_inb_pib',
                    subtopico = 'ACECON',
                    fuentes = 'World Development Indicators',
                    analista = 'Andrés Salles',
                    aclaraciones = NULL,
                    exportar = TRUE,
                    pk = c("anio", "iso3"),
                    es_serie_tiempo = TRUE,
                    columna_indice_tiempo = "anio",
                    columna_geo_referencia = "iso3",
                    nivel_agregacion = "pais",
                    nullables = FALSE,
                    etiquetas_indicadores = list("diferencia_inb_pbi" = "Diferencia entre Ingreso Bruto Nacional y PBI"),
                    unidades = "Porcentaje respecto al PBI",
                    classes = NULL)