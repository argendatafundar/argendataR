
<!-- README.md is generated from README.Rmd. Please edit that file -->

# argendataR <a><img src="inst/hex_sticker_transparent_border.png" width="400" align="right" /></a>

<!-- badges: start -->
<!-- badges: end -->

Este paquete reune un conjunto de funciones para facilitar el flujo de
trabajo usando con las fuentes utilizadas por investigadores y los _outputs_ generados en 
el [proceso de armonziación (ETL)](https://github.com/argendata/etl/) [^1]. 

[^1]:Las salidas finales son las que luego se comaprten y actualizan en 
<https://github.com/argendata/data> y alimentan el sitio <https://argendata.fund.ar/> para descarga de datos.  


## Requisito

Para usar muchas de las funciones de este paquete es necesario tener
seteado en el `.Renviron` la variable `‘ARGENDATA_DRIVE’` con el _id_ del
directorio de Argendata. Para editar el `.Renviron` se puede usar
`usethis::edit_r_environ()`. Allí se declara de la siguiente forma:

    ARGENDATA_DRIVE='xaskooasdklaslkaldd'
    RUTA_FUENTES = "/ruta/al/directorio/etl-fuentes/"
    IP_FUENTES = "ruta-ip/al/etl-fuentes/"

## Instalacion

Para instalar el paquete se puede clonar este proyecto y luego abrir el
proyecto y usar la función

``` r
devtools::install_local()
```

La función también se puede usar sin abrir el proyecto pero hay que
pasarle la ruta a la carpeta del proyecto de `argendataR`.

Para instalar directo desde github se puede usar

``` r
devtools::install_github("argendata/argendataR")
```

Pero esta función requiere que esté configurada la variable de entorno
`‘GITHUB_PAT’` con el token del `PAT` de github del usuario, con el que debe
tener los permisos necesarios para acceder al repositorio.

## Fuentes

Para el trabajo con fuentes hay dos grandes conjuntos de funciones: `raw` y
`clean`. Las primeras se usan para el proceso con fuentes en su estado
original, declaradas por investigadores en su proceso de producción de contenidos;
 las segundas para trabajar con fuentes ya limpias o procesadas. 

Con `agregar_fuente_raw()` se registra una nueva fuente en la `sheet` de fuentes
crudas y carga en la carpeta de `BASES DE DATOS/Fuentes/raw` el archivo de
`path_raw`. A cada fuente se le asigna automaticamente un id numerico que
permite seleccionar la fuente segun aparece en el `sheet`. Para consultar
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
                          fecha_actualizar = "2023-01-01")

Esta función actualiza ‘fecha_descarga’ y ‘fecha_actualizar’ de una
fuente en la sheet de fuentes en el drive de Argendata. Hace
`google::drive_upload()` de la fuente con `overwrite = T` pisando la
version anterior en el drive. Las fuentes raw se pueden consultar con
`fuentes_raw()`.

Existen funciones analogas para el trabajo con _fuentes clean_. Estas
refieren a cada salida (en general un archivo `.csv`) disponibilizada
a partir de una fuente descargada que debería cumplir con una minima limpieza de nombres de
columnas y estar en formato `long`, generalmente. Las funciones no evalúan esas
condiciones pero ayudan en su registro sistematico. Vean los siguientes.

**Bloque 1**


    agregar_fuente_clean(id_fuente_raw = 1,
                         path_clean = "ejemplo.csv",
                         nombre = "Ejemplo 1",
                         script = "ejemplo1.R"
                         )

**Bloque 2**


    agregar_fuente_clean(id_fuente_raw = 1,
                         path_clean = "ejemplo.csv",
                         nombre = "Ejemplo 2",
                         script = "ejemplo2.R"
                         )

Muchas fuentes limpias (_clean_) se pueden relacionar a una misma fuente cruda (_raw_) pero
cada una debería ser unica. Un bloque de codigo como los anteriores
debería aparecer dentro del _script_ que genera cada fuente limpia y
usarse solo la primera vez que se ejecute el script. Las fuentes limpias se
pueden consultar con `fuentes_clean()`. Sucesivas actualizaciones de una
fuente clean ya registrada deberían usar en cambio:

    actualizar_fuente_clean(id_fuente_clean = 1)

Esta función actualiza `‘fecha’` de una fuente en la sheet de fuentes
clean en el drive de Argendata y hace `google::drive_upload()` de la
fuente con `overwrite = T` pisando la version anterior en el drive.

## Outputs

El paquete tambien incluye funciones para el trabajo con _outputs_. La
principal funcion es `write_output()`. Esta funcion genera un `json` con
la _data_ y _metadata_ que el proyecto de Argendata requiere para cada
conjunto de datos. Opcionalmente también escribe un `csv` usando el estándar de
Argendata (`UTF-8`, `double quote` agresivo, eol `“\n”, na = ““`). Esta
función debería aparecer al final de cada script de generación de
_outputs_.

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

## Reference



| Function                        | Description                                                    |
|---------------------------------|----------------------------------------------------------------|
| **Fuentes** |
| `actualizar_fuente_clean()`:    | Actualizar informacion de una fuente `clean`                     |
| `actualizar_fuente_raw()`:      | Actualizar informacion de una fuente `raw`                       |
| `agregar_fuente_clean()`:       | Agregar nueva fuente `clean`                                     |
| `agregar_fuente_raw()`:         | Agregar nueva fuente `raw`                                       |
| `descargar_fuente_clean()`:     | Descarga fuente version `clean`                                  |
| `descargar_fuente_raw()`:       | Descarga fuente version `raw`                                    |
| `fuentes()`:                    | Fuentes - listado                                              |
| `fuentes_clean()`:              | Fuentes clean - listado                                        |
| `fuentes_raw()`:                | Fuentes raw - listado                                          |
| **Aux** |
| `expansor_xvar()`:              | Expandir una serie usando variaciones de otra serie            |
| `get_nomenclador_geografico()`: | Devuelve el nomenclador geografico para argendata              |
| `subtopico_init` | Inicializa un nuevo subtopico |
| `script_subtopico()`:           | Crea un `.R` con el esquema basico de _script_ para _outputs_        |
| `subtopico_outputs()`:          | Consulta lista de outputs del subtopico/entrega en drive       |
| **Control - QA** |
| `comparar_outputs` | Set de indicadores de comparación entre datos de entrada (investigadores) y _outputs_ generados en proceso de aromnización |
| `check_cols` | Compara columnas de datos de entrada (investigadores) vs _outputs_ |
| `check_datatypes` | Compara tipo de datos de columnas de entrada (investigadores) vs _outputs_ |
| `control_valores_nonnum` | No description available |
| `control_valores_num` | No description available |
| `nuevos_na` | Contro de valores perdidos entre datos de entrada (investigadores) y _outputs_ generados |
| **Outputs** |
| `write_csv_fundar()`:           | write_csv estilo Fundar                                        |
| `write_output()`:               | Genera json y csv con metadata y data de output para argendata |


<div>
<a href="https://fund.ar">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://github.com/datos-Fundar/fundartools/assets/86327859/6ef27bf9-141f-4537-9d78-e16b80196959">
    <source media="(prefers-color-scheme: light)" srcset="https://github.com/datos-Fundar/fundartools/assets/86327859/aa8e7c72-4fad-403a-a8b9-739724b4c533">
    <img src="fund.ar"></img>
  </picture>
</a>
</div>
