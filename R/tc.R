#' Descarga series del tipo de cambio
#'
#' Descarga las series de tipo promedio de las operaciones de compra y venta
#' de d\\u00f3lar en las entidades de intermediaci\\u00f3n financiera
#'
#' @param frecuencia La frecuencia de la deseada de la serie: "diaria", "mensual",
#'  "trimestral" y "anual"
#' @return un tibble con las serie del tipo de cambio de comprar y venta
#' @examples
#' get_tcambio(frecuencia = "mensual")
#' get_tcambio(frecuencia = "diaria")

# Descargando el tipo de cambio

get_tcambio <- function(frecuencia = "diaria") {
    # Para que el pipe funciones sin cargar dplyr
    `%>%` <-magrittr::`%>%`

    # url del archivo de tipo de cambio en la pagina del BC
    url_tc <-  paste0("https://cdn.bancentral.gov.do/documents/estadisticas/",
                       "mercado-cambiario/documents/TASA_ENTIDADES_FINANCIERAS",
                       ".xls?v=1570028955144")

    # Ruta temporarl a la que ira el archivo
    path_tc <- tempfile(pattern = "", fileext = ".xls")

    # Descargando el archivo
    download.file(url_tc, path_tc, mode = "wb", quiet = TRUE)

    suppressMessages(
        suppressWarnings(

    # importar el archivo
    if(frecuencia == "diaria") {
        # Importa el archivo tc diario
        tc_diario <- readxl::read_excel(path_tc, sheet = "Diaria", skip = 2)

        # adecuar el archivo
        tc_diario <- tc_diario %>%
            setNames(
                c("year", "mes", "dia","tcn_compra", "tcn_venta")
            ) %>%
            dplyr::mutate(mes = crear_mes(mes, "corto"),
                   fecha = lubridate::ymd(paste(year, mes, dia, sep = "-")),
                   mes = crear_mes(mes = mes)) %>%
            dplyr::select(fecha, year, mes, everything())

        # Output de la funcion
        return(tc_diario)

    } else if (frecuencia == "mensual"){
        # Importar archivo
        tc_mensual <- readxl::read_excel(path_tc, sheet = "Mensual", skip = 2)

        # Adecuando el archivo
        tc_mensual <- tc_mensual %>%
            setNames(
                c("year", "mes","tcn_compra", "tcn_venta")
                ) %>%
            dplyr::mutate(
                fecha = seq(as.Date("1992/2/1"),
                            by = "month",
                            length.out = length(year)),
                year = lubridate::year(fecha),
                mes = crear_mes(mes = lubridate::month(fecha))
                ) %>%
            dplyr::select(fecha, year, mes, everything())

        #Output de la funcion
        return(tc_mensual)

    } else if(frecuencia == "trimestral") {
        # Descargando archivo
        tc_trimestral <- readxl::read_excel(path_tc, sheet = "Trimestral", skip = 2)

        # Adecuando el archivo
        tc_trimestral <- tc_trimestral %>%
            setNames(
                c("year", "trimestre","tcn_compra", "tcn_venta")
            ) %>%
            dplyr::mutate(
                fecha = seq(as.Date("1992/1/1"),
                            by = "quarter",
                            length.out = length(year)),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
                ) %>%
            dplyr::select(fecha, year, trimestre, everything())

        # Output de la funcion
        return(tc_trimestral)

    } else if(frecuencia == "anual") {
        # Descargando el archivo
        tc_anual <- readxl::read_excel(path_tc, sheet = "Anual", skip = 2)

        # Adecuando el archivo
        tc_anual <- tc_anual %>%
            dplyr::rename(fecha = Año) %>%
            dplyr::select(fecha, everything())

        # Output de la funcion
        return(tc_anual)
    }
    )
    )


}
