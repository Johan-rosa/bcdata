
crear_mes <- function(mes, type = "largo") {
    # creating the pipe
    `%>%` <- magrittr::`%>%`

    if(is.numeric(mes)){
        dplyr::recode(mes,
               `1` = "Enero",
               `2` = "Febrero",
               `3` = "Marzo",
               `4` = "Abril",
               `5` = "Mayo",
               `6` = "Junio",
               `7` = "Julio",
               `8` = "Agosto",
               `9` = "Septiembre",
               `10` = "Octubre",
               `11` = "Noviembre",
               `12` = "Diciembre")

    } else if(type == "corto"){

        dplyr::recode(mes,
               "Ene" = 01,
               "Feb" = 02,
               "Mar" = 03,
               "Abr" = 04,
               "May" = 05,
               "Jun" = 06,
               "Jul" = 07,
               "Ago" = 08,
               "Sep" = 09,
               "Oct" = 10,
               "Nov" = 11,
               "Dic" = 12)
    }
}

#' Descarga las series del IPC
#'
#' Descarga las series del IPC de la Republica Dominicana
#' con diferentes desagregaciones
#'
#' @param desagregacion string indicando la desagregacion deseada. opciones:
#' "general", "grupos", "regiones", "subyacente", "tnt" (transable y no transable)
#'
#' @return Un tibble con las series del ipc con la desagregacion deseada
#' @examples
#' get_ipc_data()
#' get_ipc_data(desagregacion = "grupos")
#' get_ipc_data(desagregacion = "regiones")



# Funcion para descargar data del IPC
get_ipc_data <- function(desagregacion = "general"){

    # Asignando el pipe para usarlo sin cargar dplyr
    `%>%` <- magrittr::`%>%`

    if(desagregacion == "general") {
        # Descarga el ipc general  ---------------------------
        url_descarga <- paste0("https://cdn.bancentral.gov.do/documents/",
                               "estadisticas/precios/documents/",
                               "ipc_base_2010.xls?v=1570116997757")
        # directorio de descarga
        file_path <- tempfile(pattern = "", fileext = ".xls")

        # descarga el archivo
        download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

        suppressMessages(
            # leer el archivo
            ipc_general <- readxl::read_excel(
                file_path,
                sheet = 1,
                col_names = FALSE,
                skip = 7)
            )

        # Adecuando el archivo
        ipc_general <- ipc_general %>%
            janitor::clean_names() %>%
            dplyr::select(x1:x7) %>%
            setNames(
                c("year", "mes", "ipc","ipc_vm", "ipc_vd", "ipc_vi", "ipc_p12")
            ) %>%
            dplyr::filter(!is.na(mes)) %>%
            dplyr::mutate(
                fecha = seq(lubridate::ymd("1984/01/01"),
                            by = "month",
                            length.out = nrow(.)),
                year = lubridate::year(fecha)
            ) %>%
            dplyr::select(fecha, year, mes, everything())

        return(ipc_general)

    } else if(desagregacion == 'grupos') {

        # IPC por grupo de bienes y servicios ------------------------------------------
        url_descarga <- paste0(
            "https://cdn.bancentral.gov.do/documents/estadisticas/",
            "precios/documents/ipc_grupos_base_2010.xls?v=1570117393117"
        )

        # directorio de descarga
        file_path <- tempfile(pattern = "", fileext = ".xls")

        # descarga el archivo
        download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

        # header del dataframe
        header_ipc_grupos <- c(
            "fecha", "ipc_ayb", "ipc_ayb_vm", "ipc_alcohol_tabaco",
            "ipc_alcohol_tabaco_vm", "ipc_ropa_calzado", "ipc_ropa_calzado_vm",
            "ipc_vivienda", "ipc_vivienda_vm",
            "ipc_muebles", "ipc_muebles_vm", "ipc_salud", "ipc_salud_vm",
            "ipc_transporte", "ipc_transporte_vm", "ipc_comunicaciones",
            "ipc_comunicaciones_vm", "ipc_cultura", "ipc_cultura_vm", "ipc_educacion",
            "ipc_educacion_vm", "ipc_hotel_restaurantes", "ipc_hotel_restaurantes_vm",
            "ipc_bines_servicios", "ipc_bienes_servicios_vm"
        )

        suppressMessages(
        # Importar archivos
        ipc_grupos <- readxl::read_excel(
            file_path,
            skip = 10,
            col_names = F,
            na = "-"
        ))

        # adecuando el archivo
        ipc_grupos <-
            ipc_grupos %>%
            janitor::clean_names() %>%
            dplyr::select(x1:x25) %>%
            setNames(header_ipc_grupos) %>%
            dplyr::filter(!is.na(ipc_ayb)) %>%
            dplyr::mutate(
                fecha = seq(lubridate::ymd('1999/01/01'),
                            by = "month",
                            length.out = nrow(.)),
                year = lubridate::year(fecha),
                mes = crear_mes(mes = lubridate::month(fecha), type = "largo")) %>%
            dplyr::select(fecha, year, mes, everything())

        return(ipc_grupos)

    } else if(desagregacion == 'regiones') {
        # IPC por regiones ---------------------------------------

        # Header ipc por regiones
        header_ipc_regiones <- c(
            "year", "mes", "ipc_ozama", "ipc_ozama_vm", "ipc_cibao",
            "ipc_cibao_vm", "ipc_este", "ipc_este_vm", "ipc_sur",
            "ipc_sur_vm")

        # url de descarga
        url_descarga <-  paste0(
            "https://cdn.bancentral.gov.do/",
            "documents/estadisticas/precios/documents/",
            "ipc_regiones_base_2010.xls?v=1570117393117"
        )

        # ruta del archivo
        file_path <- tempfile(pattern = "", fileext = ".xls")

        # descarga el archivo
        download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

        suppressMessages(
        # importar files con ipc por regiones
        ipc_region <- readxl::read_excel(
            file_path,
            skip = 7,
            col_names = F
        ))

        # adecuando el archivo
        ipc_region <-
            ipc_region %>%
            purrr::set_names(header_ipc_regiones) %>%
            dplyr::filter(!is.na(mes)) %>%
            dplyr::mutate(
                fecha = seq(lubridate::ymd('2011/01/01'),
                            by = "month",
                            length.out = nrow(.)),
                year = lubridate::year(fecha),
                mes = crear_mes(mes = lubridate::month(fecha))) %>%
            dplyr::select(fecha, year, mes, everything())

        return(ipc_region)

    } else if(desagregacion == "subyacente") {
        # IPC según subyacente o no subyacente -----------------------

        #Header ipc subyacente o no subyacente
        header_ipc_subyacente <- c(
            "year", "mes", "ipc_subyacente", "ipc_subyacente_vm",
            "ipc_subyacente_vd", "ipc_subyacente_vi"
        )

        # url de descarga
        url_descarga <- paste0(
            "https://cdn.bancentral.gov.do/documents/",
            "estadisticas/precios/documents/",
            "ipc_subyacente_base_2010.xlsx?v=1570117393117"
        )

        # ruta del archivo
        file_path <- tempfile(pattern = "", fileext = ".xlsx")

        # descarga el archivo
        download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

        suppressMessages(
        # importar el archivo
        ipc_subyacente <- readxl::read_excel(
            file_path,
            skip = 25,
            col_names = F
        ))

        # adecuar el objeto
        ipc_subyacente <-
            ipc_subyacente %>%
            janitor::clean_names() %>%
            dplyr::select(x1:x6) %>%
            setNames(header_ipc_subyacente) %>%
            dplyr::mutate(
                fecha = seq(lubridate::ymd('2000/01/01'),
                            by = "month",
                            length.out = nrow(.)),
                year = lubridate::year(fecha),
                mes = crear_mes(mes = lubridate::month(fecha))) %>%
            dplyr::select(fecha, year, mes, everything()) %>%
            dplyr::filter(!is.na(ipc_subyacente))

        return(ipc_subyacente)

    } else if(desagregacion == "tnt") {
        # IPC de bienes transables y no transable -------------------------------

        # Header del data frame
        header_ipc_tnt <- c(
            "year", "mes", "ipc", "ipc_vm", "ipc_vd",
            "ipc_t", "ipc_t_vm", "ipc_t_vd", "ipc_nt",
            "ipc_nt_vm", "ipc_nt_vd"
        )

        # url de descarga de
        url_descarga <- paste0(
            "https://cdn.bancentral.gov.do/",
            "documents/estadisticas/precios/",
            "documents/ipc_tnt_base_2010.xls?v=1570117393117"
        )

        # ruta del archivo
        file_path <- tempfile(pattern = "", fileext = ".xls")

        # descarga el archivo
        download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

        suppressMessages(
        # importar archivo
        ipc_tnt <- readxl::read_excel(
            file_path,
            skip = 27,
            col_names = F,
            na = "-"
        )
        )

        # Adecuando el objeto
        ipc_tnt <- ipc_tnt %>%
            janitor::clean_names() %>%
            setNames(header_ipc_tnt) %>%
            dplyr::filter(!is.na(mes)) %>%
            dplyr::mutate(
                fecha = seq(lubridate::ymd('1999/02/01'),
                            by = "month",
                            length.out = nrow(.)),
                year = lubridate::year(fecha),
                mes = crear_mes(mes = lubridate::month(fecha))) %>%
            dplyr::select(fecha, year, mes, everything())

        return(ipc_tnt)

    }

}
