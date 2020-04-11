# Funciones para descargar las tasas del sistema funanciero

get_tasas_bm <- function(tipo = 'pasivas') {
    # Ajuste para usar el pipe sin cargar dplyr
    `%>%` <- magrittr::`%>%`

    suppressMessages(
    if(tipo == "pasivas") {
        # Tasas pasivas -------------------------------------------------------
        # Enlaces de descarga de archivos
        url_tasas_pasivas_91_07 <- paste0("https://cdn.bancentral.gov.do/documents/",
                                          "estadisticas/sector-monetario-y-financiero/",
                                          "documents/tbm_pasiva-1991-2007.xls?v=1570566381090")

        url_tasas_pasivas_08_12 <- paste0("https://cdn.bancentral.gov.do/documents/",
                                          "estadisticas/sector-monetario-y-financiero/",
                                          "documents/tbm_pasivad-2008-2012.xls?v=1570566381090")

        url_tasas_pasivas_13_16 <- paste0("https://cdn.bancentral.gov.do/documents/",
                                          "estadisticas/sector-monetario-y-financiero/",
                                          "documents/tbm_pasivad-2013-2016.xlsx?v=1570566381090")

        url_tasas_pasivas_17 <- paste0("https://cdn.bancentral.gov.do/documents/",
                                        "estadisticas/sector-monetario-y-financiero/",
                                        "documents/tbm_pasivad.xlsx?v=1570566381090")

        # Directorios
        pasivas_9107_path <- tempfile(pattern = "", fileext = "")
        pasivas2_0812_path <- tempfile(pattern = "", fileext = "")
        pasivas3_1316_path <- tempfile(pattern = "", fileext = "")
        pasivas4_17_path <- tempfile(pattern = "", fileext = "")

        # descargando los excel
        download.file(url_tasas_pasivas_91_07, pasivas_9107_path, mode = "wb")
        download.file(url_tasas_pasivas_08_12, pasivas2_0812_path, mode = "wb")
        download.file(url_tasas_pasivas_13_16, pasivas3_1316_path, mode = "wb")
        download.file(url_tasas_pasivas_17, pasivas4_17_path, mode = "wb")

        # importar los archivos
        pasivas_9107 <- readxl::read_excel(pasivas_9107_path, sheet = "Pasivas",
                                   col_names = FALSE, range = "B157:M266")

        pasivas_0812 <- readxl::read_excel(pasivas2_0812_path, sheet = "Pasivas",
                                   col_names = FALSE, range = "A14:O88")

        pasivas_1316 <- readxl::read_excel(pasivas3_1316_path, sheet = "Pasivas",
                                   col_names = FALSE, range = "A11:O67")

        pasivas_17 <- readxl::read_excel(pasivas4_17_path, sheet = "Pasivas",
                                 col_names = FALSE, skip = 10)

        # Adecuando archivos

        # adecuando Tasas pasivas 1991-2007 --- ---
        pasivas_9107 <-
            pasivas_9107 %>%
            janitor::clean_names() %>%
            setNames(
                c("tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360", "tp_m360",
                  "tp_ps", "tp_pp", "tp_dep_ahorros", "tp_preferencial", "tp_general",
                  "tp_interbancarios")
            ) %>%
            tidyr::drop_na() %>%
            dplyr::mutate(fecha = seq.Date(as.Date("2000/1/1"),
                                    by = "month",
                                    length.out = length(tp_30d)),
                   year = lubridate::year(fecha),
                   mes = lubridate::month(fecha),
                   mes = crear_mes(mes)) %>%
            dplyr::select(fecha, year, mes, dplyr::everything())


        # adecuando Tasas pasivas 2008-2012 --- ---

        pasivas_0812 <-
            pasivas_0812 %>%
            setNames(
                c("mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d",
                  "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros",
                  "tp_general", "tp_preferencial", "tp_interbancarios")
                ) %>%
            dplyr::filter(stringr::str_detect(mes, "^[A-Z]")) %>%
            dplyr::mutate(mes = stringr::str_extract(string = mes, pattern = "[A-z]+"),
                   fecha = seq.Date(from = as.Date("2008-01-01"), to = as.Date("2012-12-01"),
                                    by = "month"),
                   year = lubridate::year(fecha)) %>%
            dplyr::select(fecha, year, mes, everything())

        # adecuando Tasas pasivas 2013-2016 --- ---

        pasivas_1316 <-
            pasivas_1316 %>%
            setNames(
                c("mes", "tp_30d", "tp_60d", "tp_90d",  "tp_180d", "tp_360d",
                  "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros",
                  "tp_general", "tp_preferencial", "tp_interbancarios")
            ) %>%
            dplyr::filter(stringr::str_detect(mes, "^[A-Z]")) %>%
            dplyr::mutate(
                mes = stringr::str_extract(string = mes, pattern = "[A-z]+"),
                fecha = seq.Date(from = as.Date("2013-01-01"),
                                 to = as.Date("2016-12-01"),
                                 by = "month"),
                year = lubridate::year(fecha)) %>%
            dplyr::select(fecha, year, mes, everything())

        # adecuando Tasas pasivas 2017 en adelante --- ---

        pasivas_17 <-
            pasivas_17 %>%
            setNames(
                c("mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d",
                  "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros",
                  "tp_general", "tp_preferencial", "tp_interbancarios")
                ) %>%
            tidyr::drop_na() %>%
            dplyr::filter(stringr::str_detect(mes, "[A-Z]+")) %>%
            dplyr::mutate(mes = stringr::str_extract(string = mes, pattern = "[A-z]+"),
                   fecha = seq.Date(from = as.Date("2017-01-01"), by = "month",
                                    length.out = nrow(.)),
                   year = lubridate::year(fecha))%>%
            dplyr::select(fecha, year, mes, everything())

        tasas_pasivas <-
            dplyr::bind_rows(pasivas_9107,
                      pasivas_0812,
                      pasivas_1316,
                      pasivas_17)

        return(tasas_pasivas)

    } else if (tipo == "activas") {
        # Tasas activas ------------------------------------------------------------
        # url para descargar archivos --- --- --- --- --- --- --- --- --- --- ---- -
        url_activas_9107 <- paste0("https://cdn.bancentral.gov.do/documents/",
                                   "estadisticas/sector-monetario-y-financiero/",
                                   "documents/tbm_activa-1991-2007.xls?v=1570134897519")

        url_activas_0812 <- paste0("https://cdn.bancentral.gov.do/documents/",
                                   "estadisticas/sector-monetario-y-financiero/",
                                   "documents/tbm_activad-2008-2012.xls?v=1570198636254")

        url_activas_1316 <- paste0("https://cdn.bancentral.gov.do/documents/",
                                   "estadisticas/sector-monetario-y-financiero/",
                                   "documents/tbm_activad-2013-2016.xlsx?v=1570198636254")

        url_activas_17 <- paste0("https://cdn.bancentral.gov.do/documents/",
                                       "estadisticas/sector-monetario-y-financiero/",
                                       "documents/tbm_activad.xlsx?v=1570198636254")

         # paths temporales para los archivos --- --- --- --- --- --- --- --- --- --
        path_activas_9107 <- tempfile(pattern = "", fileext = ".xls")
        path_activas_0812 <- tempfile(pattern = "", fileext = ".xls")
        path_activas_1316 <- tempfile(pattern = "", fileext = ".xlsx")
        path_activas_17 <- tempfile(pattern = "", fileext = ".xlsx")

        # descargando los archivos --- --- --- --- --- --- --- --- --- --- --- --- -
        download.file(url_activas_9107, path_activas_9107, mode = "wb")
        download.file(url_activas_0812, path_activas_0812, mode = "wb")
        download.file(url_activas_1316, path_activas_1316, mode = "wb")
        download.file(url_activas_17, path_activas_17, mode = "wb")

        # Importando los archivos --- --- --- --- --- --- --- --- --- --- --- --- --
        # Tasas activas 1991-2007
        activas_9107 <- readxl::read_excel(path_activas_9107, sheet = "Activas",
                                   col_names = FALSE, range = "C157:O266")
        # Tasas activas 2008-2012
        activas_0812 <- readxl::read_excel(path_activas_0812, sheet = "Activas",
                                   col_names = FALSE, range = "A14:M85")

        # Tasas activas 2013-2016
        activas_1316 <- readxl::read_excel(path_activas_1316, sheet = "Activas",
                                   col_names = FALSE, range = "A10:M66")

        # Tasas activas 2017 en adelante
        activas_17 <- readxl::read_excel(path_activas_17, sheet = "Activas",
                                 col_names = FALSE, skip = 9)

        # Adecuando los archivos --- --- --- --- --- --- --- --- --- --- --- --- ---
        # adecuando Tasas activas 1991-2007
        activas_9107 <-
            activas_9107 %>%
            janitor::clean_names() %>%
            dplyr::select(-x4) %>%
            setNames(
                c("ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a",
                  "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio",
                  "ta_consumo", "ta_hipotecario")
            ) %>%
            tidyr::drop_na() %>%
            dplyr::mutate(
                fecha = seq.Date(as.Date("2000/1/1"),
                                 by = "month",
                                 length.out = length(ta_90d)),
               year = lubridate::year(fecha),
               mes = crear_mes(lubridate::month(fecha))
                   ) %>%
            dplyr::select(fecha, year, mes, everything())

        # adecuando Tasas activas 2008-2012
        activas_0812 <-
            activas_0812 %>%
            setNames(
                c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
                  "ta_m5a", "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio",
                  "ta_consumo", "ta_hipotecario")
                ) %>%
            dplyr::filter(stringr::str_detect(mes, "[A-z]+")) %>%
            dplyr::mutate(
                mes = stringr::str_extract(string = mes, pattern = "[A-z]+"),
                fecha = seq.Date(from = as.Date("2008-01-01"),
                                 to = as.Date("2012-12-01"),
                                 by = "month"),
                year = lubridate::year(fecha)) %>%
            dplyr::select(fecha, year, everything())

        # adecuando Tasas activas 2013-2016
        activas_1316 <-
            activas_1316 %>%
            setNames(
                c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a",
                  "ta_5a", "ta_m5a", "ta_ps", "ta_pp", "ta_preferencial",
                  "ta_comercio", "ta_consumo", "ta_hipotecario")
            ) %>%
            dplyr::filter(stringr::str_detect(mes, "[A-z]+")) %>%
            dplyr::mutate(
                mes = stringr::str_extract(string = mes, pattern = "[A-z]+"),
                fecha = seq.Date(from = as.Date("2013-01-01"),
                                 to = as.Date("2016-12-01"),
                                 by = "month"),
                year = lubridate::year(fecha)) %>%
            dplyr::select(fecha, year, everything())

        # adecuando Tasas activas 2017 en adelante
        activas_17 <-
            activas_17 %>%
            setNames(
                c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
                  "ta_m5a", "ta_pp", "ta_ps", "ta_comercio", "ta_consumo",
                  "ta_hipotecario", "ta_preferencial", "ta_preferencial_comercio",
                  "ta_preferencial_consumo", "ta_preferencial_hipotecario")
                ) %>%
            dplyr::filter(stringr::str_detect(mes, "[A-Z]+"), !is.na(ta_90d)) %>%
            dplyr::mutate(mes = stringr::str_extract(string = mes, pattern = "[A-z]+"),
                          fecha = seq.Date(from = as.Date("2017-01-01"), by = "month",
                                           length.out = nrow(.)),
                          year = lubridate::year(fecha)) %>%
            dplyr::select(fecha, year, mes, everything())

        # Consolidando archivos --- --- --- --- --- --- --- --- --- --- -- --- --- -
        tasas_activas <- dplyr::bind_rows(
                activas_9107, activas_0812,
                activas_1316,  activas_17)

        return(tasas_activas)

     }
    )

}
