get_llegadas_aereas <- function() {

}

`%>%` <- magrittr::`%>%`

# LLegadas de según aropuerto y estatus de residencia
urls <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-turismo/documents/",
    "lleg_total_", 1996:2020,
    ".xls")

temporal_paths <- purrr::map(
    1996:2020,
    ~tempfile(fileext = ".xls")
)

purrr::map2(
    urls, temporal_paths,
    ~download.file(url = .x, destfile = .y, mode = "wb", quiet = TRUE)
    )

llegadas1 <- purrr::map(
    temporal_paths[1:7],
    ~readxl::read_excel(
        path = .x, skip = 4
    ) %>% janitor::clean_names() %>%
        setNames(c("aeropuerto", "total", names(.)[-(1:2)])) %>%
        janitor::remove_empty(which = "rows") %>%
        janitor::remove_empty(which = "cols") %>%
        dplyr::mutate(
            residencia = stringr::str_extract(aeropuerto, "NO RESIDENTES|RESIDENTES|TOTAL"),
            nacionalidad = stringr::str_extract(
                aeropuerto, "DOMINICANOS|EXTRANJEROS|NO RESIDENTES|RESIDENTES|TOTAL")
        ) %>%
        tidyr::fill(residencia, nacionalidad, .direction = "down") %>%
        dplyr::filter(!aeropuerto %in% c("TOTAL PASAJEROS", "RESIDENTES",
                                 "DOMINICANOS", "EXTRANJEROS", "NO RESIDENTES"))
) %>% setNames(1996:2002) %>%
    dplyr::bind_rows(.id = "year") %>%
    dplyr::select(-total) %>%
    dplyr::filter(!residencia == nacionalidad)


llegadas2 <- purrr::map(
    temporal_paths[8:10],
    ~readxl::read_excel(
        path = .x, skip = 5
    ) %>% janitor::clean_names() %>%
        setNames(c("aeropuerto", "total", names(.)[-(1:2)]))%>%
        janitor::remove_empty(which = "rows") %>%
        janitor::remove_empty(which = "cols") %>%
        dplyr::mutate(
            residencia = stringr::str_extract(aeropuerto, "NO RESIDENTES|RESIDENTES|TOTAL"),
            nacionalidad = stringr::str_extract(
                aeropuerto, "DOMINICANOS|EXTRANJEROS|NO RESIDENTES|RESIDENTES|TOTAL")
        ) %>%
        tidyr::fill(residencia, nacionalidad, .direction = "down") %>%
        dplyr::filter(!aeropuerto %in% c("TOTAL PASAJEROS", "RESIDENTES",
                                         "DOMINICANOS", "EXTRANJEROS", "NO RESIDENTES"))
) %>% setNames(2003:2005) %>%
    dplyr::bind_rows(.id = "year") %>%
    dplyr::select(-total) %>%
    dplyr::filter(!residencia == nacionalidad)


llegadas3 <-
    purrr::map(
    temporal_paths[11:length(temporal_paths)],
    ~readxl::read_excel(
        path = .x, skip = 3
    ) %>% janitor::clean_names() %>%
        setNames(c("aeropuerto", "total", stringr::str_extract(names(.)[-(1:2)], "^..."))) %>%
        janitor::remove_empty(which = "rows") %>%
        janitor::remove_empty(which = "cols") %>%
        #dplyr::filter(!is.na(aeropuerto)) %>%
        dplyr::mutate(
            residencia = stringr::str_extract(aeropuerto, "NO RESIDENTES|RESIDENTES|TOTAL"),
            nacionalidad = stringr::str_extract(
                aeropuerto, "DOMINICANOS|EXTRANJEROS|NO RESIDENTES|RESIDENTES|TOTAL")
        ) %>%
        tidyr::fill(residencia, nacionalidad, .direction = "down") %>%
        dplyr::filter(!aeropuerto %in% c("TOTAL PASAJEROS", "RESIDENTES",
                                         "DOMINICANOS", "EXTRANJEROS", "NO RESIDENTES"))
) %>% setNames(2006:2020) %>%
    dplyr::bind_rows(.id = "year") %>%
    dplyr::select(-total) %>%
    dplyr::filter(!residencia == nacionalidad) %>%
    dplyr::filter(!is.na(ene))


dplyr::bind_rows(list(llegadas1, llegadas2, llegadas3)) %>%
    tidyr::pivot_longer(cols = ene:dic, names_to = 'mes', values_to = 'pasajeros') %>%
    janitor::remove_empty(which = 'cols')


# Llegadas detalles -------------------------------------------------------


get_turistas <- function() {

    url <- paste0('https://cdn.bancentral.gov.do/',
                 'documents/estadisticas/sector-turismo/',
                 'documents/lleg_caracteristicas_', 2006:2020
                 ,'.xls')
    `%>%` <- magrittr::`%>%`

detalles_year <- function(url) {
        `%>%` <- magrittr::`%>%`
        path <- tempfile(fileext = '.xls')
        download.file(url, path, quiet = TRUE, mode = 'wb')
        sheets <- readxl::excel_sheets(path) %>%
            stringr::str_subset('^[0-9]+$', negate = TRUE)

        headers <- c(
            "pais", "sexo_total", "sexo_femenino", "sexo_masculino", "alojamiento_total",
            "alojamiento_hotel", "alojamiento_otro", "edad_total", "edad_0a12",
            "edad_13a20", "x21a35", "x36a49", "x50mas", "motivo_total", "motivo_recreacion",
            "motivo_negocio", "motivo_conf", "motivo_estudio", "motivo_amigo_pareja",
            "motivo_otro", 'aeropuerto')

        pattern_region <- c('^America', 'Asia', 'Europa', 'Resto', 'AMERICA', 'ASIA',
                            'EUROPA', 'RESTO') %>%
            toupper() %>%
            paste(collapse = '|^')

        pattern_remove <- c('^TOTAL', 'RESIDENTES', 'NO RESIDENTES', 'Dominicanos', 'Extranjeros',
                            'EXTRANJEROS', 'Ext. Residentes.', 'Dom. Residentes.', 'Dom. No Residentes.',
                            'PAIS', 'Pais', 'pais') %>%
            paste(collapse = '|^')

        purrr::map(
            sheets,
            ~suppressMessages(readxl::read_excel(path, col_names = FALSE, sheet = .x)) %>%
                janitor::clean_names() %>%
                dplyr::select(x1:x20) %>%
                janitor::remove_empty(which = 'rows') %>%
                janitor::remove_empty(which = 'cols') %>%
                dplyr::mutate(aeropuerto = ifelse(stringr::str_detect(tolower(x1), 'aeropuerto'), x1, NA)) %>%
                tidyr::fill(aeropuerto, .direction = 'down') %>%
                tidyr::drop_na() %>%
                dplyr::filter(stringr::str_detect(x1, '^RESIDENCIA|^NACIONALIDAD', negate = TRUE)) %>%
                setNames(headers) %>%
                dplyr::mutate(region = ifelse(stringr::str_detect(pais, pattern_region), pais, NA)) %>%
                tidyr::fill(region, .direction = 'down') %>%
                dplyr::filter(stringr::str_detect(pais, pattern_remove, negate = TRUE)) %>%
                dplyr::filter(stringr::str_detect(pais, pattern_region, negate = TRUE)) %>%
                dplyr::mutate(mes = .x)
        ) %>%
            dplyr::bind_rows() %>%
            dplyr::mutate(year = readr::parse_number(stringr::str_extract(url, '[0-9]+\\.xls$'))) %>%
            dplyr::select(year, mes, aeropuerto, region, pais, dplyr::everything())

}

data_turistas <- purrr::map_df(
    url,
    detalles_year
)


}

data_turistas <- get_turistas()

data_turistas %>%
    dplyr::select(year, mes, aeropuerto, region, pais, dplyr::contains('sexo'))


