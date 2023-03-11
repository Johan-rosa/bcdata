get_icdv <- function() {

    # # Índice de Costos Directos de la Construcción de Viviendas
    # icdv:    Índice general
    # icdv_vm: Variación mensual
    # icdv_d:  Variación año corrido
    # icdv_vi: Variación doce meses

  `%>%` <- magrittr::`%>%`

  # URL del ICDV
  icdv_general_url <- rvest::read_html(
      'https://www.one.gob.do/datos-y-estadisticas/temas/estadisticas-economicas/precios/icdv/') %>%
    rvest::html_node('a[href*="1-icdv-general"]') %>%
    rvest::html_attr('href') %>%
    stringr::str_replace('según', 'seg%C3%BAn') %>%
    stringr::str_replace('año', 'a%C3%B1o') %>%
    paste0('https://www.one.gob.do', .)
  # Descargando el archivo temporal
  temporal_file <- tempfile(fileext = '.xlsx')
  # Descargando el archivo
  response <- httr::GET(icdv_general_url)
  bin <- httr::content(response, 'raw')
  writeBin(bin, temporal_file)
  # Reading the file
  readxl::read_excel(temporal_file, skip = 7) %>%
    setNames(c('mes', 'icdv', 'icdv_vm', 'icdv_d', 'icdv_vi')) %>%
    dplyr::filter(!is.na(mes)) %>%
    dplyr::mutate(year = ifelse(stringr::str_detect(mes, '^[0-9]+$'), mes, NA)) %>%
    tidyr::fill(year) %>%
    dplyr::filter(!is.na(icdv)) %>%
    dplyr::mutate(
      year = as.numeric(year),
      mes = bcdata::crear_mes(mes),
      periodo = lubridate::make_date(year = year, month = mes, day = '01')
    ) %>%
    dplyr::select(periodo, dplyr::contains('icdv'))
}
