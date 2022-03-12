    # url de descarga
    url <- paste0(
        "https://cdn.bancentral.gov.do/documents/estadisticas/",
        "sector-monetario-y-financiero/documents/serie_indicadores_bcrd.xlsx",
        "?v=1637785656143"
        )

# path temporal
temp_path <- tempfile(pattern = "", fileext = ".xlsx")

# descargando el archivo
download.file(url, temp_path, quiet = TRUE, mode = "wb")

# Importando la data
suppressMessages(
    indicadores_bcrd <- readxl::read_excel(
        path = temp_path,
        skip = 4,
        col_names = TRUE, n_max = 53, na = c("n.d.")
    )
)


create_date <- function(string) {

    string <- stringr::str_remove_all(string, "[//.*]")

    numerics <- stringr::str_subset(string, "[A-z]+", negate = TRUE)
    without_day <- stringr::str_subset(string, "^[A-z]+")
    with_day <- stringr::str_subset(string, "^[0-9]+-[A-z]+")

    numerics <- janitor::excel_numeric_to_date(as.numeric(numerics))

    without_day <- lubridate::make_date(
        stringr::str_replace(without_day, ".+([0-9]{2})$", "20\\1"),
        crear_mes(stringr::str_extract(without_day, "[A-z]+")),
        1
    )

    with_day <- lubridate::make_date(
        stringr::str_replace(with_day, ".+([0-9]{2})$", "20\\1"),
        crear_mes(stringr::str_extract(with_day, "[A-z]+")),
        stringr::str_extract(with_day, "^[0-9]+")
    )

    c(numerics, without_day, with_day)

}

#datapasta::dpasta(tibble::as_tibble(clipr::read_clip_tbl()))

elements <- tibble::tribble(
                                                                                                                        ~original_names,                                                  ~labels,                         ~short_names,                   ~categoria, ~nivel,
                                                                          "ACTIVOS INTERNACIONALES BRUTOS (Activos Externos) (US$) (1)",                                       "Activos externos",     "activos_internacionales_brutos",           "Indicadores BCRD",     1L,
                                                               "RESERVAS INTERNACIONALES BRUTAS (Activos de Reserva Oficial) (US$) (1)",                        "Reservas internacionales brutas",    "reservas_internacionales_brutas",           "Indicadores BCRD",     1L,
                                                                                           "RESERVAS INTERNACIONALES NETAS   (US$) (1)",                         "Reservas internacionales netas",     "reservas_internacionales_netas",           "Indicadores BCRD",     1L,
                                                                                                                     "ACTIVOS INTERNOS",                                       "Activos internos",                   "activos_internos",           "Indicadores BCRD",     1L,
                                                                                               "Activos frente al Gobierno Central (2)",                     "Activos frente al gobierno central",            "activos_internos_vs_gob",           "Indicadores BCRD",     2L,
                                                                                            "De los cuales: Bono de Capitalización (3)",             "Bonos de capitalización frente al gobierno", "activos_internos_capitalizacion_bc",           "Indicadores BCRD",     3L,
                                                                                                 "Activos frente al Sector Privado (4)",                       "Activos frente al sector privado",       "activos_internos_vs_sprivado",           "Indicadores BCRD",     2L,
                                                                                           "Crédito a Otras Sociedades de Depósito (5)",                                          "Crédito a OSD",                        "credito_osd",           "Indicadores BCRD",     2L,
                                                                                                           "VALORES EN CIRCULACION (6)",                                 "Valores en circulación",             "valores_en_circulacion",           "Indicadores BCRD",     1L,
                                                                                                        "Descuento en Valores Emitidos",                          "Descuento en valores emitidos",               "descuento_en_valores",           "Indicadores BCRD",     2L,
                                                                                                 "DEPOSITOS REMUNERADOS DE CORTO PLAZO",                               "Depósitos remunerados CP",        "depositos_remunerados_corto",           "Indicadores BCRD",     1L,
                                                                                                           "BASE MONETARIA RESTRINGIDA",                             "Base monetaria restringida",         "base_monetaria_restringida",             "Base monetaria",     1L,
                                                                                                          "Billetes y Monedas Emitidos",                            "Billetes y monedas emitidos",                   "billetes_monedas",             "Base monetaria",     2L,
                                                                                                      "De los que: Tenencias OSD en MN",                              "Billetes y monedas en OSD",            "billetes_monedas_osd_mn",             "Base monetaria",     3L,
                                                                    "Depósitos Encaje Legal y Saldos de Compensación de OSD en BC (MN)",                           "Depositos encaje legal en MN",   "depositos_encaje_compensacion_mn",             "Base monetaria",     2L,
                                                              "Valores del BCRD en posesión de las OSD para fines de encaje legal (MN)",                    "Valores del BCRD en OSD para encaje",      "valores_bcrd_en_osd_encaje_mn",             "Base monetaria",     2L,
                                                                                                              "BASE MONETARIA AMPLIADA",                                "Base monetaria ampliada",            "base_monetaria_ampliada",             "Base monetaria",     1L,
                                                                                                           "Base Monetaria Restringida",                             "Base monetaria restringida",                   "base_restringida",             "Base monetaria",     2L,
                                                                    "Depósitos Encaje Legal y Saldos de Compensación de OSD en BC (ME)",                           "Depósitos encaje legal en ME",   "depositos_encaje_compensacion_me",             "Base monetaria",     2L,
                                                                                     "Depósitos Remunerados de Corto Plazo (Overnight)",                                    "Depósitos Overnight",    "depositos_remunerados_overnight",             "Base monetaria",     2L,
                                                                                           "Depósitos Remunerados de Corto Plazo en ME",                         "Depósitos remunerados CP en ME",           "depositos_remunerados_me",             "Base monetaria",     2L,
                                                                                                       "Otros Depósitos de OSD en BCRD",                         "Otros depósitos de OSD en BCRD",          "otros_depositos_osd_en_bc",             "Base monetaria",     2L,
                                                                  "Valores a Corto Plazo emitidos por BC en manos de las OSD (MN y ME)",                          "Valores de CP del BCRD en OSD",     "valores_corto_plazo_bc_en_osde",             "Base monetaria",     2L,
                                                                                                                "MEDIO CIRCULANTE (M1)",                                       "Medio círculante",                                 "m1",       "Agregados monetarios",     1L,
                                                                                              "Billetes y monedas en poder del público",                 "Billetes y monedas en poder de público",                "billetes_monedas_pp",       "Agregados monetarios",     2L,
                                                                                                        "Depósitos Transferibles en MN",                          "Depósitos transferibles en MN",         "depositos_transferibles_mn",       "Agregados monetarios",     2L,
                                                                                                       "OFERTA MONETARIA AMPLIADA (M2)",                              "Oferta monetaria ampliada",                                 "m2",       "Agregados monetarios",     1L,
                                                                                                                "Medio Circulante (M1)",                                                     "M1",                                 "m1",       "Agregados monetarios",     2L,
                                                                                                                  "Otros depósitos M/N",                                  "Otros depósitos en MN",                 "otros_depositos_mn",       "Agregados monetarios",     2L,
                                                                                    "Valores distintos de acciones MN-emitidos por OSD",      "Valores distintos de acciones MN-emitidos por OSD",      "valores_no_acciones_de_osd_mn",       "Agregados monetarios",     2L,
                                                                               "Valores distintos de acciones MN- emitidos por el BCRD", "Valores distintos de acciones MN- emitidos por el BCRD",     "valores_no_acciones_de_bcrd_mn",       "Agregados monetarios",     2L,
                                                                                                        "DINERO EN SENTIDO AMPLIO (M3)",                               "Dinero en sentido amplio",                                 "m3",       "Agregados monetarios",     1L,
                                                                                                       "Oferta Monetaria Ampliada (M2)",                              "Oferta monetaria ampliada",                                 "m2",       "Agregados monetarios",     2L,
                                                                                                                  "Otros depósitos M/E",                                  "Otros depósitos en ME",                 "otros_depositos_me",       "Agregados monetarios",     2L,
                                                                                                     "Valores distintos de acciones ME",                    "Valores distintos de acciones en ME",             "valores_no_acciones_me",       "Agregados monetarios",     1L,
                                                                                                             "K2.1 = M1/BM restringida",                                                     "K1",                               "k2.1", "Multiplicadores monetarios",     1L,
                                                                                                             "K2.2 = M2/BM restringida",                                                     "K2",                               "k2.2", "Multiplicadores monetarios",     1L,
                                                                                                                "K2.3 = M3/BM ampliada",                                                     "K3",                               "k2.3", "Multiplicadores monetarios",     1L,
                                                                                                                       "TASA DE CAMBIO",                                         "Tasa de cambio",                        "tasa_cambio",             "Tipo de cambio",     1L
                                                              )


# Limpiando los datos (quitando filas sin datos y títulos)
indicadores_bcrd <- indicadores_bcrd %>%
    dplyr::filter(!is.na(`INDICADORES BANCO CENTRAL`)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(remove = any(!is.na(dplyr::c_across(-`INDICADORES BANCO CENTRAL`)))) %>%
    dplyr::filter(remove) %>%
    dplyr::select(-remove) %>%
    dplyr::bind_cols(elements)

indicadores_bcrd %>%
    dplyr::rename(descripcion =  `INDICADORES BANCO CENTRAL`) %>%
    tidyr::pivot_longer(cols = -c(descripcion, short_names, categoria, nivel, original_names, labels),
                        names_to = "date", values_to = "values") %>%
    dplyr::mutate(
        date = create_date(date)
    )









