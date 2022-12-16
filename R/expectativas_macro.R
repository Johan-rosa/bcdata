# Funciones para descargar datos de encuestas macroeconomicas


# Encuesta de Expectativas Macroeconómicas ----

get_em_eem <- function(medida = "promedio" # promedio, mediana y
                                           # dv (desviación estándar)
                               ){
    # Ajuste para usar el pipe sin cargar dplyr
    `%>%` <- magrittr::`%>%`

    # Enlace
    file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                       "politica-monetaria/expectativas-macroeconomicas/",
                       "documents/Historico-EEM.xlsx?v=1670880703924")

    # Directorio
    file_path <- tempfile(pattern = "", fileext = "")

    # Descarga archivo
    download.file(file_url, file_path, mode = "wb", quiet = TRUE)

    # hojas del libro
    medida <- dplyr::case_when(
        medida == "promedio" ~ "Promedio",
        medida == "mediana" ~ "Mediana",
        medida == "dv" ~ "Desviación estándar"
    )



    # archivo
    expectativas <- readxl::read_excel(file_path,
                                       sheet = medida,
                                       skip = 9,
                                       col_types = c(rep('numeric', 17)),
                                       col_names = c('año', 'mes',
                                                     'inflacion_año_actual',
                                                     'inflacion_12_meses',
                                                     'inflacion_año_siguiente',
                                                     'inflacion_24_meses',
                                                     # Variación porcentual II TC
                                                     'vitc_año_actual',
                                                     'vitc_12_meses',
                                                     'vitc_año_siguiente',
                                                     'vitc_24_meses',
                                                     # creciemiento Int. PIB real
                                                     'cpib_trimestre_actual',
                                                     'cpib_año_actual',
                                                     'cpib_año_siguiente',
                                                     # Tasa de política monetaria
                                                     'tpm_mes_actual',
                                                     'tpm_trimestre_actual',
                                                     'tpm_año_actual',
                                                     'tpm_12_meses')) %>%
        dplyr::mutate(periodo = lubridate::make_date(año, mes)) %>%
      dplyr::select(periodo, año:tpm_12_meses)



    return(expectativas)
}


# Encuesta de opinión empresarial ----



get_em_eoe <- function(temporalidad = "mensual"){

  # Ajuste para usar el pipe sin cargar dplyr
  `%>%` <- magrittr::`%>%`

  # Enlace
  periocidad <- ifelse(temporalidad == "mensual",
                       "documents/Historico-EOE-(Mensual).xlsx?v=1670985585615",
                       "documents/Historico-EOE-(Trimestral).xlsx?v=1670985585615")

  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "politica-monetaria/expectativas-macroeconomicas/",
                     periocidad)



  # Directorio
  file_path <- tempfile(pattern = "", fileext = "")

  # Descarga archivo
  download.file(file_url, file_path, mode = "wb", quiet = TRUE)


  if(temporalidad == "mensual"){

  # archivo mesual
  opinion_empresarial_mensual <- readxl::read_excel(file_path,
                                     skip = 6,
                                     col_types = c('numeric', 'guess',
                                                   rep('numeric', 11))) %>%
    janitor::clean_names() %>%
    # dplyr::mutate(periodo = lubridate::make_date(ano, mes))
    # dplyr::select(periodo, "año" = ano  ) %>%
    suppressMessages()

  return(opinion_empresarial_mensual)
  }

  # archivo trimestral
  else if (temporalidad == "trimestral"){

    opinion_empresarial_trimestral <- readxl::read_excel(file_path,
                                            skip = 4,
                                            col_types = c('guess',
                                                          rep('numeric', 19))) %>%
    janitor::clean_names() %>%
    # dplyr::filter(!is.na(produccion_respecto_al_trimestre_anterior)) %>%
    suppressMessages()

  return(opinion_empresarial_trimestral)
  }
}

get_em_eoe("trimestral")
