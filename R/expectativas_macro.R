# Funciones para descargar datos de encuestas macroeconomicas


# Encuesta de Expectativas Macroeconómicas ----

get_em_eem <- function(medida = "promedio" # promedio, mediana y
                                           # dv (desviación estándar)
<<<<<<< HEAD
    ){
=======
                               ){

>>>>>>> d9bc13d93983ee064765975b73266614999a7418

    # Enlace
    file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                       "politica-monetaria/expectativas-macroeconomicas/",
                       "documents/Historico-EEM.xlsx?v=1670880703924")

    # Directorio
    file_path <- tempfile(pattern = "", fileext = "")

    # Descarga archivo
    download.file(file_url, file_path, mode = "wb", quiet = TRUE)

    # hojas del libro
    medida <- tolower(medida)
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
                                                     'tpm_12_meses')) |>
        dplyr::mutate(periodo = lubridate::make_date(año, mes)) |>
      dplyr::select(periodo, año:tpm_12_meses)



    return(expectativas)
}


# Encuesta de opinión empresarial ----

<<<<<<< HEAD


get_em_eoe <- function(mensual = TRUE){


=======
get_em_eoe <- function(mensual = TRUE){
  
>>>>>>> d9bc13d93983ee064765975b73266614999a7418
  # Enlace
  periocidad <- ifelse(mensual,
                       "documents/Historico-EOE-(Mensual).xlsx?v=1670985585615",
                       "documents/Historico-EOE-(Trimestral).xlsx?v=1670985585615")
  
  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "politica-monetaria/expectativas-macroeconomicas/",
                     periocidad)
<<<<<<< HEAD

=======
  
  
  
>>>>>>> d9bc13d93983ee064765975b73266614999a7418
  # Directorio
  file_path <- tempfile(pattern = "", fileext = "")
  
  # Descarga archivo
  download.file(file_url, file_path, mode = "wb", quiet = TRUE)
<<<<<<< HEAD


  # archivo mesual
  if(mensual){
   opinion_empresarial_mensual<-  readxl::read_excel(file_path,
=======
  
  
  if(mensual){
    
    # archivo mesual
    opinion_empresarial_mensual <-
      readxl::read_excel(file_path,
>>>>>>> d9bc13d93983ee064765975b73266614999a7418
                         skip = 6,
                         col_types = c('numeric', 'guess',
                                       rep('numeric', 11))) |>
      setNames(c( "year", "mes", "situacion_economica", "nivel_de_inventario",
                  "produccion", "pedidos", "empleo", "expectativa_produccion",
                  "expectativa_precios", "expectativa_situacion_economica",
                  "expectativa_empleo", "indice_de_confianza_industrial",
                  "indice_de_clima_empresarial")) |>
      dplyr::mutate(
<<<<<<< HEAD
          mes = bcdata::crear_mes(mes),
          year = as.numeric(year),
          periodo = lubridate::make_date(year = year, month = mes, "01")) |>
=======
        mes = bcdata::crear_mes(mes),
        year = as.numeric(year),
        periodo = lubridate::make_date(year = year, month = mes, "01")) |>
>>>>>>> d9bc13d93983ee064765975b73266614999a7418
      dplyr::select(periodo, situacion_economica:indice_de_clima_empresarial) |>
      suppressMessages()
    
    fecha <- tail(opinion_empresarial_mensual$periodo, n = 1)
    print("datos mensual")
    print(paste("Fecha", fecha))

<<<<<<< HEAD

   print("datos mensual")
   return(opinion_empresarial_mensual)


  } else if (!mensual){


  # datos trimestral
   opinion_empresarial_trimestral <- readxl::read_excel(file_path,
                                            skip = 4,
                                            col_types = c('guess',
                                                          rep('numeric', 19))) |>
        janitor::clean_names() |>
        dplyr::mutate(year = ifelse(stringr::str_detect(periodo, '^[0-9]+$'), periodo, NA)) |>
        tidyr::fill(year) |>
        dplyr::filter(!is.na(produccion_respecto_al_trimestre_anterior)) |>
        dplyr::mutate(
            year = as.numeric(year),
            trimestre = tolower(periodo),
            trimestre = dplyr::case_when(
                trimestre == "ene-mar"  ~ "T1",
                trimestre == "abr-jun"  ~ "T2",
                trimestre == "jul-sept" ~ "T3",
                trimestre == "oct-dic"  ~ "T4"
            ),
            periodo = paste(trimestre, year, sep = "-")) |>
        dplyr::select(periodo,
        produccion_igual_trimestre_ano_anterior:indice_de_clima_empresarial_ice) |>
    suppressMessages ()

   print("datos trimestral")
   return(opinion_empresarial_trimestral)
=======
    return(opinion_empresarial_mensual)
  }
  
  # archivo trimestral
  else if (!mensual){
    
    opinion_empresarial_trimestral <- readxl::read_excel(file_path,
                                                         skip = 4,
                                                         col_types = c('guess',
                                                                       rep('numeric', 19))) |>
      janitor::clean_names() |>
      dplyr::mutate(year = ifelse(stringr::str_detect(periodo, '^[0-9]+$'), periodo, NA)) |>
      tidyr::fill(year) |>
      dplyr::filter(!is.na(produccion_respecto_al_trimestre_anterior)) |>
      dplyr::mutate(
        year = as.numeric(year),
        trimestre = tolower(periodo),
        trimestre = dplyr::case_when(
          trimestre == "ene-mar"  ~ "T1",
          trimestre == "abr-jun"  ~ "T2",
          trimestre == "jul-sept" ~ "T3",
          trimestre == "oct-dic"  ~ "T4"
        ),
        periodo = paste(trimestre, year, sep = "-")) |>
      dplyr::select(periodo,
                    produccion_igual_trimestre_ano_anterior:indice_de_clima_empresarial_ice) |>
      suppressMessages ()
    
    
    print("datos trimestral")
    return(opinion_empresarial_trimestral)
    
>>>>>>> d9bc13d93983ee064765975b73266614999a7418
  }
}






















