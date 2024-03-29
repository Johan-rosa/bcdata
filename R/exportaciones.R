#' Descarga las exportaciones dominicanas
#'
#' Esta funcion descarga los archivos con las exportaciones
#' dominicanas y los combina para terminar con una serie actualizada
#' y desagregada por producto, tipo de producto y regimen de exportación.
#' El valor de las exportaciones está en millones de RD$.
#'
#' @return Un tibble con la serie de las exportaciones
#' @usage get_exportaciones()

get_exportaciones <- function(frecuencia = 'mensual') {

    `%>%` <- magrittr::`%>%`

    header_exportaciones <- c(
      "x_minerales", "x_oro", "x_ferroniquel", "x_cobre", "x_plata",
      "x_bauxita", "x_piedra_caliza", "x_zinc", "x_otros minerales",
      "x_agro", "x_agro_nacionales", "x_agro_nacionales_guineo", "x_agro_nacionales_cacao",
      "x_agro_nacionales_aguacate", "x_agro_nacionales_ajies", "x_agro_nacionales_coco",
      "x_agro_nacionales_cafe", "x_agro_nacionales_batata", "x_agro_nacionales_platano",
      "x_agro_nacionales_tabaco", "x_agro_nacionales_otros", "x_agro_zf",
      "x_agro_zf_cacao", "x_agro_zf_otros", "x_ind", "x_ind_nac", "x_ind_nac_azucar",
      "x_ind_nac_quimicos", "x_ind_nac_cemento", "x_ind_nac_varillas",
      "x_ind_nac_ron", "x_ind_nac_plasticos", "x_ind_nac_harina", "x_ind_nac_condimentos",
      "x_ind_nac_galletas", "x_ind_nac_cervezas", "x_ind_nac_pastas",
      "x_ind_nac_cajas_carton", "x_ind_nac_frutas_procesadas", "x_ind_nac_tabaco_manufacturado",
      "x_ind_nac_cafe_manufacturado", "x_ind_nac_cacao_manufacturado",
      "x_ind_nac_aceite_soya", "x_ind_nac_combustibles_petroleo", "x_ind_nac_alimentos_aeronaves",
      "x_ind_nac_combustibles_aeronaves", "x_ind_nac_otros", "x_ind_zf",
      "x_ind_zf_textil", "x_ind_zf_productos_electricos", "x_ind_zf_joyeria",
      "x_ind_zf_farmaceuticos", "x_ind_zf_equipos_medicos", "x_ind_zf_calzado_manufacturado",
      "x_ind_zf_tabaco_manufacturado", "x_ind_zf_cacao_manufacturado",
      "x_ind_zf_alimentos_aeronaves", "x_ind_zf_otros", "x_total",
      "x_nac", "x_zf", "x_bienes_puerto", "x_combustibles_aeronaves", "x_alimentos_aeronaves")

    periodo <- c(2010:lubridate::year(Sys.Date()))

    # url de descargas
    url_descarga <- paste0(
        "https://cdn.bancentral.gov.do/documents/estadisticas/",
        "sector-externo/documents/Exportaciones_Mensuales_",
        periodo,"_6.xls?v=1571253704905"
    )

    # Archivos temporales
    files_path <- purrr::map_chr(
        periodo,
        ~tempfile(pattern = "", fileext = ".xls")
    )

    download <- purrr::possibly(download.file, otherwise = -1)

    suppressWarnings(
      # descargar archivos
      descarga <- purrr::map2(
        url_descarga,
        files_path,
        ~download(.x, .y, mode = "wb", quiet = TRUE))
    )

    files_path <- files_path[as.logical(unlist(descarga) + 1)]

    suppressMessages(
      suppressWarnings(
        # importar archivos
        exportaciones <- purrr::map(
            files_path,
            readxl::read_excel, sheet = 1,
            col_names = TRUE, skip = 8, na = "n.d.",
            n_max = 70) %>%
            setNames(periodo[as.logical(unlist(descarga) + 1)])
      )
    )

    exportaciones1 <- exportaciones %>%
        purrr::map(
            ~.x %>%
                janitor::clean_names() %>%
                dplyr::slice(-1) %>%
                #setNames(header_exportaciones) %>%
                dplyr::select(-x1, -dplyr::last_col()) %>%
                dplyr::rename(detalle1 = x2) %>%
                tidyr::drop_na(detalle1) %>%
                dplyr::mutate(detalle = header_exportaciones) %>%
                tidyr::gather('mes', "valor_expor", -detalle, -detalle1)
        ) %>%
        dplyr::bind_rows(.id = "year") %>%
        dplyr::select(detalle1, detalle, year, mes, dplyr::everything()) %>%
        dplyr::mutate(
            mes1 = dplyr::recode(mes,
                                 'ene' = '01',
                                 'feb' = '02',
                                 'mar' = '03',
                                 'abr' = '04',
                                 'may' = '05',
                                 'jun' = '06',
                                 'jul' = '07',
                                 'ago' = '08',
                                 'sep' = '09',
                                 'oct' = '10',
                                 'nov' = '11',
                                 'dic' = '12'),
            fecha = as.Date(paste(year, mes1, '1', sep = '-')),
            trimestre = lubridate::quarter(fecha, with_year = TRUE),
            grupo = stringr::str_extract(detalle, "agro|minerales|ind|bienes_puerto"),
            regimen = stringr::str_extract(detalle, "nac|zf"),
            regimen = ifelse(is.na(regimen), "nac", regimen)) %>%
        dplyr::select(-mes, -mes1) %>% 
        tidyr::fill(grupo) %>%
        dplyr::filter(!detalle %in% c("x_total", "x_nac", "x_zf", "x_minerales", "x_agro",
                               "x_agro_nacionales", "x_agro_zf", "x_ind", "x_ind_nac",
                               "x_ind_zf", "x_bienes_puerto")) %>%
        dplyr::mutate(
            detalle = stringr::str_remove(
                detalle,
                "x_ind_nac_|x_ind_zf_|x_agro_zf_|x_agro_nacionales_|x_")
            ) %>%
        dplyr::mutate(
            grupo = dplyr::recode(grupo,
                                  "agro" = "Agropecuario",
                                  "ind" = "Industrial",
                                  "bienes_puerto" = "Bienes en puerto",
                                  "minerales" = 'Minerales'),
            regimen = dplyr::recode(regimen,
                                    "nac" = "Nacional",
                                    "zf" = "Zonas francas")
            ) %>%
      dplyr::select(-detalle) %>%
      dplyr::rename(detalle = detalle1)

    if(frecuencia == 'trimestral'){
      exportaciones <- exportaciones1 %>% 
        dplyr::select(-fecha) %>% 
        dplyr::group_by(year, regimen, grupo, trimestre, detalle) %>% 
        dplyr::summarize(valor_expor = sum(valor_expor)) %>% 
        suppressMessages()
    } else if(frecuencia == 'anual'){
      exportaciones <- exportaciones1 %>% 
        dplyr::select(-fecha, - trimestre) %>% 
        dplyr::group_by(year, regimen, grupo, detalle) %>% 
        dplyr::summarize(valor_expor = sum(valor_expor)) %>% 
        suppressMessages()
    } else {
      exportaciones <- exportaciones1
    }
    
        return(exportaciones)

}
