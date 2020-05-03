#' Descarga las importaciones dominicanas
#'
#' Esta funcion descarga los archivos con las importaciones
#' dominicanas y los combina para terminar con una serie actualizada
#' y desagregada por producto, tipo de producto y regimen de importacion
#'
#' @return Un tibble con la serie de las importaciones
#' @usage get_exportaciones()

get_importaciones <- function() {
    # Para usar el pipe sin cargar dplyr o magrittr
    `%>%` <- magrittr::`%>%`
    # años potenciales
    periodo <- c(2010:lubridate::year(Sys.Date()))
    # Url de descarga para cada anio
    url_descarga <- paste0("https://cdn.bancentral.gov.do/documents/estadisticas/",
                           "sector-externo/documents/Importaciones_Mensuales_",
                           periodo,
                           "_6.xls?v=1571324085432")
    # Creando una ruta temporal para ubicar cada archivo
    files_paths <- purrr::map_chr(
        periodo,
        ~tempfile(pattern = "", fileext = ".xls"))

    # Creando una versión segura de la funcion de descaga
    download <- purrr::possibly(download.file, otherwise = -1)

    # Descargando archivos
    descarga <- purrr::map2(url_descarga, files_paths, ~download(.x, .y, mode = "wb"))

    # Filtrando los resultado que tuvieron exito
    files_paths <- files_paths[as.logical(unlist(descarga) + 1)]

    header_importaciones <- c(
        "m_consumo", "m_consumo_duradero", "m_consumo_partes", "m_consumo_herramientas",
        "m_consumo_repuestos_vehiculos", "m_consumo_estufas", "m_consumo_alimentos_elaborados",
        "m_consumo_leche", "m_consumo_arroz", "m_consumo_azucar", "m_consumo_farmaceuticos",
        "m_consumo_combustibles", "m_consumo_combustibles_otros", "m_consumo_otros",

        "m_materias_primas_", "m_materias_primas_nac_", "m_materias_primas_nac_agricultura",
        "m_materias_primas_nac_alimentos_noelaborado", "m_materias_primas_nac_aceites",
        "m_materias_primas_nac_maiz", "m_materias_primas_nac_azucar",
        "m_materias_primas_nac_madera", "m_materias_primas_nac_textil",
        "m_materias_primas_nac_envases", "m_materias_primas_nac_bebidas",
        "m_materias_primas_nac_tabaco_noelaborado", "m_materias_primas_nac_trigo",
        "m_materias_primas_nac_petroleo_crudo", "m_materias_primas_nac_combustibles_otros_noelaborado",
        "m_materias_primas_nac_carbon", "m_materias_primas_nac_grasas_aceites_otros",
        "m_materias_primas_nac_quimicos_inorganicos", "m_materias_primas_nac_quimicos_organicos",
        "m_materias_primas_nac_plastico_artificial", "m_materias_primas_nac_papel",
        "m_materias_primas_nac_hierro", "m_materias_primas_nac_otros",

        "m_materias_primas_zf", "m_materias_primas_zf_materias_primas",
        "m_materias_primas_zf_comercializadoras",

        "m_capital_", "m_capital_nac_",
        "m_capital_nac_agricultura", "m_capital_nac_construccion", "m_capital_nac_transporte",
        "m_capital_nac_industria", "m_capital_nac_repuestos_maquinaria",
        "m_capital_nac_otros",

        "m_capital_zf", "m_total", "m_nacionales",
        "m_zf", "m_petroleras", "m_no_petroleras")

    # Importando los archivos
    importaciones <-
        purrr::map(files_paths,
                   readxl::read_excel,
                   sheet = 1,
                   col_names = TRUE,
                   skip = 7,
                   na = "n.d.",
                   n_max = 60)

    # Adecuar achivos
    importaciones <- importaciones %>%
        purrr::map(
            ~.x %>%
                janitor::clean_names() %>%
                dplyr::slice(-1) %>%
                #setNames(header_exportaciones) %>%
                dplyr::select(-x1, -dplyr::last_col()) %>%
                dplyr::rename(detalle1 = x2) %>%
                tidyr::drop_na() %>%
                dplyr::mutate(detalle = header_importaciones) %>%
                tidyr::gather('mes', "valor_expor", -detalle, -detalle1)
        ) %>%
        setNames(periodo[as.logical(unlist(descarga) + 1)]) %>%
        dplyr::bind_rows(.id = "year") %>%
        dplyr::select(detalle, detalle1, year, mes, dplyr::everything()) %>%
        dplyr::mutate(
            grupo = stringr::str_extract(detalle, "consumo|materias_primas|capital"),
            regimen = stringr::str_extract(detalle, "nac|zf"),
            regimen = ifelse(is.na(regimen), "nac", regimen)) %>%
        tidyr::fill(grupo) %>%
        dplyr::filter(!detalle %in% c("m_consumo", "m_materias_primas", "m_materias_primas_nac_",
                                      "m_materias_primas_zf", "m_capital_", "m_capital_nac_",
                                      "m_capital_zf", "m_total", "m_nacionales", "m_zf", "m_petroleras",
                                      "m_no_petroleras")) %>%
        dplyr::mutate(
            detalle = stringr::str_remove(
                detalle,
                "m_consumo_|m_materias_primas_nac_|m_materias_primas_zf_|m_capital_nac_|m_capital_zf_|m_")
        ) %>%
        dplyr::mutate(
            grupo = dplyr::recode(grupo,
                                  "consumo" = "Bienes de consumo",
                                  "ind" = "Materias primas",
                                  "bienes_puerto" = "Bienes de capital"),
            regimen = dplyr::recode(regimen,
                                    "nac" = "Nacional",
                                    "zf" = "Zonas francas")
        ) %>%
        dplyr::select(-detalle) %>%
        dplyr::rename(detalle = detalle1)

    return(importaciones)

}