#' Descarga las exportaciones dominicanas
#'
#' Esta funcion descarga los archivos con las exportaciones
#' dominicanas y los combina para terminar con una serie actualizada
#' y desagregada por producto, tipo de producto y regimen de exportacion
#'
#' @return Un tibble con la serie de las exportaciones
#' @usage get_exportaciones()

get_exportaciones <- function() {

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

    # descargar archivos
    descarga <- purrr::map2(url_descarga, files_path, ~download(.x, .y, mode = "wb"))

    files_path <- files_path[as.logical(unlist(descarga) + 1)]

    # importar archivos
    exportaciones <- purrr::map(
        files_path,
        readxl::read_excel, sheet = 1,
        col_names = TRUE, skip = 7, na = "n.d.",
        n_max = 70) %>%
        setNames(periodo[as.logical(unlist(descarga) + 1)])

    exportaciones <- exportaciones %>%
        purrr::map(
            ~.x %>%
                janitor::clean_names() %>%
                dplyr::slice(-1) %>%
                #setNames(header_exportaciones) %>%
                dplyr::select(-x1, -dplyr::last_col()) %>%
                dplyr::rename(detalle1 = x2) %>%
                tidyr::drop_na() %>%
                dplyr::mutate(detalle = header_exportaciones) %>%
                tidyr::gather('mes', "valor_expor", -detalle, -detalle1)
        ) %>%
        dplyr::bind_rows(.id = "year") %>%
        dplyr::select(detalle1, detalle, year, mes, dplyr::everything()) %>%
        dplyr::mutate(
            grupo = stringr::str_extract(detalle, "agro|minerales|ind|bienes_puerto"),
            regimen = stringr::str_extract(detalle, "nac|zf"),
            regimen = ifelse(is.na(regimen), "nac", regimen)) %>%
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

        return(exportaciones)

}


#' Descarga las importaciones dominicanas
#'
#' Esta funcion descarga los archivos con las importaciones
#' dominicanas y los combina para terminar con una serie actualizada
#' y desagregada por producto, tipo de producto y regimen de importacion
#'
#' @return Un tibble con la serie de las importaciones
#' @usage get_exportaciones()

get_impotaciones <- function() {
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









