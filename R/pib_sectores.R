#' Descarga la series del PIB por sectores de origen
#'
#' Descarga las series del PIB por sectores de origen,
#' con diferentes alternativas según las metodolog\\u00edas
#' y opciones de presentaci\\u00f3n de resultados.
#'
#' @param modalidad String indicando la modalidad deseada
#' de los datos, si precios corrientes o \\u00edndicesndices de volumen
#' encadenados. Opciones: "nominal" y "real"
#'
#' @param acumulado Logical indicando si se desea serie trimestral
#' o trimestral acumulada al año.
#'
#' @param homogenea_91 Logical indicando si se desea la serie homogenea
#' desde el año 1991, con base 2007.
#'
#' @return Un tibble con la serie del PIB desagregada por sectores de origen.
#'
#' @examples
#' # PIB nominal por sectores de origen, serie trimestral desde 1991, base 2007
#' get_pib_sectores(modalidad = "nominal", acumulado = FALSE, homogenea_91 = TRUE)
#' # PIB real por sectores de origen, serie trimestral desde 1991, base 2007
#' get_pib_sectores(modalidad = "nominal", acumulado = FALSE, homogenea_91 = TRUE)
#'

get_pib_sectores <- function(modalidad = "real", acumulado = FALSE, homogenea_91 = FALSE) {
    `%>%` <- magrittr::`%>%`


    if(homogenea_91 == FALSE){
        # Url de descarga
        url <- paste0(
            "https://cdn.bancentral.gov.do/documents/",
            "estadisticas/sector-real/documents/",
            "pib_origen_2007.xlsx?v=1586996101374"
        ) } else {
            url <- paste0(
                "https://cdn.bancentral.gov.do/documents/",
                "estadisticas/sector-real/documents/",
                "pib_origen_retro.xlsx?v=1588471346723"
            )
        }


    # Ruta temporal del archivo
    path <- tempfile(pattern = "pib", fileext = ".xlsx")
    # descarga del archivo
    download.file(url, path, quiet = TRUE, mode = "wb")


    if(modalidad == "real" & homogenea_91 == FALSE & acumulado == FALSE) {
    # PIB real trimestral -----------------------------------------------------
        suppressMessages(
            pib_sectores <- readxl::read_excel(
                path = path,
                sheet = "PIBK_Trim",
                skip = 9,
                n_max = 31,
                col_names = FALSE)
            )

        suppressMessages(
            pib_incidencia <- readxl::read_excel(
                path = path,
                sheet = "PIBK_Trim",
                skip = 81,
                n_max = 31,
                col_names = FALSE)
            )


        pib_header <- c(
            "sector",
            seq(as.Date("2007-01-01"), by = "quarter",
                length.out = (ncol(pib_sectores) - 1)))

        pib_sectores <- pib_sectores %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "pib_2007"
            )

        pib_incidencia <- pib_incidencia %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "incidencia"
            )

        pib_sectores <-
            dplyr::left_join(pib_sectores, pib_incidencia) %>%
            dplyr::mutate(
                fecha = as.numeric(trimestre),
                fecha = lubridate::as_date(fecha),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(fecha, year, trimestre, dplyr::everything())

        #return(pib_sectores)

    } else if (modalidad == "nominal" & homogenea_91 == FALSE & acumulado == FALSE) {
    # PIB nominal trimestral -----------------------------------------------------
        suppressMessages(
            pib_sectores <- readxl::read_excel(
                path = path,
                sheet = "PIB$_Trim",
                skip = 9,
                n_max = 31,
                col_names = FALSE)
            )

        suppressMessages(
            pib_pond <- readxl::read_excel(
                path = path,
                sheet = "PIB$_Trim",
                skip = 45,
                n_max = 31,
                col_names = FALSE)
        )

        pib_header <- c(
            "sector",
            seq(as.Date("2007-01-01"), by = "quarter",
                length.out = (ncol(pib_sectores) - 1)))

        pib_sectores <- pib_sectores %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "pib_2007"
            )

        pib_pond <- pib_pond %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "ponderacion"
            )

        pib_sectores <- dplyr::left_join(pib_sectores, pib_pond) %>%
            dplyr::mutate(
                fecha = as.numeric(trimestre),
                fecha = lubridate::as_date(fecha),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(fecha, year, trimestre, dplyr::everything())

    } else if (modalidad == "nominal" & homogenea_91 == FALSE & acumulado == TRUE) {
    # PIB nominal acumulado ---------------------------------------------------
        suppressMessages(
            pib_sectores <- readxl::read_excel(
                path = path,
                sheet = "PIB$_Trim_Acum",
                skip = 9,
                n_max = 31,
                col_names = FALSE)
        )

        suppressMessages(
            pib_pond <- readxl::read_excel(
                path = path,
                sheet = "PIB$_Trim_Acum",
                skip = 45,
                n_max = 31,
                col_names = FALSE)
        )

        pib_header <- c(
            "sector",
            seq(as.Date("2007-01-01"), by = "quarter",
                length.out = (ncol(pib_sectores) - 1)))

        pib_sectores <- pib_sectores %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "pib_2007"
            )

        pib_pond <- pib_pond %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "ponderacion"
            )

        pib_sectores <- dplyr::left_join(pib_sectores, pib_pond) %>%
            dplyr::mutate(
                fecha = as.numeric(trimestre),
                fecha = lubridate::as_date(fecha),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(fecha, year, trimestre, dplyr::everything())

        # return(pib_sectores)
    } else if(modalidad == "real" & homogenea_91 == FALSE & acumulado == TRUE) {
    # PIB real acumulado ------------------------------------------------------
        suppressMessages(
            pib_sectores <- readxl::read_excel(
                path = path,
                sheet = "PIBK_Trim_Acum",
                skip = 9,
                n_max = 31,
                col_names = FALSE)
        )

        suppressMessages(
            pib_incidencia <- readxl::read_excel(
                path = path,
                sheet = "PIBK_Trim_Acum",
                skip = 81,
                n_max = 31,
                col_names = FALSE)
        )


        pib_header <- c(
            "sector",
            seq(as.Date("2007-01-01"), by = "quarter",
                length.out = (ncol(pib_sectores) - 1)))

        pib_sectores <- pib_sectores %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "pib_2007"
            )

        pib_incidencia <- pib_incidencia %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "incidencia"
            )

        pib_sectores <-
            dplyr::left_join(pib_sectores, pib_incidencia) %>%
            dplyr::mutate(
                fecha = as.numeric(trimestre),
                fecha = lubridate::as_date(fecha),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(fecha, year, trimestre, dplyr::everything())

        #return(pib_sectores)

    } else if(modalidad == "real" & homogenea_91 == TRUE & acumulado == FALSE) {
    # PIB real trimestral serie homogenea -------------------------------------
        suppressMessages(
            pib_sectores <- readxl::read_excel(
                path = path,
                sheet = "PIBK_Trim",
                skip = 9,
                n_max = 21,
                col_names = FALSE)
        )

        suppressMessages(
            pib_incidencia <- readxl::read_excel(
                path = path,
                sheet = "PIBK_Trim",
                skip = 61,
                n_max = 21,
                col_names = FALSE)
        )


        pib_header <- c(
            "sector",
            seq(as.Date("1991-01-01"), by = "quarter",
                length.out = (ncol(pib_sectores) - 1)))

        pib_sectores <- pib_sectores %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "pib_2007"
            )

        pib_incidencia <- pib_incidencia %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "incidencia"
            )

        pib_sectores <-
            dplyr::left_join(pib_sectores, pib_incidencia) %>%
            dplyr::mutate(
                fecha = as.numeric(trimestre),
                fecha = lubridate::as_date(fecha),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(fecha, year, trimestre, dplyr::everything())

        #return(pib_sectores)
    } else if (modalidad == "nominal" & homogenea_91 == TRUE & acumulado == FALSE) {
    # PIB nominal trimestral serie homogenea ----------------------------------
        suppressMessages(
            pib_sectores <- readxl::read_excel(
                path = path,
                sheet = "PIB$_Trim",
                skip = 9,
                n_max = 21,
                col_names = FALSE)
        )

        suppressMessages(
            pib_pond <- readxl::read_excel(
                path = path,
                sheet = "PIB$_Trim",
                skip = 35,
                n_max = 21,
                col_names = FALSE)
        )

        pib_header <- c(
            "sector",
            seq(as.Date("1991-01-01"), by = "quarter",
                length.out = (ncol(pib_sectores) - 1)))

        pib_sectores <- pib_sectores %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "pib_2007"
            )

        pib_pond <- pib_pond %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "ponderacion"
            )

        pib_sectores <- dplyr::left_join(pib_sectores, pib_pond) %>%
            dplyr::mutate(
                fecha = as.numeric(trimestre),
                fecha = lubridate::as_date(fecha),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(fecha, year, trimestre, dplyr::everything())

    }  else if (modalidad == "nominal" & homogenea_91 == TRUE & acumulado == TRUE) {
    # PIB nominal acumulado serie homogenea -----------------------------------
        suppressMessages(
            pib_sectores <- readxl::read_excel(
                path = path,
                sheet = "PIB$_Trim Acum",
                skip = 9,
                n_max = 21,
                col_names = FALSE)
        )

        suppressMessages(
            pib_pond <- readxl::read_excel(
                path = path,
                sheet = "PIB$_Trim Acum",
                skip = 35,
                n_max = 21,
                col_names = FALSE)
        )

        pib_header <- c(
            "sector",
            seq(as.Date("1991-01-01"), by = "quarter",
                length.out = (ncol(pib_sectores) - 1)))

        pib_sectores <- pib_sectores %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "pib_2007"
            )

        pib_pond <- pib_pond %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "ponderacion"
            )

        pib_sectores <- dplyr::left_join(pib_sectores, pib_pond) %>%
            dplyr::mutate(
                fecha = as.numeric(trimestre),
                fecha = lubridate::as_date(fecha),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(fecha, year, trimestre, dplyr::everything())

    } else if(modalidad == "real" & homogenea_91 == TRUE & acumulado == TRUE) {
    # PIB real acumulado serie homogenea --------------------------------------
        suppressMessages(
            pib_sectores <- readxl::read_excel(
                path = path,
                sheet = "PIBK_Trim Acum",
                skip = 9,
                n_max = 21,
                col_names = FALSE)
        )

        suppressMessages(
            pib_incidencia <- readxl::read_excel(
                path = path,
                sheet = "PIBK_Trim Acum",
                skip = 61,
                n_max = 21,
                col_names = FALSE)
        )


        pib_header <- c(
            "sector",
            seq(as.Date("1991-01-01"), by = "quarter",
                length.out = (ncol(pib_sectores) - 1)))

        pib_sectores <- pib_sectores %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "pib_2007"
            )

        pib_incidencia <- pib_incidencia %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -sector,
                names_to = "trimestre",
                values_to = "incidencia"
            )

        pib_sectores <-
            dplyr::left_join(pib_sectores, pib_incidencia) %>%
            dplyr::mutate(
                fecha = as.numeric(trimestre),
                fecha = lubridate::as_date(fecha),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(fecha, year, trimestre, dplyr::everything())

    }


    detalle_sectores <- structure(
        list(sector = c(
            "Agropecuario", "Subsector Agricola","Ganadería, Silvicultura y Pesca",
            "Industrias", "Explotación de Minas y Canteras",
            "Manufactura Local", "Industrias de Alimentos", "Elaboración de Bebidas y Productos de Tabaco",
            "Fabricación de Productos de la Refinación de Petróleo y Quimicos",
            "Otras Manufacturas", "Manufactura Zonas Francas", "Construcción",
            "Servicios", "Energía y Agua", "Comercio", "Hoteles, Bares y Restaurantes",
            "Transporte y Almacenamiento", "Comunicaciones", "Intermediación Financiera, Seguros y Actividades Conexas",
            "Actividades Inmobiliarias y de Alquiler", "Enseñanza", "Enseñanza de Mercado",
            "Enseñanza No de Mercado", "Salud", "Salud de Mercado", "Salud No de Mercado",
            "Otras Actividades de Servicios de Mercado", "Administración Pública y Defensa; Seguridad Social de Afiliación Obligatoria y Otros Servicios",
            "Valor Agregado", "Impuestos a la producción netos de subsidios",
            "Producto Interno Bruto"),
            agregacion = c(
                "sector", "subsector",
                "subsector", "sector", "subsector", "subsector", "actividad",
                "actividad", "actividad", "actividad", "subsector", "subsector",
                "sector", "subsector", "subsector", "subsector", "subsector",
                "subsector", "subsector", "subsector", "subsector", "actividad",
                "actividad", "subsector", "actividad", "actividad", "subsector",
                "subsector", "valor agregado", "impuestos", "pib"),
            nombre_sector = c(
                "Agropecuario",
                "Agropecuario", "Agropecuario", "Industrias", "Industrias", "Industrias",
                "Industrias", "Industrias", "Industrias", "Industrias", "Industrias",
                "Industrias", "Servicios", "Servicios", "Servicios", "Servicios",
                "Servicios", "Servicios", "Servicios", "Servicios", "Servicios",
                "Servicios", "Servicios", "Servicios", "Servicios", "Servicios",
                "Servicios", "Servicios", "valor agregado", "impuestos", "pib"),
            nombre_subsector = c(
                "Agropecuario", "Subsector Agricola",
                "Ganadería, Silvicultura y Pesca", "Industrias", "Explotación de Minas y Canteras",
                "Manufactura Local", "Manufactura Local", "Manufactura Local",
                "Manufactura Local", "Manufactura Local", "Manufactura Zonas Francas",
                "Construcción", "Servicios", "Energía y Agua", "Comercio", "Hoteles, Bares y Restaurantes",
                "Transporte y Almacenamiento", "Comunicaciones", "Intermediación Financiera, Seguros y Actividades Conexas",
                "Actividades Inmobiliarias y de Alquiler", "Enseñanza", "Enseñanza",
                "Enseñanza", "Salud", "Salud", "Salud", "Otras Actividades de Servicios de Mercado",
                "Administración Pública y Defensa; Seguridad Social de Afiliación Obligatoria y Otros Servicios",
                "valor agregado", "impuestos", "pib")
        ),
        row.names = c(NA, -31L),
        class = c("tbl_df", "tbl", "data.frame")
    )

    pib_sectores <- dplyr::left_join(pib_sectores, detalle_sectores)
    return(pib_sectores)

}
