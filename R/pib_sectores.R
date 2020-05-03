

get_pib_sectores <- function(modalidad = "real") {
    `%>%` <- magrittr::`%>%`
    # Eventualmente esto podría se run argumento para
    # descargar también las series desde el 91
    base = 2007

    if(base == 2007){
        # Url de descarga
        url <- paste0(
            "https://cdn.bancentral.gov.do/documents/",
            "estadisticas/sector-real/documents/",
            "pib_origen_2007.xlsx?v=1586996101374"
        ) } else {
            url <- paste0(
                "https://cdn.bancentral.gov.do/documents/",
                "estadisticas/sector-real/documents/",
                "PIB_sectores_origen.xls?v=1587419018655"
            )
        }


    # Ruta temporal del archivo
    path <- tempfile(pattern = "pib", fileext = ".xlsx")
    # descarga del archivo
    download.file(url, path, quiet = TRUE, mode = "wb")

    if(modalidad == "real" & base == 2007) {

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

    } else if (modalidad == "nominal" & base == 2007) {

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

       # return(pib_sectores)

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
