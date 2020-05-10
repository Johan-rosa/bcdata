
get_pib_gasto <- function(modalidad = "nominal", acumulado = FALSE, homogenea_91 = FALSE) {

    `%>%` <- magrittr::`%>%`

    if(homogenea_91 == FALSE) {
        file_path <- tempfile(pattern = "", fileext = ".xls")
        url <- paste0("https://cdn.bancentral.gov.do/documents/estadisticas/",
                      "sector-real/documents/pib_gasto_2007.xls?v=1588388825134")
    } else if(homogenea_91 == TRUE) {
        file_path <- tempfile(pattern = "", fileext = ".xlsx")
        url <- paste0(
            "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/",
            "documents/pib_gasto_retro.xlsx?v=1589039003514")
        }

    download.file(url, file_path, quiet = TRUE, mode = "wb")

    if (modalidad == "nominal" & acumulado == FALSE & homogenea_91 == FALSE) {

        suppressMessages(
            pib_gasto <- readxl::read_excel(
                path = file_path, sheet = "PIB$_Trim",
                skip = 9, n_max = 14,
                col_names = FALSE)
        )

        pib_header <- c(
            "partida",
            seq(as.Date("2007-01-01"), by = "quarter",
                length.out = (ncol(pib_gasto) - 1)))

        pib_gasto <- pib_gasto %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(x1)) %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -partida,
                names_to = "trimestre",
                values_to = "monto") %>%
            dplyr::mutate(
                trimestre = readr::parse_number(trimestre),
                fecha = lubridate::as_date(trimestre),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(partida, fecha, year,
                          trimestre, monto)

    } else if(modalidad == "nominal" & acumulado == TRUE & homogenea_91 == FALSE) {

        suppressMessages(
            pib_gasto <- readxl::read_excel(
                path = file_path, sheet = "PIB$_Trim_Acum",
                skip = 9, n_max = 14,
                col_names = FALSE)
        )

        pib_header <- c(
            "partida",
            seq(as.Date("2007-01-01"), by = "quarter",
                length.out = (ncol(pib_gasto) - 1)))

        pib_gasto <- pib_gasto %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(x1)) %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -partida,
                names_to = "trimestre",
                values_to = "monto") %>%
            dplyr::mutate(
                trimestre = readr::parse_number(trimestre),
                fecha = lubridate::as_date(trimestre),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(partida, fecha, year,
                          trimestre, monto)

    } else if(modalidad == "real" & acumulado == FALSE & homogenea_91 == FALSE) {

        suppressMessages(
            pib_gasto <- readxl::read_excel(
                path = file_path, sheet = "PIBK_Trim",
                skip = 9, n_max = 14,
                col_names = FALSE)
        )

        pib_header <- c(
            "partida",
            seq(as.Date("2007-01-01"), by = "quarter",
                length.out = (ncol(pib_gasto) - 1)))

        pib_gasto <- pib_gasto %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(x1)) %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -partida,
                names_to = "trimestre",
                values_to = "indice") %>%
            dplyr::mutate(
                trimestre = readr::parse_number(trimestre),
                fecha = lubridate::as_date(trimestre),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(partida, fecha, year,
                          trimestre, indice)

    } else if(modalidad == "real" & acumulado == TRUE & homogenea_91 == FALSE) {

        suppressMessages(
            pib_gasto <- readxl::read_excel(
                path = file_path, sheet = "PIBK_Trim_Acum",
                skip = 9, n_max = 14,
                col_names = FALSE)
        )

        pib_header <- c(
            "partida",
            seq(as.Date("2007-01-01"), by = "quarter",
                length.out = (ncol(pib_gasto) - 1)))

        pib_gasto <- pib_gasto %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(x1)) %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -partida,
                names_to = "trimestre",
                values_to = "indice") %>%
            dplyr::mutate(
                trimestre = readr::parse_number(trimestre),
                fecha = lubridate::as_date(trimestre),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(partida, fecha, year,
                          trimestre, indice)
    } else if (modalidad == "nominal" & acumulado == FALSE & homogenea_91 == TRUE) {

        suppressMessages(
            pib_gasto <- readxl::read_excel(
                path = file_path,
                sheet = "PIB$_Trim",
                skip = 9, n_max = 14,
                col_names = FALSE)
        )

        pib_header <- c(
            "partida",
            seq(as.Date("1991-01-01"), by = "quarter",
                length.out = (ncol(pib_gasto) - 1)))

        pib_gasto <- pib_gasto %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(x1)) %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -partida,
                names_to = "trimestre",
                values_to = "monto") %>%
            dplyr::mutate(
                trimestre = readr::parse_number(trimestre),
                fecha = lubridate::as_date(trimestre),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(partida, fecha, year,
                          trimestre, monto)
    } else if (modalidad == "nominal" & acumulado == TRUE & homogenea_91 == TRUE) {

        suppressMessages(
            pib_gasto <- readxl::read_excel(
                path = file_path,
                sheet = "PIB$_Trim_Acum",
                skip = 9, n_max = 14,
                col_names = FALSE)
        )

        pib_header <- c(
            "partida",
            seq(as.Date("1991-01-01"), by = "quarter",
                length.out = (ncol(pib_gasto) - 1)))

        pib_gasto <- pib_gasto %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(x1)) %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -partida,
                names_to = "trimestre",
                values_to = "monto") %>%
            dplyr::mutate(
                trimestre = readr::parse_number(trimestre),
                fecha = lubridate::as_date(trimestre),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(partida, fecha, year,
                          trimestre, monto)

    } else if (modalidad == "real" & acumulado == FALSE & homogenea_91 == TRUE) {

        suppressMessages(
            pib_gasto <- readxl::read_excel(
                path = file_path,
                sheet = "PIBK_Trim",
                skip = 9, n_max = 14,
                col_names = FALSE)
        )

        pib_header <- c(
            "partida",
            seq(as.Date("1991-01-01"), by = "quarter",
                length.out = (ncol(pib_gasto) - 1)))

        pib_gasto <- pib_gasto %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(x1)) %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -partida,
                names_to = "trimestre",
                values_to = "monto") %>%
            dplyr::mutate(
                trimestre = readr::parse_number(trimestre),
                fecha = lubridate::as_date(trimestre),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(partida, fecha, year,
                          trimestre, monto)
    } else if(modalidad == "real" & acumulado == TRUE & homogenea_91 == TRUE) {

        suppressMessages(
            pib_gasto <- readxl::read_excel(
                path = file_path,
                sheet = "PIBK_Trim_Acum",
                skip = 9, n_max = 14,
                col_names = FALSE)
        )

        pib_header <- c(
            "partida",
            seq(as.Date("1991-01-01"), by = "quarter",
                length.out = (ncol(pib_gasto) - 1)))

        pib_gasto <- pib_gasto %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(x1)) %>%
            setNames(pib_header) %>%
            tidyr::pivot_longer(
                cols = -partida,
                names_to = "trimestre",
                values_to = "monto") %>%
            dplyr::mutate(
                trimestre = readr::parse_number(trimestre),
                fecha = lubridate::as_date(trimestre),
                year = lubridate::year(fecha),
                trimestre = lubridate::quarter(fecha)
            ) %>%
            dplyr::select(partida, fecha, year,
                          trimestre, monto)
    }

    return(pib_gasto)


}


