
get_imae <- function(variaciones = TRUE) {

# Asignando el pipe para usarlo sin cargar dplyr
`%>%` <- magrittr::`%>%`

# url de descarga
url <- paste0("https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/imae.xlsx?v=1588451379591")

# path temporal
temp_path <- tempfile(pattern = "", fileext = ".xlsx")

# descargando el archivo
download.file(url, temp_path, quiet = TRUE, mode = "wb")

# headers
header_imae <- c(
    "mes",
    "indice_original", "original_vi", "origianl_va", "original_p12m",
    "indice_desetacionalizado", "desestacionalizado_vm", "desetacionalizado_vi", "desestacionalizado_va",
    "desestacionalizado_p12m", "indice_tc", "tc_vm", "tc_vi", "tc_va", "tc_p12m")

# Importando la data
suppressMessages(
    imae <- readxl::read_excel(
        path = temp_path,
        skip = 9,
        col_names = FALSE

    )
)

# limpiando el archivo
imae <- imae %>%
    janitor::clean_names() %>%
    dplyr::select(-x1) %>%
    dplyr::filter(!is.na(x2)) %>%
    setNames(header_imae) %>%
    dplyr::mutate(
        fecha = seq(as.Date("2007-01-01"), by="month", length.out = nrow(.)),
        year = lubridate::year(fecha)
    ) %>%
    dplyr::select(fecha, year, mes, dplyr::everything())


if(variaciones == FALSE) {
    imae <- imae %>%
        dplyr::select(fecha, year, mes, dplyr::contains("indice"))
}

return(imae)

}
