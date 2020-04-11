# Ipc por articulos
url_articulos <- paste0("https://cdn.bancentral.gov.do/documents/estadisticas/",
                        "precios/documents/ipc_articulos_base_2010.xlsx?v=1586177873450")

temp_path <- tempfile(pattern = "", fileext = ".xlsx")

download.file(url_articulos, temp_path)

sheets <- readxl::excel_sheets(temp_path)

header_articulos <- c("descripcion", "ponderacion", "enero", "febrero", "marzo",
                      "abril", "mayo", "junio", "julio", "agosto", "septiembre",
                      "octubre", "noviembre", "Diciembre")

indice_articulo <- readxl::read_excel(
    temp_path,
    sheet = "IPC artículos 2020",
    skip = 5,
    col_names = FALSE
)

indice_articulo %>%
    setNames(., header_articulos[seq_along(.)]) %>%
    tidyr::gather(mes, indice, -descripcion, -ponderacion)

# PIB por sectores de origenes base 2007




