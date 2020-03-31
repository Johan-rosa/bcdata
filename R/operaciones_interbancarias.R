get_op_interbancaria <- function() {
     # Url del archivo en la página del Banco Central
     web_url <-  paste0("https://cdn.bancentral.gov.do/documents/estadisticas/",
                       "sector-monetario-y-financiero/documents/",
                       "operaciones_interb.xls?v=1575577215314")
     # Directorio en el que se va a guardar temporalmente
     file_path <- tempfile(pattern = "", fileext = ".xls")

     # Descarga del archivo
     download.file(web_url, file_path)

     # Importando el archivo
     op_interbancaria <- xlsx::read.xlsx(
         file_path,
         sheetName = "Interbancarios",
         startRow = 29,
         startCol = 2,
         header = FALSE)

     op_interbancaria <-
         op_interbancaria %>%
         # selecciona las columnas de interes
         dplyr::select(X2, X3, X4) %>%
         # excluye filas en blanco
         tidyr::drop_na() %>%
         dplyr::filter(stringr::str_detect(X2, "^[A-Z]")) %>%

         # agrega nombres últiles a las columnas
         purrr::set_names(nm = c("mes", "op_interb", "tasa_interb")) %>%
         # le da la clase correcta a la columna mes
         dplyr::mutate(mes = as.character(mes)) %>%
         dplyr::mutate(
             fecha = seq(
                 ymd("2000/01/01"),
                 length.out = length(mes),
                 by = "month"),
             year = year(fecha)
         ) %>%
         # excluye todas las columnas que no
        dplyr::select(fecha, year, mes, everything())

     return(op_interbancaria)
}
