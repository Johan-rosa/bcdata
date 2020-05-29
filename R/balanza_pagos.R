# # Librerías ---------------------------------------------------------------
#
# library(easypackages)
# libraries("readxl","stringi","stringr","tidyverse","janitor","lubridate",
#           "xlsx", "tm", "data.table", "httr")
#
#
#
# # URL y descargando archivo -----------------------------------------------
#
# url <- paste0("https://cdn.bancentral.gov.do/documents/",
#               "estadisticas/sector-externo/documents/",
#               "bpagos__trim_6.xls?v=1571246008106")
#
#
#
#
# download.file(URL_BALANZA_PAGOS, )
#
# INTERNAL_URL_BALANZA_PAGOS <- "insumos_excel/bpagos__trim_6.xls"
#
# # Objetos utilitarios -----------------------------------------------------
#
# header_balanza_pagos <- c(
#     "cuenta_corriente", "balanza_bys", "balanza_bienes",
#     "balanza_export", "balanza_export_nac", "balanza_export_zf",
#     "balanza_import", "balanza_import_nac", "balanza_import_zf",
#     "balanza_servicios", "balanza_credito", "balanza_credito_viajes",
#     "balanza_credito_manufactura", "balanza_credito_otros", "balanza_debito",
#     "balanza_debito_fletes", "balanza_debito_otros",  "ingreso_prim",
#     "ingreso_prim_remuneracion", "ingreso_prim_remuneracion_credito",
#     "ingreso_prim_remuneracion_debito", "ingreso_prim_renta_inversion",
#     "ingreso_prim_ied", "ingreso_prim_ied_credito",  "ingreso_prim_ied_debito",
#     "ingreso_prim_inversion_cartera", "ingreso_prim_inversion_cartera_credito",
#     "ingreso_prim_inversion_cartera_debito", "ingreso_prim_otra_inversion",
#     "ingreso_prim_otra_inversion_credito", "ingreso_prim_otra_inversion_debito",
#     "ingreso_sec_", "ingreso_sec_credito", "ingreso_sec_credito_remesas",
#     "ingreso_sec_credito_otras_trans", "ingreso_sec_debito",
#     "ingreso_sec_debito_remesas", "ingreso_sec_debito_otras_trans",
#     "cuenta_capital", "endeudamiento_neto", "cuenta_financiera",
#     "cf_inversion_directa", "cf_inversion_cartera", "cf_deuda_neta_lp",
#     "cf_deuda_neta_cp", "cf_depositos", "cf_otros", "saldo_bp",
#     "errores", "financiamiento", "finan_activos_reservas",
#     "finan_credito_fmi", "finan_condonacion_deuda",
#     "finan_otra_inversion_pasivos")
#
# # Importar datos ----------------------------------------------------------
#
# # Series Indicadores BCRD --- ---
#
# balanza_pagos <- read_excel(INTERNAL_URL_BALANZA_PAGOS,
#                             sheet = "BOP_TRIM",
#                             col_names = TRUE,
#                             skip = 8,
#                             n_max = 65)
#
# # # Adecuar archivos ------------------------------------------------------
#
# # Adecuando Balanza de Pagos --- ---
#
# balanza_pagos <-
#     balanza_pagos %>%
#     select(-contains("Ene-Jun"),
#            -contains("Ene-Sep"),
#            -contains("Ene-Dic")) %>%
#     clean_names() %>%
#     filter(!is.na(x1)) %>%
#     mutate(x1 = header_balanza_pagos) %>%
#     select(-x1) %>%
#     data.table::transpose() %>%
#     setNames(header_balanza_pagos) %>%
#     mutate(fecha = seq(as.Date("2010/1/1"),
#                        by = "quarter",
#                        length.out = length(cuenta_corriente)),
#            year = year(fecha),
#            trimestre = quarter(fecha)) %>%
#     select(fecha, year, trimestre, everything())
#
