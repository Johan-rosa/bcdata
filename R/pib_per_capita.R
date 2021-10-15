# Funciones para descargar el PIB per capita

get_pib_per_capita <- function(nivel = TRUE){
  # Ajuste para usar el pipe sin cargar dplyr
  `%>%` <- magrittr::`%>%`
  
  # Enlace
  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "estadisticas/sector-real/",
                     "documents/pib_dolares.xls?v=1634051828540")
  
  # Directorio
  file_path <- tempfile(pattern = "", fileext = "")
  
  # Descarga archivo
  download.file(file_url, file_path, mode = "wb", quiet = TRUE)
  
  # Archivo
  pib_per_capita <- readxl::read_excel(file_path, 
                                       sheet = 1L,
                                       skip = 8,
                                       col_types = c(rep('numeric',7)),
                                       col_names = c("periodo", "poblacion",
                                                     "pib_corriente_RD",
                                                     "pib_corriente_per_capita_RD",
                                                     "pib_corriente_US",
                                                     "pib_corriente_per_capita_US",
                                                     "pib_referencia_2007")) %>% 
    suppressWarnings()
  
  niveles <- pib_per_capita[1:(min(which(is.na(pib_per_capita$poblacion))) - 1),] %>% 
    dplyr::mutate(periodo = 1990 + dplyr::row_number())
  
  crecimiento <- pib_per_capita[(max(which(is.na(pib_per_capita$poblacion))) + 1):nrow(pib_per_capita),] %>% 
    dplyr::mutate(periodo = 1990 + dplyr::row_number())
  
  if(nivel){
    pib_per_capita <- niveles
  } else {
    pib_per_capita <- crecimiento
  }
  
  return(pib_per_capita)
}

