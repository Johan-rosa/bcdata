# Funcino para descargar la inversion extranjera directa

get_ied <- function(tipo = 'origen'){
  # Ajuste para usar el pipe sin cargar dplyr
  `%>%` <- magrittr::`%>%`
  
  # Enlace
  if(tipo == 'origen'){
    file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                       "estadisticas/sector-externo/",
                       "documents/inversion_ext_pais_6.xls?v=1634229219478")
  } else if(tipo == 'destino') {
    file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                       "estadisticas/sector-externo/",
                       "documents/inversion_ext_sector_6.xls?v=1634229219478")
  }
 
   # Directorio
  file_path <- tempfile(pattern = "", fileext = "")
  
  # Descarga archivo
  download.file(file_url, file_path, mode = "wb", quiet = TRUE)
  
  range <- readxl::read_excel(file_path, 
                              sheet = 1L,
                              range = 'A3:A4') %>% 
    as.character()
  
  ied <- readxl::read_excel(file_path, 
                             sheet = 1L,
                             skip = 8,
                             col_names = c('nombre', 
                                           paste0(2010:(lubridate::year(Sys.Date()))))) %>% 
    na.omit()
  
  print(range)
  return(ied)
}



