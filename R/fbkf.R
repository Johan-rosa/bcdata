# Funcion para descargar la formacion bruta de capital

get_fbkf <- function(nivel = TRUE){
  # Ajuste para usar el pipe sin cargar dplyr
  `%>%` <- magrittr::`%>%`
  
  # Enlace
  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "estadisticas/sector-real/",
                     "documents/fbkf.xlsx?v=1634051828540")
  
  # Directorio
  file_path <- tempfile(pattern = "", fileext = "")
  
  # Descarga archivo
  download.file(file_url, file_path, mode = "wb", quiet = TRUE)
  
  # Archivo
  if(nivel){
    file <- readxl::read_excel(file_path, 
                               sheet = 1L,
                               skip = 7,
                               n_max = 7,
                               col_names = FALSE)[,-1] %>% 
      suppressMessages()
  } else {
    file <- readxl::read_excel(file_path, 
                               sheet = 'FBKF',
                               skip = 21,
                               n_max = 7,
                               col_names = FALSE)[,-(1:2)] %>% 
      suppressMessages()
  }
  
  fbkf <- data.frame(periodo = 2006 + 1:ncol(file),
                     privado = t(file[3,]),
                     publico = t(file[4,]),
                     construccion = t(file[6,]),
                     maquinaria_y_equipos = t(file[7,]))
  
  return(fbkf)
}

