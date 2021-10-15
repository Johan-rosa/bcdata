# Funcion para descragar las importaciones de crudo y derivados

get_crudo <- function(frecuencia = 'mensual'){
  # Ajuste para usar el pipe sin cargar dplyr
  `%>%` <- magrittr::`%>%`
  
  # Enlace
  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "estadisticas/sector-externo/",
                     "documents/Importaciones_Crudo_6.xls?v=1634084961190")
  
  # Directorio
  file_path <- tempfile(pattern = "", fileext = "")
  
  # Descarga archivo
  download.file(file_url, file_path, mode = "wb", quiet = TRUE)
  
  file <- readxl::read_excel(file_path, 
                             sheet = 1L,
                             skip = 8,
                             col_names = c('fecha','crudo_volumen', 'crudo_precio', 'crudo_valor', 'gasolina_volumen',
                                           'gasolina_precio', 'gasolina_valor', 'gasoil_volumen', 'gasoil_precio',
                                           'gasoil_valor', 'glp_volumen', 'glp_precio', 'glp_valor', 'g_natural_volumen',
                                           'g_natural_precio', 'g_natural_valor', 'fuel_oil_volumen', 'fuel_oil_precio',
                                           'fuel_oil_valor', 'gas_aviacion_volumen', 'gas_aviacion_precio',
                                           'gas_aviacion_valor', 'avtur_volumen', 'avtur_precio', 'avtur_valor',
                                           'otros_volumen', 'otros_precio', 'otros_valor', 'total_volumen',
                                           'total_precio', 'total_valor')) %>% 
    dplyr::filter(!is.na(total_valor),
                  fecha %in% c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', ' Junio',
                               'Julio', 'Agosto', 'Septiembre', ' Octubre', 'Noviembre', 'Diciembre')) %>% 
    dplyr::mutate(fecha = seq(from = as.Date('2010-01-01'),
                              by = 'months',
                              along.with = fecha),
                  trimestre = lubridate::quarter(fecha, with_year = TRUE),
                  year = lubridate::year(fecha),
                  crudo_volumen = as.numeric(crudo_volumen))

  # Resumen por columnas: por alguna razon no he podido hacer el group_by()
  
  #if(frecuencia == 'trimestral'){
  #  crudo <- file %>% 
  #    dplyr::select(-fecha, -year) %>% 
  #    dplyr::group_by(trimestre) %>% 
  #    dplyr::mutate(dplyr::across(.cols = everything(), .fns = sum)) %>% 
  #    suppressMessages()
  #} else if(frecuencia == 'anual'){
  #  crudo <- file %>% 
  #    select(-fecha, - trimestre) %>% 
  #    group_by(year, regimen, grupo, detalle) %>% 
  #    summarize(valor_expor = sum(valor_expor)) %>% 
  #    suppressMessages()
  #} else {
  #  crudo <- file
  #}
  
  #return(crudo)
  
  return(file)
}