# Min max scale -----------------------------------------------------------

rescale <- function(x, new_min = 0, new_max = 1){
    old_range <-  (max(x) - min(x))
    new_range <-  (new_max - (new_min))
    new_value <-  (((x - min(x)) * new_range) / old_range) + (new_min)
    return(new_value)
}



# Crea mes ----------------------------------------------------------------


crear_mes <- function(mes, type = "largo") {
    # creating the pipe
    `%>%` <- magrittr::`%>%`

    if(is.numeric(mes)){
        dplyr::recode(mes,
                      `1` = "Enero",
                      `2` = "Febrero",
                      `3` = "Marzo",
                      `4` = "Abril",
                      `5` = "Mayo",
                      `6` = "Junio",
                      `7` = "Julio",
                      `8` = "Agosto",
                      `9` = "Septiembre",
                      `10` = "Octubre",
                      `11` = "Noviembre",
                      `12` = "Diciembre")

    } else if(type == "corto"){

        dplyr::recode(mes,
                      "Ene" = 01,
                      "Feb" = 02,
                      "Mar" = 03,
                      "Abr" = 04,
                      "May" = 05,
                      "Jun" = 06,
                      "Jul" = 07,
                      "Ago" = 08,
                      "Sep" = 09,
                      "Oct" = 10,
                      "Nov" = 11,
                      "Dic" = 12)
    }
}
