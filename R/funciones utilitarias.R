# Min max scale -----------------------------------------------------------

rescale <- function(x, new_min = 0, new_max = 1){
    old_range <-  (max(x) - min(x))
    new_range <-  (new_max - (new_min))
    new_value <-  (((x - min(x)) * new_range) / old_range) + (new_min)
    return(new_value)
}

# Crea mes ----------------------------------------------------------------

crear_mes <- function(mes, type = "text_to_number") {
    # creating the pipe
    `%>%` <- magrittr::`%>%`

    if(type == "number_to_text") {

        new_mes <- dplyr::recode(mes,
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

    }

    if(type == "number_to_shorttext"){

        new_mes <- dplyr::recode(mes,
                                 `1` = "Ene",
                                 `2` = "Feb",
                                 `3` = "Mar",
                                 `4` = "Abr",
                                 `5` = "May",
                                 `6` = "Jun",
                                 `7` = "Jul",
                                 `8` = "Ago",
                                 `9` = "Sep",
                                 `10` = "Oct",
                                 `11` = "Nov",
                                 `12` = "Dic")

    }

    if(type == "text_to_number"){

        mes  <-  stringr::str_to_title(mes)

        new_mes <- dplyr::recode(mes,
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
                                 "Dic" = 12,

                                 "Enero" = 01,
                                 "Febrero" = 02,
                                 "Marzo" = 03,
                                 "Abril" = 04,
                                 "Mayo" = 05,
                                 "Junio" = 06,
                                 "Julio" = 07,
                                 "Agosto" = 08,
                                 "Septiembre" = 09,
                                 "Octubre" = 10,
                                 "Noviembre" = 11,
                                 "Diciembre" = 12,

                                 "January" = 01,
                                 "February" = 02,
                                 "March" = 03,
                                 "April" = 04,
                                 "May" = 05,
                                 "June" = 06,
                                 "July" = 07,
                                 "August" = 08,
                                 "September" = 09,
                                 "October" = 10,
                                 "November" = 11,
                                 "December" = 12)
    }

    return(new_mes)
}


# Labels for date axis ----------------------------------------------------

date_label <- function(date = Sys.Date()) {
    paste(stringr::str_to_title(lubridate::month(date, label = TRUE)), lubridate::year(date))
}
