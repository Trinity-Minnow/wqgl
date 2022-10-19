#' Add Lookup Values for Ammonia
#'
#' @description Creates ammonia lookup table. Rounding differs for BC, CCME and PWQO.
#' @param gl What location will be used
#' @param temp field temperature
#' @param value Field pH
#'
#' @return A vector with Temperature and pH for screening
#' @export
#'
#' @examples

glTempPh <- function(gl=c("CCME", "BC", "PWQO"),temp, pH) {

  if(gl=="CCME"){

    temp.r <- ifelse(temp < 0, 0, ifelse(temp > 30, 30, temp))
    ph.r <- ifelse(pH < 6, 6, ifelse(pH > 10, 10, pH))

    temp_ph <- paste0(DescTools::RoundTo(temp.r, 5), "_", gsub(" ", "", as.character(format(DescTools::RoundTo(ph.r, 0.5, FUN=ceiling), nsmall=1))))

    temp_ph <- gsub("_10.0", "_10", temp_ph)
  }

  if(gl=="BC"){

    temp.r <- ifelse(temp < 0, 0, ifelse(temp > 20, 20, temp))
    ph.r <- ifelse(pH < 6.5, 6.5, ifelse(pH > 9, 9, pH))

    temp_ph <- paste0(DescTools::RoundTo(temp.r, 1), "-", gsub(" ", "", as.character(format(DescTools::RoundTo(ph.r, 0.1, FUN=ceiling), nsmall=1))))

  }

  if(gl=="PWQO"){

    temp.r <- ifelse(temp < 0, 0, ifelse(temp > 30, 30, temp))
    ph.r <- ifelse(pH < 6, 6, ifelse(pH > 9, 10, pH)) #GL jumps straight from 9-10, so naything >9 will need to be rounded to 10


    temp_ph <- paste0(DescTools::RoundTo(temp.r, 1), "_", gsub(" ", "", as.character(format(DescTools::RoundTo(ph.r, 0.5, FUN=ceiling), nsmall=1))))

  }

  return(temp_ph)
}
