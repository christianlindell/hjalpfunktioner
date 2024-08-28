fpxw_get_region_codes = function(url_api = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy", niva_nchar = NULL, lan_kod = NULL) {
  
  library(tidyverse)
  library(pxweb)
  
  px_levels <- pxweb_get(url_api)
  # Kontrollera om listan med namnet "variables" innehåller en namnlös lista med 
  # en lista som heter "text" som innehåller textsträngen "region" och se vilken 
  # position i listan den har
  region_index <- which(map_lgl(px_levels$variables, ~ "region" %in% .x$text))
  
  # Sortera fram de regionkoder som efterfrågas
  if (length(region_index) > 0 ) {
    reg_kod <- px_levels$variables[[region_index]]$values
    if (!is.null(niva_nchar)) reg_kod <- reg_kod[nchar(reg_kod) == niva_nchar]
    if (!is.null(lan_kod)) reg_kod <- reg_kod[substr(reg_kod, 1, 2) %in% lan_kod]
    if (niva_nchar == 2 & !is.null(reg_kod)) reg_kod <- reg_kod[reg_kod != "00"]
    
    return(reg_kod)
  } else {
    return("Värde saknas")
  }
}

fpxw_get_region_codes(niva_nchar = 2)

