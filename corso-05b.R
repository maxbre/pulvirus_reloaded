{
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(glue)
  
  library(datiInquinanti)
  library(datiMeteo)
  
  setwd("~/R/pulvirus_reloaded")
  rm(list = ls())
}

filter(stazioniAria, regione == "VENETO") %>% select(station_eu_code) %>% write_csv(file = "staz_veneto.txt")
