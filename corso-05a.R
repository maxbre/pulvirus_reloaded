#!/usr/bin/env Rscript

# datiInquinanti::stazioniAria %>% filter(region_id == 6) %>% select(station_eu_code) %>% write_csv2(file = "staz_fvg.txt")
# Rscript --vanilla corso-05a.R staz_lombardia.txt "no2"

## init ####
{
  library(correlation)
  library(dplyr)
  library(logr)
  library(mgcv)
  library(purrr)
  library(readr)
  library(stringr)
  library(glue)

  library(datiInquinanti)
  library(datiMeteo)

  setwd("~/R/pulvirus_reloaded")
  rm(list = ls())
}


args <- commandArgs(trailingOnly = TRUE)

if( is.na(args[1]) | is.na(args[2]) ) {
  stop("Nessun codice stazione e/o inquinante")

}else{
  codici <- read.table(args[1], header = TRUE)
  pltnt <- args[2]
}

# variabile per tenere traccia delle stazioni elaborate 
assign("N_stazioni", c())

# function di scelta del modello ####
scelta_modello <- function(eu_code, pltnt) {
  assign("bic", TRUE, envir = .GlobalEnv)
  
  assign("eu_code", eu_code, envir = .GlobalEnv)
  assign("pltnt", pltnt, envir = .GlobalEnv)
  
  # dati ####
  {
    # bisogna trattare a parte l'ozono ####
    if(pltnt == "o3") {
      get(pltnt) %>% 
        set_names(c("reporting_year", "pollutant_fk", "station_eu_code", "date", "o3_max_h_d", "value")) -> df_pltnt
    }else{
      df_pltnt <- get(pltnt)
    }
    df_pltnt %>%
      filter(station_eu_code == eu_code, reporting_year >= 2016) %>%
      inner_join(datiMeteo::dati_meteo, by = c("station_eu_code", "date")) %>%
      mutate(
        jd = as.numeric(date - lubridate::ymd(20130101)),
        value = ifelse(value <= 0.2, 0.2, value),
        ppblmin = lag(pblmin),
        ppblmax = lag(pblmax),
        weekday = weekdays(date),
        mese = lubridate::month(date),
        lock = case_when(
          reporting_year == 2020 & mese == 3 ~ "L3",
          reporting_year == 2020 & mese == 4 ~ "L4",
          reporting_year == 2020 & mese == 5 ~ "L5",
          reporting_year == 2020 & mese == 6 ~ "L6",
          TRUE ~ "L0"
        )
      ) -> df
  }
  
  # aggiornamento variabile sui dataset non vuoti
  if(nrow(df) == 0) {
    return()
  }else{
    N_stazioni <- get("N_stazioni", envir = .GlobalEnv)
    assign("N_stazioni", c(N_stazioni, eu_code), envir = .GlobalEnv)
  }

    
  assign("df", df, envir = .GlobalEnv)
  
  v_meteo <- names(datiMeteo::dati_meteo)[4:21]
  
  select(df, all_of(v_meteo)) %>%
    correlation() %>%
    filter(r >= 0.7, r <= 0.7) %>%
    select(Parameter2) %>%
    unique() %>% unlist() %>% as.character() -> v_elevata_correlazione
  
  assign("v_elevata_correlazione", v_elevata_correlazione, envir = .GlobalEnv)
  
  v_meteo <- v_meteo[!v_meteo %in% c(v_elevata_correlazione)]
  
  assign("v_meteo", v_meteo, envir = .GlobalEnv)
  v_cappa <- length(unique(df$reporting_year))
  
  assign("ICS", list(), envir = .GlobalEnv)
  assign("v_dead", c(), envir = .GlobalEnv)
  
  # funzioni ####
  {
    source("f_buildMods_par.R")
    source("f_bestMod_par.R")
    source("f_sceltaVar_par.R")
  }
  
  fn <- file.path(glue::glue("{pltnt}_{eu_code}.log"))
  lf <- log_open(fn)
  log_print(
    datiInquinanti::stazioniAria %>% filter(station_eu_code == eu_code) %>% select(comune, nome_stazione, st_x, st_y)
  )
  
  sceltaVar("AIC")
  
  
  ## Modello finale ####
  if(pltnt == "o3") {
    get(pltnt) %>% 
      set_names(c("reporting_year", "pollutant_fk", "station_eu_code", "date", "o3_max_h_d", "value")) -> df_pltnt
  }else{
    df_pltnt <- get(pltnt)
  }
  
  df_pltnt %>%
    filter(station_eu_code == eu_code) %>%
    inner_join(datiMeteo::dati_meteo, by = c("station_eu_code", "date")) %>%
    mutate(
      jd = as.numeric(date - lubridate::ymd(20130101)),
      value = ifelse(value <= 0.2, 0.2, value),
      ppblmin = lag(pblmin),
      ppblmax = lag(pblmax),
      weekday = weekdays(date),
      mese = lubridate::month(date),
      lock = case_when(
        reporting_year == 2020 & mese == 3 ~ "L3",
        reporting_year == 2020 & mese == 4 ~ "L4",
        reporting_year == 2020 & mese == 5 ~ "L5",
        reporting_year == 2020 & mese == 6 ~ "L6",
        TRUE ~ "L0"
      )
    ) -> df
  
  
  ## check correlazione ####
  # select(df, all_of(v_meteo)) %>% correlation() %>% filter(r >= 0.7)
  {
    df %>%
      select(names(ICS)) %>%
      correlation() %>%
      cor_lower() %>%
      log_print(hide_notes = TRUE)
    
    mod_fin <-
      lapply(names(ICS), function(x)
        paste0("s(", x, ")")) %>% paste(collapse = " + ")
    
    mod_A <-
      eval(parse(
        text = glue::glue(
          "gam(value ~ weekday + lock + s(mese, bs='cc', k=12) + s(jd, k={v_cappa}) + {mod_fin}, gamma=1.4, family=gaussian(link=log), data = df)"
        )
      ))
    
    # assign("mod_A", mod_A, envir = .GlobalEnv)
    # summary(mod_A) %>% log_print(hide_notes = TRUE)
  }
  
  
  ## rimuoviamo le variabili non significative ####
  
  v_sign <- summary(mod_A)$s.table %>%
    as.data.frame() %>%
    filter(`p-value` < 0.01) %>%
    rownames() %>%
    tail(n = -2) %>%
    log_print(hide_notes = TRUE)
  
  mod_fin <- paste(v_sign, collapse = " + ")
  mod_B <-
    eval(parse(
      text = glue::glue(
        "gam(value ~ weekday + lock + s(mese, bs='cc', k=12) + s(jd, k={v_cappa}) + {mod_fin}, gamma=1.4, family=gaussian(link=log), data = df)"
      )
    ))
  assign("mod_B", mod_B, envir = .GlobalEnv)
  
  summary(mod_B) %>% log_print(hide_notes = TRUE)
  
  mod_B$model %>% select(names(mod_B$var.summary)) %>% correlation() %>% cor_lower()
  
  log_close()
  # save(mod_B, mod_A, file = glue::glue("{pltnt}_{eu_code}_B.RData"))
  
  # salvataggio dati ####
  
  # creiamo una directory per regione
  stazioniAria %>% 
    filter(station_eu_code == eu_code) %>% 
    select(region_id) %>% 
    as.numeric() -> region_id
  
  dir.create(glue("rdatas/{region_id}"), recursive = TRUE, showWarnings = FALSE)
  
  save.image(file = glue::glue("rdatas/{region_id}/{pltnt}_{eu_code}_all.RData"))

}

# mi assicuro che ci sia un numero massimo di stazioni ####
for (eu_code in codici$station_eu_code) {
  f <- glue::glue("rdatas/{pltnt}_{eu_code}_all.RData")
  
  if(file.exists(f)) {
    next
  }
  scelta_modello(eu_code, pltnt)
  
  N_stazioni <- get("N_stazioni", envir = .GlobalEnv)
  
}

