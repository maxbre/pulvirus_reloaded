# init ####

{
  library(correlation)
  library(dplyr)
  library(logr)
  library(mgcv)
  library(purrr)
  library(readr)
  library(stringr)
  
  library(datiInquinanti)
  library(datiMeteo)
  
  setwd("~/R/pulvirus_reloaded")
  rm(list = ls())
}

eu_code <- "IT0825A"
pltnt <- "no2"

assign("eu_code", eu_code, envir = .GlobalEnv)
assign("pltnt", pltnt, envir = .GlobalEnv)

# EX trasformare in una function la preparazione del dataframe ####
{
  get(pltnt) %>% 
    filter(station_eu_code == eu_code, reporting_year >= 2016) %>% 
    inner_join(datiMeteo::dati_meteo, by = c("station_eu_code", "date") ) %>%  
    mutate(
      jd = as.numeric( date - lubridate::ymd(20130101) ),
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
        TRUE ~ "L0")
    ) -> df
}

assign("df", df, envir = .GlobalEnv)

v_meteo <- names(datiMeteo::dati_meteo)[4:21]

# select(df, all_of(v_meteo)) %>% 
#   correlation() %>% 
#   filter(r >= 0.7) %>% 
#   select(Parameter2) %>% 
#   unique() %>% 
#   unlist() %>% 
#   as.character() -> v_elevata_correlazione
# 
# assign("v_elevata_correlazione", v_elevata_correlazione, envir = .GlobalEnv)

# v_meteo <- v_meteo[!v_meteo %in% c(v_elevata_correlazione)]

assign("v_meteo", v_meteo, envir = .GlobalEnv)
v_cappa <- length(unique(df$reporting_year))

assign("AICS", list(), envir = .GlobalEnv)
assign("v_dead", c(), envir = .GlobalEnv)

{
  source("f_buildMods.R")
  source("f_bestMod.R")
  source("f_sceltaVar.R")
}

fn <- file.path(glue::glue("{pltnt}_{eu_code}.log"))
lf <- log_open(fn)
log_print(datiInquinanti::stazioniAria %>% filter(station_eu_code == eu_code) %>% select(comune, nome_stazione, st_x, st_y))

sceltaVar()

## Modello finale ####
get(pltnt) %>%
  filter(station_eu_code == eu_code) %>%
  inner_join(datiMeteo::dati_meteo, by = c("station_eu_code", "date")) %>%
  mutate(
    jd = as.numeric( date - lubridate::ymd(20130101) ),
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
      TRUE ~ "L0")
  ) -> df


## check correlazione ####
# select(df, all_of(v_meteo)) %>% correlation() %>% filter(r >= 0.7)
{
  df %>% 
    select( names(AICS) ) %>% 
    correlation() %>% 
    cor_lower() %>% 
    log_print(hide_notes = TRUE)
  
  mod_fin <- lapply(names(AICS), function(x) paste0("s(", x, ")") ) %>% paste(collapse = " + ")
  
  mod_A <- eval(parse(text = glue::glue("gam(value ~ weekday + lock + s(mese, bs='cc', k=12) + s(jd, k={v_cappa}) + {mod_fin}, gamma=1.4, family=gaussian(link=log), data = df)")))
  
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
mod_B <- eval(parse(text = glue::glue("gam(value ~ weekday + lock + s(mese, bs='cc', k=12) + s(jd, k={v_cappa}) + {mod_fin}, gamma=1.4, family=gaussian(link=log), data = df)")))
# assign("mod_B", mod_B, envir = .GlobalEnv)

summary(mod_B) %>% log_print(hide_notes = TRUE)

mod_B$model %>% select( names(mod_B$var.summary) ) %>% correlation() %>% cor_lower()

log_close()
# save(mod_B, mod_A, file = glue::glue("{pltnt}_{eu_code}_B.RData"))


## check modello ####
mod_viz <- mgcViz::getViz(mod_B)
mod_viz$model %>% select( names(mod_viz$var.summary) ) %>% correlation() %>% cor_lower()

assign("mod_viz", mod_viz, envir = .GlobalEnv)

mgcViz::check.gamViz(mod_viz)
mgcViz::plot.gamViz(mod_viz)

save.image(file = glue::glue("rdatas/{pltnt}_{eu_code}_all.RData"))

