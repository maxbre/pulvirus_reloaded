# validazione delle serie 

# calcolo i mesi validi ####
library(datiInquinanti)
library(dplyr)
library(purrr)
library(naniar)
library(tibble)
# data(package = "datiInquinanti")

## controllo dati ####
# - una serie e'valida se lunga almeno 5 anni 
# - un mese e' valido se contiene almeno il 75% di dati validi (senza ulteriori sulla continuit dei blocchi dei dati mancanti all'interno del mese)
# - un anno e' valido se contiene tutte le stagioni valide (almeno due mesi validi per stagione)
# - le serie devono avere tutti i mesi validi nel 2020

validazioneSerie <- function(pltnt, cod_reg) {
  serie <- as_tibble_col( 
    seq(lubridate::ymd('2016-01-01'), 
        lubridate::ymd('2020-06-30'), by = 'days'), column_name = "date")
  
  filter(datiInquinanti::stazioniAria, region_id == cod_reg) -> stazioni_reg
  
  get(pltnt) %>% 
    filter( !is.na(value), value != -999, station_eu_code %in% stazioni_reg$station_eu_code ) %>% 
    mutate(
      mese = lubridate::month(date),
      stagione =
        case_when(
          format(date, "%m") %in% c("01", "02", "12") ~ "inverno",
          format(date, "%m") %in% c("03", "04", "05") ~ "primavera",
          format(date, "%m") %in% c("06", "07", "08") ~ "estate",
          TRUE ~ "autunno"
        ) 
    ) -> df
  
  df <- left_join(serie, df, by = "date")
  
  df %>%
    split(.$station_eu_code) %>%
    map(function(df) {
      df %>%
        group_by(reporting_year, mese, stagione) %>%
        miss_var_summary() %>%
        filter(variable == "value" ) %>%
        mutate(flag_mese = ifelse(pct_miss > 25, 0, 1) )
    }) -> mc.mesi

  # calcolo stagioni valide ####
  mc.mesi %>% 
    map(\(df) {
      df %>% 
        group_by(reporting_year, stagione) %>% 
        summarise(n = sum(flag_mese), .groups = 'drop') %>% 
        mutate(flag_stagione = ifelse(n > 1, 1, 0))    
    }) -> mc.stagioni
  
  # calcolo anni validi ####
  mc.stagioni %>% 
    map(\(df) {
      df %>% 
        select(reporting_year, flag_stagione) %>% 
        group_by(reporting_year) %>% 
        summarise(n = sum(flag_stagione),  .groups = 'drop') %>% 
        mutate(flag_anno = ifelse(n != 4, 0, 1) )    
    }) -> mc.anni
  
  # stazioni che superano i criteri ####
  mc.anni %>% 
    map(\(df) {
      df %>% 
        filter(reporting_year > 2015 & 
                 reporting_year < 2020 & 
                 flag_anno == 1
               ) %>% 
        nrow() == 4
    }) %>% unlist() -> l1
  
  mc.mesi %>% 
    map(\(df) {
      df %>% 
        filter(reporting_year == 2020 & mese < 7 & flag_mese == 1) %>% 
        nrow() == 6
    }) %>% unlist() -> l2
  
  do.call(cbind, list(l1, l2)) %>% 
    as.data.frame() %>% 
    setNames( c("v1519", "v20") ) %>% 
    rownames_to_column(var = "station_eu_code") -> df_valide
  
  # return( list(mc.mesi, mc.stagioni, mc.anni, l1, l2) )
  return( df_valide )
}

# validazioneSerie("no2", 12) 

names(X2020_Dati_Lazio_NO2)[1:2] <- c("data", "ora")

X2020_Dati_Lazio_NO2 %>% 
  select(-`Cod. EU Stazione`) %>% 
  reshape2::melt(id.vars = c("data", "ora")) %>% 
  filter(value > 0.2) %>% 
  group_by(data, variable) %>% 
  summarise(media = mean(value) ) %>% 
  mutate(
    reporting_year = lubridate::year(data),
    pollutant_fk = 8,
    ) %>% 
  setNames(c("date", "station_eu_code", "value", "reporting_year", "pollutant_fk")) %>%
  select(reporting_year, pollutant_fk, station_eu_code, date, value) %>%
  rbind(NO2_2013_2019) %>% arrange(reporting_year, date) -> no2

validazioneSerie("no2", 12) %>% 
  mutate(valida = v1519 & v20 ) %>% filter(valida == TRUE) -> df_valide


