# validazione delle serie 

# calcolo i mesi validi ####

no2 %>% 
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
      filter(reporting_year > 2015 & reporting_year < 2020 & flag_anno == 1) %>% 
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
  setNames(c("v1519", "v20")) %>% 
  rownames_to_column(var = "station_eu_code") -> df_valide

df_valide %>% mutate(valida = v1519 & v20) ->  df_valide