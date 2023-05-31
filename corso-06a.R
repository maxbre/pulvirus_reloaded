# incertezza ####

# init ####
{
  library(datiInquinanti)
  library(datiMeteo)
  library(dplyr)
  library(lubridate)
  library(mgcv)
  library(mgcViz)
  library(Metrics)
  library(glue)
  library(FSMUMI)
  library(stringr)
  library(tools)
  library(logr)
  library(readr)
  library(tibble)
  
  setwd("~/R/pulvirus_reloaded")
  rm(list = ls())
}


outdir <- "~/R/pulvirus_reloaded/validazione/"

args <- commandArgs(trailingOnly = TRUE)

if(is.na(args[1]) & is.na(args[2]) ) {
  region_id <- "3"
  pltnt <- "pm25"
}else{
  pltnt <- args[1]
  region_id <- args[2]
}

rdatas <- list.files(path = glue("./rdatas/{region_id}"),
                     pattern = paste0("^", pltnt, "_(.*).RData"),
                     recursive = TRUE,
                     full.names = TRUE)

# stazioni che hanno superato la validazione ####
{
  df <- read_delim(glue::glue("validazione/{region_id}/validazione_{pltnt}.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE)

  df[(df[["FAC2"]] >= 0.8 & 
        df[["rsq_80"]] >= 0.5 &
        between( df[["rsq_20"]] / df[["rsq_80"]], 0.8, 1.2) &  
        between( df[["rmse_20"]] / df[["rmse_80"]], 0.5, 1.5) & 
        df[["FB"]] <= 0.5 & 
        df[["NMSE"]] <= 0.5),] %>% select(station_eu_code) -> s_valide

}

my_list <- list()
my_list_tdf <- list()
my_mat <- matrix()

lf <- log_open(glue::glue("incertezza/{region_id}/{pltnt}.log"))

log_print((pltnt))
log_print( sprintf("Stazioni totali: %s", length(rdatas)) , hide_notes = TRUE)
log_print( sprintf("Stazioni valide: %s", nrow(s_valide)), hide_notes = TRUE)

run <- 1
for (i in rdatas) {
  load(i)
  
  str_split(i, pattern = "_") %>% 
    unlist() %>% 
    grep(pattern = "IT") -> code_idx 
  
  eu_code <- file_path_sans_ext(  str_split(basename(i), "_")[[1]][code_idx] )
  
  log_print(sprintf("%s %d", eu_code, run), hide_notes = TRUE)
  log_print(sprintf("%s ", as.character( formula.gam(mod_B)))[3], hide_notes = TRUE)
  
  # mettiamo insieme il data frame ####
  {
    if(pltnt == "o3") {
      get(pltnt) %>% 
        setNames(c("reporting_year", "pollutant_fk", "station_eu_code", "date", "o3_max_h_d", "value")) -> df_pltnt
    }else{
      df_pltnt <- get(pltnt)
    }
    df_pltnt %>% 
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
      ) %>% 
      select(-c(coordx, coordy, altitude, altitudedem)) -> df
  }
  
  
  if(nrow(df) == 0) {
    cat("stazione senza dati nel file", eu_code)
    log_print(sprintf("no data %s", eu_code), hide_notes = TRUE)
    
    next
  }
  
  dfc <- na.omit(df)
  
  if(nrow(dfc) == 0) {
    cat("stazione con troppi valori nulli", eu_code)
    log_print(sprintf("NULL data %s", eu_code), hide_notes = TRUE)
    
    next
  }  

  vars <- sort( c("lockL3", "lockL4", "lockL5", "lockL6") )
  dfapp <- data.frame(vars)
  
  # run per l'incertezza ####
  for (k in 1:30) {
    log_print( paste("run ", k, collapse = " "), hide_notes = TRUE )
    
    d <- floor(nrow(dfc) * 0.8)
    s <- sample(dfc$jd, size = d) 
    
    # dataset ####
    pdf <- dfc[which( !(dfc$jd %in% s)),] # predict
    tdf <- dfc %>% filter(jd %in% s) # training
    
    # addestriamo il modello sul DF di training
    gam_tdf <- gam(formula.gam(mod_B), data = tdf, family = family(mod_B))
    
    # applichiamo al DF di predict
    gam_pdf <- predict.gam(gam_tdf, newdata = pdf)
    
    # calcolo contributo assoluto ####
    b <- anova.gam(gam_tdf)
    
    intercetta <- b$p.table[1,]
    
    b$p.table %>%
      as.data.frame() %>%
      setNames(c("stima", "stderr", "tvalue", "pvalue") ) %>%
      filter(pvalue < 0.01) %>%
      tail(n = 4) %>%
      mutate(Concentrazione = (exp(stima + (stderr^2 /2) ) - 1 ) * exp(intercetta[1] + intercetta[2]^2 /2 ) ) %>%
      select(Concentrazione) %>%
      rownames_to_column %>%
      filter(str_detect(rowname, "^lockL")) %>% setNames(c("vars", "concentrazione")) -> tmp
    
    df1 <- left_join(dfapp, tmp, by = "vars") %>% 
      select(concentrazione)
    
    # assign(names(models[[1]]), df1)
    
    dfapp <- cbind(dfapp, df1)
    
    colnames(dfapp)[ncol(dfapp)] <- k
  }
  
  my_list[[eu_code]] <- dfapp
  
  # paste(deparse(formula.gam(mod_B)), collapse = "")
  
  run <- run + 1
}

log_close()

# creiamo una directory per regione
stazioniAria %>% 
  filter(station_eu_code == eu_code) %>% 
  select(region_id) %>% 
  as.numeric() -> region_id

dir.create(glue("incertezza/{region_id}"), recursive=TRUE)

save( my_list, file = glue("incertezza/{region_id}/ic_{pltnt}.RData") )
log_close()

# colnames(my_df) <- c("station_eu_code", "rmse_20", "rmse_80", "mse_20", "mse_80", "rsq_20", "rsq_80", "FAC2", "FB", "NMSE")

# sapply(my_df, function(x) { as.vector(x) }) %>% 
# write.table(file = glue::glue("{outdir}/validazione_{pltnt}.csv"), sep = ";", row.names = FALSE)