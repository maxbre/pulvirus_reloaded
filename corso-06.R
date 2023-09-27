# validazione ####

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
  setwd("~/R/pulvirus_reloaded")
  rm(list = ls())
}


outdir <- "~/R/pulvirus_reloaded/validazione/"

args <- commandArgs(trailingOnly = TRUE)

if( is.na(args[1]) & is.na(args[2]) ) {
  region_id <- "6"
  pltnt <- "o3"
}else{
  pltnt <- args[1]
  region_id <- args[2]
}

rdatas <- list.files(path = glue("./rdatas/{region_id}"),
                     pattern = paste0("^", pltnt, "_(.*).RData"),
                     recursive = TRUE,
                     full.names = TRUE)

my_list <- list()
my_list_tdf <- list()
my_mat <- matrix()

lf <- log_open(glue::glue("validazione/{pltnt}.log"))

log_print((pltnt))
set.seed(1974)

log_print( sprintf("Stazioni totali: %s", length(rdatas)) , hide_notes = TRUE)

run <- 1
for (i in rdatas) {
  load(i)
 
    str_split(i, pattern = "_") %>% 
    unlist() %>% 
    grep(pattern = "IT") -> code_idx

    eu_code <- file_path_sans_ext(  
      str_split(basename(i), "_")[[1]][code_idx] 
    )
    
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
    
    d <- floor(nrow(dfc) * 0.8)
    s <- sample(dfc$jd, size = d)
    
    # dataset ####
    pdf <- dfc[ which( !(dfc$jd %in% s)), ] # predict
    tdf <- dfc %>% filter(jd %in% s) # training
    
    # addestriamo il modello sul DF di training
    gam_tdf <- gam(formula.gam(mod_B), data = tdf, family = family(mod_B))
    
    # applichiamo al DF di predict
    gam_pdf <- predict.gam(gam_tdf, newdata = pdf)
    
    # confrontiamo adesso i valori predetti e gli "osservati" sull'80%
    # Factor of 2 ####
    { 
      tmpdf <- cbind(exp(as.numeric(gam_pdf)), pdf$value) %>% 
        as.data.frame() %>%  
        setNames(c("pred", "obs"))
      
      n <- length(pdf$value)
      tmpdf %>% 
        mutate(flag = ifelse(between(pred/obs, 0.5, 2), pred/obs, 0) ) %>% 
        filter(flag > 0) %>% 
        nrow() -> nvalidate
    }
    
    # Fractional BIAS  ####
    compute.fb(tmpdf$pred, tmpdf$obs) -> fb
    
    # Normalized Mean Square Error  ####
    {  
      nmse <- tmpdf %>% 
        mutate(num = (obs - pred)^2, den = obs*pred) %>% 
        summarise(sum(num)/sum(den))
      
      rsq <- function (x, y) cor(x, y) ^ 2
      
      rsq(pdf$value, as.numeric(gam_pdf))
      rsq(gam_tdf$y, gam_tdf$fitted.values)
    }
    
    # paste(deparse(formula.gam(mod_B)), collapse = "")
    my_list[[eu_code]] <- c(rmse(pdf$value, exp(as.numeric(gam_pdf))),  # 20%
                            rmse(gam_tdf$y, gam_tdf$fitted.values), # 80%
                            mse(pdf$value, exp(as.numeric(gam_pdf))), # 20%
                            mse(gam_tdf$y, gam_tdf$fitted.values), # 80%
                            rsq(pdf$value, exp(as.numeric(gam_pdf))), # 20%
                            rsq(gam_tdf$y, gam_tdf$fitted.values), # 80%
                            nvalidate/n, # FAC2
                            fb, # Fractional BIAS
                            nmse # Normalized Mean Square Error
    )
    run <- run + 1
}

log_close()


my_mat <- do.call(rbind, my_list)
my_df <- data.frame(id = names(my_list), my_mat)

colnames(my_df) <- c("station_eu_code", "rmse_20", "rmse_80", "mse_20", "mse_80", "rsq_20", "rsq_80", "FAC2", "FB", "NMSE")

# salvataggio dati ####

# creiamo una directory per regione
stazioniAria %>% 
  filter(station_eu_code == eu_code) %>% 
  select(region_id) %>% 
  as.numeric() -> region_id

dir.create(glue("validazione/{region_id}"), recursive = TRUE, showWarnings = FALSE)

sapply(my_df, function(x) { as.vector(x) }) %>% 
  write.table(file = glue::glue("validazione/{region_id}/validazione_{pltnt}.csv"), sep = ";", row.names = FALSE)

# Criteri di validazione modelli ####
# Rsq(80) > 0.5
# 0.8 <= (Rsq20 /Rsq80) <= 1.2
# 0.5 <= (RMSE20 /RMSE80) <= 1.5
# NMSE <= 0.5
# FB <= 0.5
# FA2 >= 0.80