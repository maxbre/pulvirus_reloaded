library(dplyr)
# library(glue)
library(logr)
library(mgcv)
library(readr)
library(modelr)
library(tidyverse)

setwd("~/R/pulvirus/presentazione")

## importazione dati ####
file.dati <- list.files(path = "~/R/pulvirus/presentazione/dati-lazio/per-stazione/valide", full.names = TRUE, pattern = "*.csv")

file.dati %>%
  purrr::map(function(file_name){ 
    read_csv(file_name)
  }) -> dfs

names(dfs) <- basename(file.dati) %>% str_remove(pattern = ".csv")


## di cosa abbiamo bisogno ####

# costruire le stringhe dei modelli
# scegliere il modello migliore
# predisporre il modello backward

enframe(c("x0", "x1", "x2", "x3", "x4"))
edfs <- enframe(dfs)

edfs %>% 
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )

## costruiamo i modelli ####

vars <- c("x0", "x1", "x2", "x3", "x4")

c0 <- combn(vars, 1) 

w <- c0 %>% imap_chr(~glue::glue("gam(value ~ s({.x}), data = df)"))

w <- c0 %>% map(~glue::glue("gam(value ~ s({.x}))"))

w <- lapply(c0, function(x) paste0("gam(value ~ s(", x, "), data = df)"))


## con le variabili meteo ####

v_meteo <- names(datiMeteo::dati_meteo)[4:21]
w <- v_meteo %>% 
  imap_chr(~ glue::glue("gam(value ~ s({.x}), data = df)"))

# scegliamo un sottoinsieme di 4 stazioni
readRDS("~/R/pulvirus/presentazione/valide.RDS") %>% 
  tail(n = 4) -> valide 

dfs2 <- dfs[names(dfs) %in% valide] 

dfs2 %>% 
  map(\(df) {
    map(w, \(y) eval(parse(text = y)))
  }) -> mod.res

mod.res %>% 
  map(~ map_dbl(.x, AIC)) 


## applichiamo i modelli ####

# aggiungiamo trasformata logaritmica e julian day
w <- v_meteo %>% imap_chr(~ glue::glue("gam(value ~ s(jd) + s({.x}), gamma = 1.4, family = gaussian(link = log), data = df)"))

dfs2 %>% 
  map(\(df) {
    map(w, \(y) eval(parse(text = y)))
  }) -> mod.res

# AIC dei modelli
mod.res %>% 
  map(~ map_dbl(.x, AIC))

# summary del GAM (mgcv)
mod.res %>% 
  map(~ map(.x, summary.gam)) 


## variabile più performante? ####

map(mod.res, ~ map_dbl(.x, AIC)) %>% 
  map_dbl(\(aic) which.min(aic) ) -> mins

v_meteo[mins] %>% as.data.frame() %>% setNames(c("variabile"))
v1 <- v_meteo[mins]

# metodo alternativo 
ex_fun <- function(arg1, arg2){
  arg1[[arg2]][["var.summary"]][2] %>% names()
}
v1 <- map2(mod.res, mins, ex_fun) %>% unlist(use.names = FALSE) 


## nuovi modelli N > 1 ####

v1 %>% 
  map(function(v) {
    res <- v_meteo[!v_meteo %in% v] # eliminiamo dalle variabili quella entrata nel modello
  })

# le stringhe dei modelli bivariati
v1 %>% 
  map(function(v) {
    res <- v_meteo[!v_meteo %in% v]
    stringi::stri_join("s(", rep(v, length(res)), ") + s(", res, ")")
  }) -> vv1 

# i modelli bivariati
m2 <- map(vv1, ~ glue::glue("gam(value ~ {.x}, gamma = 1.4, family = gaussian(link = log), data = df)"))

# cosa dovremmo fare adesso ? ####

# caso base
m2[[1]]
dfs2[[1]]

df <- dfs2[[1]]
imap(m2[[1]], ~eval(parse(text = .x)))

imap(m2[[1]], ~eval(parse(text = .x))) %>% 
  map(\(mod) {
    summary(mod)
    # AIC(mod)
  })

imap(dfs2, \(x, idx) {
  paste0(idx, ": ", nrow(x))
})

imap(dfs2, \(x, idx) {
  paste0("Indice: ", idx, ": Numerosità ", nrow(x))
})

# non funziona
imap(dfs2, \(x, idx) {
  paste0("Indice: ", idx, ": modelli ", w2[[idx]])
})


## modelli per tutte le stazioni (sui dataframe) ####
names(m2) <- names(dfs2)

# questo funziona
imap(dfs2, \(x, idx) {
  paste0("Indice: ", idx, ": modelli ", m2[[idx]])
})

imap(dfs2, \(x, idx) {
  paste0("EU code: ", idx, ": modelli ", m2[[idx]])
})

imap(dfs2, \(x, idx) {
  imap(m2[[idx]], ~eval(parse(text = .x)))
}) -> mod2.res

## modello bivariato più performante ####

mod2.res %>% 
  map(~ map_dbl(.x, AIC)) %>% 
  map_dbl(\(aic) which.min(aic) ) -> mins2

v_meteo[mins2] %>% as.data.frame() %>% setNames(c("variabile"))

ex_fun <- function(arg1, arg2){
  arg1[arg2]
}

map2_dfr(m2, mins2, ex_fun)

# mod2.res[["IT2173A"]][[14]][["var.summary"]]
ex_fun <- function(arg1, arg2){
  arg1[[arg2]][["var.summary"]][2] %>% names()
}
map2(mod2.res, mins2, ex_fun) %>% unlist(use.names = FALSE)



## con l'approccio "classico" ####
models <- list(list(), list())
for (d in seq_along(dfs2)) {
  cat(d, "<== \n")
  
  for (m in seq_along(w2[[d]])) {
    cat("\t", m, "<--", w2[[d]][[m]]  , "\n")
    
    df <- dfs2[[d]]
    models[[d]][[m]] <-  map(w2[[d]][[m]], \(y) eval(parse(text = y)))
    
  }
}

# for(eu_code in stazioni_valide$station_eu_code) {
#   # apro il file di log
#   f_log <- file.path(glue::glue("tmp/{eu_code}.log"))
#   lf <- log_open(f_log)
#   
#   df <- read_csv(glue("dati-lazio/per-stazione/{eu_code}.csv"))
#   # cappa <- length(unique(dfSub$reporting_year))
#   
#   # inizializzo le liste che popolerà la funzione "sceltaVar"
#   AICS <- list()
#   v_dead <- c()
#   RUN <- 0
#   
#   # il set di variabili iniziali che voglio includere variabili da inizializzare ad ogni tornata
#   v_meteo <- c("t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
#             "sp", "nirradiance", "ppblmin", "ppblmax", "pblmin", "pblmax", "wspeed_max")
#   
#   assign("AICS", AICS, envir = .GlobalEnv)
#   assign("v_dead", v_dead, envir = .GlobalEnv)
#   # assign("dfSub", dfSub, envir = .GlobalEnv)
#   assign("v_meteo", v_meteo, envir = .GlobalEnv)
#   # assign("cappa", cappa, envir = .GlobalEnv)
#   
#   log_print(sprintf("Stazione: %s", eu_code), hide_notes = TRUE)
#   
#   # sceltaVar(cappa)
#   
#   log_print("Salvataggio dati")
#   # saveRData(eu_code, pltnt, cod_reg, out_dir)
#   
#   log_close()
# }
