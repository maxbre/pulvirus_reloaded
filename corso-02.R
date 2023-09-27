{
  library(dplyr)
  library(glue)
  library(logr)
  library(mgcv)
  library(readr)
  library(modelr)
  library(tidyverse)
}

setwd("/home/rmorelli/R/pulvirus_reloaded")

## importazione dati ####
file.dati <- list.files(path = "~/R/pulvirus_reloaded/dati-lazio/per-stazione/valide", full.names = TRUE, pattern = "*.csv")

file.dati %>%
  purrr::map(
    function(file_name) { 
      read_csv(file_name)
    }
  ) -> dfs

names(dfs) <- basename(file.dati) %>% str_remove(pattern = ".csv")


## di cosa abbiamo bisogno ####

# costruire le stringhe dei modelli
# scegliere il modello migliore


## costruiamo i modelli ####

vars <- c("x0", "x1", "x2", "x3", "x4")

c0 <- combn(vars, 1)

w <- c0 %>% 
  imap_chr(
    ~glue::glue("gam(value ~ s({.x}), data = df)")
  )

w <- c0 %>% map(~glue::glue("gam(value ~ s({.x}))"))

w <- lapply(c0, function(x) paste0("gam(value ~ s(", x, "), data = df)"))
w

rm(vars, c0)
## con le variabili meteo ####

v_meteo <- names(datiMeteo::dati_meteo)[4:21]

w <- v_meteo %>% 
  imap_chr(~ glue::glue("gam(value ~ s({.x}), data = df)"))
w

# scegliamo un sottoinsieme di 4 stazioni
readRDS("~/R/pulvirus_reloaded/valide.RDS") %>% 
  tail(n = 4) -> valide 
valide

dfs <- dfs[names(dfs) %in% valide] 
length(dfs)

# applichiamo tutti modelli per ogni elemento della lista
s <- Sys.time()
dfs %>% 
  map(\(df) {
    map(w,
        function(y) {
          eval(parse(text = y))
        }
    )
  }) -> mod.res

e <- Sys.time()
e - s

# con ciclo for nidificato ####
s <- Sys.time()
appoggio <- list()
for(i in names(dfs)) {
  df <- dfs[[i]]
  for(j in w) {
    appoggio[[i]][j] <- eval(parse(text = j))
  }
}
e <- Sys.time()
e - s

# calcoliamo l'indice di Akaike sui risultati
mod.res %>% 
  map(~ map_dbl(.x, AIC))

# il BIC
mod.res %>% 
  map(~ map_dbl(.x, BIC))


## applichiamo i modelli adeguati ####

# modelli N = 1 ####
# aggiungiamo trasformata logaritmica e julian day
m1 <- v_meteo %>% 
  imap_chr(~ glue::glue("gam(value ~ s(jd) + s({.x}), gamma = 1.4, family = gaussian(link = log), data = df)"))
enframe(m1)


# andiamo ad applicare i modelli
s <- Sys.time()

dfs %>% 
  map(\(df) {
    map(m1, \(y) eval(parse(text = y)))
  }) -> mod1.res
e <- Sys.time()
e - s

saveRDS(mod1.res, file = "mod1.res.RDS")

# AIC dei modelli
mod1.res %>% 
  map(~ map_dbl(.x, AIC))

# summary del GAM (mgcv)
mod1.res %>% 
  map(~ map(.x, summary.gam)) 

## variabile più performante? ####

# estraggo gli indici dei modelli migliori per le stazioni
map(mod1.res, ~ map_dbl(.x, AIC)) %>% 
  map_dbl(\(aic) which.min(aic) ) -> mins1

m1[mins1]

# guardiamo dentro l'oggetto che ci restituisce mgcv e intercettiamo l'ultima 
# variabile entrata nel modello migliore oltre al julian day
mod1.res[["IT2173A"]][[17]][["var.summary"]] %>% names()

#  .fun  function
ex_fun <- function(arg1, arg2) {
  arg1[[arg2]][["var.summary"]] %>% names() %>% last()
}

map2(mod1.res, mins1, ex_fun) %>% unlist(use.names = FALSE)

# utilizziamo una lista
v1 <- map2(mod1.res, mins1, ex_fun) 

map2(mod1.res, mins1, ex_fun) 


## modelli N = 2 ####

# - eliminiamo dalle variabili meteo quella entrata nel modello, dobbiamo farlo 
# per ciascuna stazione separatamente
# - dobbiamo tenere traccia delle variabili scelte per stazione

v_fixed <- data.frame()

v1 %>% 
  map(function(v) {
    res <- v_meteo[!v_meteo %in% v] 
  })

v_fixed <- rbind(v_fixed, v1) %>% as.data.frame()

# This is useful if you need to compute on both the value and the position of an element ####
{
  imap(dfs2, \(x, idx) {
    paste0(idx, ": ", nrow(x))
  })
  
  imap(dfs2, \(x, idx) {
    paste0("Indice: ", idx, ": Numerosità ", nrow(x))
  })
  
  # non funziona
  imap(dfs2, \(x, idx) {
    paste0("Indice: ", idx, ": modelli ", m1[[idx]])
  })
  
  # perché ho bisogno di indici
  names(m1)
  
  ## modelli per tutte le stazioni (sui dataframe) ####
  names(m2) <- names(dfs2)
  
  # questo funziona
  imap(dfs2, \(x, idx) {
    paste0("Indice: ", idx, ": modelli ", m1[[idx]])
  })
  
  imap(dfs2, \(x, idx) {
    paste0("EU code: ", idx, ": modelli ", m2[[idx]])
  })
}

imap(v_fixed, \(x, idx) {
  paste0(x, ":", idx)
  res <- v_meteo[!v_meteo %in% v_fixed[[idx]] ]

  a <- stringi::stri_join("s(", v_fixed[[idx]], ")", collapse = " + ")

  stringi::stri_join(a, " + s(", res, ")")
  
})

# parte fissa modelli N=2 ####
imap(v_fixed, \(x, idx) {
  # paste0(x, ":", idx)
  res <- v_meteo[!v_meteo %in% v_fixed[[idx]] ]
  
  a <- stringi::stri_join("s(", v_fixed[[idx]], ")", collapse = " + ")
  
  stringi::stri_join(a, " + s(", res, ")")
  
}) -> vv2

m2 <- map(vv2, ~ glue::glue("gam(value ~ s(jd) + {.x}, gamma = 1.4, family = gaussian(link = log), data = dfs[[idx]])"))

imap(dfs, \(x, idx) {
  imap(m2[[idx]], ~eval(parse(text = .x)))
}) -> mod2.res


## modello bivariato più performante ####

mod2.res %>% 
  map(~ map_dbl(.x, AIC)) %>% 
  map_dbl(\(aic) which.min(aic) ) -> mins2

# di nuovo eleganti
# mod2.res[["IT2173A"]][[14]][["var.summary"]]
ex_fun <- function(arg1, arg2){
  arg1[[arg2]][["var.summary"]] %>% names() %>% last()
}

v2 <- map2(mod2.res, mins2, ex_fun)
v2

# aggiorniamo ####
v_fixed <- rbind(v_fixed, v2) %>% as.data.frame()

imap(v_fixed, \(x, idx) {
  # paste0(x, ":", idx)
  v_meteo[!v_meteo %in% v_fixed[[idx]] ]
})

imap(v_fixed, \(x, idx) {
  # paste0(x, ":", idx)
  res <- v_meteo[!v_meteo %in% v_fixed[[idx]] ]
  
  a <- stringi::stri_join("s(", v_fixed[[idx]], ")", collapse = " + ")
  
  stringi::stri_join(a, " + s(", res, ")")

})

# le stringhe dei modelli N = 3 per ogni stazione saranno quindi
imap(v_fixed, \(x, idx) {
  # paste0(x, ":", idx)
  res <- v_meteo[!v_meteo %in% v_fixed[[idx]] ]
  
  a <- stringi::stri_join("s(", v_fixed[[idx]], ")", collapse = " + ")
  
  stringi::stri_join(a, " + s(", res, ")")
  
}) -> vv3

m3 <- map(vv3, ~ glue::glue("gam(value ~ s(jd) + {.x}, gamma = 1.4, family = gaussian(link = log), data = dfs[[idx]])"))

# modelli N=3 ####
imap(dfs, \(x, idx) {
  imap(m3[[idx]], ~eval(parse(text = .x)))
}) -> mod3.res

mod3.res %>% 
  map(~ map_dbl(.x, AIC)) 

# variabile più performante N=3 ####
map(mod3.res, ~ map_dbl(.x, AIC)) %>% 
  map_dbl(\(aic) which.min(aic) ) -> mins3

# mod1.res[["IT2173A"]][[17]][["var.summary"]]
#  .fun  function
ex_fun <- function(arg1, arg2) {
  arg1[[arg2]][["var.summary"]] %>% names() %>% last()
}

map2(mod3.res, mins3, ex_fun) %>% 
  unlist(use.names = FALSE)

# utilizziamo una lista
v3 <- map2(mod3.res, mins3, ex_fun) 

map2(mod3.res, mins3, ex_fun) 

# aggiorniamo ####
v_fixed <- rbind(v_fixed, v3) %>% as.data.frame()
