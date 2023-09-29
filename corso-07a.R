# Estrazione contributo lockdown ####

# init ####
{
  library(glue)
  library(mgcv)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(readr)
  
  setwd("~/R/pulvirus_reloaded")
  rm(list = ls())
}

args <- commandArgs(trailingOnly = TRUE)

if(is.na(args[1])) {
  pltnt <- "o3"
  cod_reg <- "3"
}else{
  pltnt <- args[1]
  cod_reg <- args[2]
}

reg <- FALSE

rdatas <- list.files(path = glue("./rdatas/{cod_reg}"),
                     pattern = glue("^{pltnt}_(.*).RData"),
                     recursive = TRUE,
                     full.names = TRUE)

my_list <- list()
my_list_tdf <- list()
my_mat <- matrix()

vars <- sort( c("lockL3", "lockL4", "lockL5", "lockL6") )
enframe(name = "mese", vars) # tibble

df_app <- data.frame("mese" = vars) # conterrà le stime delle concentrazioni

run <- 1
{
  for(i in rdatas) {
    load(i)
    
    print(paste(i, run))
    
    b <- anova.gam(mod_B)
    intercetta <- b$p.coeff[1]

    indxs <- grep("lock*", rownames(b$p.table)) # pignoleria
    
    as.data.frame(b$p.table[indxs, ]) %>% 
      setNames(c("stima", "stderr", "tvalue", "pvalue")) %>% 
      filter(pvalue < 0.01) %>%
      mutate(Concentrazione = (exp(stima + (stderr^2/2)) - 1) * 1) %>%
      select(Concentrazione) %>%   
      rownames_to_column %>%
      setNames(c("mese", "concentrazione")) -> tmp
    
    df_conc <- left_join(df_app, tmp, by = "mese") %>% select(concentrazione)
    
    df_app <- cbind(df_app, df_conc)

    colnames(df_app)[ncol(df_app)] <- stringr::str_split(basename(i), "_")[[1]][2]
    
    run <- run + 1
  }
  
  tdata <- data.table::transpose(df_app[, -c(1)])
  
  rownames(tdata) <- colnames(df_app[,-c(1)])
  colnames(tdata) <- vars

  tdata$station_eu_code <- rownames(tdata)
  
  dir.create(glue("contributo/{cod_reg}"), showWarnings = FALSE)
  write_csv(tdata, file = glue("contributo/{cod_reg}/contributo_lock_{pltnt}.csv"))
}
