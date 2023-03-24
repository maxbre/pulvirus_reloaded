# function: bestMod ####
bestMod <- function(mod.res) {
  log_print( sprintf("modelli tra cui scegliere: %s", length(mod.res) ), hide_notes = TRUE)
  aics <- map(mod.res, \(m) AIC({m}) )
  aics %>% which.min() %>% as.numeric() -> min.aic
  
  log_print("Modello migliore: ", hide_notes = TRUE)
  log_print(mod.res[[min.aic]][["formula"]], hide_notes = TRUE)
  log_print(paste("Indice: ", min.aic), hide_notes = TRUE)
  
  mod.res[[min.aic]][["var.summary"]] %>% names() -> v.min
  
  return(list( AIC(mod.res[[min.aic]]), v.min[-c(1:4)], aics))
}

