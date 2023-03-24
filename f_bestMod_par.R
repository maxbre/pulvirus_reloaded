# function: bestMod ####
bestMod <- function(mod.res, bic = TRUE) {
  log_print( sprintf("modelli tra cui scegliere: %s", length(mod.res) ), hide_notes = TRUE)
  
  if(bic == TRUE) {
    ics <- map(mod.res, \(m) BIC({m}) )
    
  }else{
    ics <- map(mod.res, \(m) AIC({m}) )
  }
  
  ics %>% which.min() %>% as.numeric() -> min.ics
  
  log_print("Modello migliore: ", hide_notes = TRUE)
  log_print(mod.res[[min.ics]][["formula"]], hide_notes = TRUE)
  log_print(paste("Indice: ", min.ics), hide_notes = TRUE)
  
  mod.res[[min.ics]][["var.summary"]] %>% names() -> v.min
  
  if(bic == TRUE) {
    return(list( BIC(mod.res[[min.ics]]), v.min[-c(1:4)], ics))
  }else{
    return(list( AIC(mod.res[[min.ics]]), v.min[-c(1:4)], ics))
  }
}
