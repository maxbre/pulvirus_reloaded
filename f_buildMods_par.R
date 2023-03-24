## function: buildMods ####
buildMods <- function(backward = FALSE) {
  ICS <- get("ICS", envir = .GlobalEnv)
  
  v_cappa <- length(unique(df$reporting_year))
  v_meteo <- get("v_meteo", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  # costruisce le "spline testuali"
  makeSpline <- function(v) {
    return(lapply(v, function(x) paste0("s(", x, ")") ))
  }
  
  if( length(ICS) > 1 & backward == TRUE ) {
    # Si elimina la variabile (n-1), si bloccano le variabili da 1 a (n-2) e
    # quella n e poi si sceglie tra le var rimanenti quella che minimizza l'AIC
    log_print("Modelli backward: ", hide_notes = TRUE)
    
    v_left <- v_meteo[!v_meteo %in% c(names(ICS), v_dead)]
    
    # se non ci sono più variabili usciamo
    if(length(v_left) < 2)
      return(NULL)
    
    s_left <- makeSpline(v_left)
    
    # costruisco le "spline" con le variabili in v_fixed tranne la n-1
    c_fixed <- lapply( c(names(ICS)[-c(length(ICS)-1)], v_dead), function(x) rep(x, length(v_left)) )
    
    s_fixed <- makeSpline(c_fixed)
    
    s_fixed <- do.call(cbind, s_fixed)
  }else{
    log_print("Modelli NON backward: ", hide_notes = TRUE)
    
    # le variabili non scelte (v_fixed) e non scartate (v_dead)
    v_left <- v_meteo[!v_meteo %in% c(names(ICS), v_dead)]
    if(length(v_left) == 0) {
      return(NULL)
    }
    
    # le combinazioni delle restanti
    s_left <- makeSpline(v_left)
    
    c_fixed <- lapply(names(ICS), function(x) rep(x, length(s_left)))
    
    s_fixed <- makeSpline(c_fixed)
    
    s_fixed <- do.call(cbind, s_fixed)
  }
  
  x <- cbind(s_fixed, s_left)
  z <- data.frame(mod = apply(x, 1, paste0, collapse = " + ")) # per ogni riga
  
  # w conterrà le stringhe dei modelli
  w <- lapply(z[,  ncol(z)], function(x) 
    paste0("gam(log(value) ~ weekday + lock + s(mese, bs='cc', k=12) + s(jd, k=", v_cappa, ") + ", x, ", data = df)"))
  
  return(w)
}