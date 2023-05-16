## function: sceltaVar ####
sceltaVar <- function(bic) {
  ICS <- get("ICS", envir = .GlobalEnv)
  v_meteo <- get("v_meteo", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  pltnt <- get("pltnt", envir = .GlobalEnv)
  
  w <- buildMods(backward = FALSE) # costruisce le stringhe dei modelli
  if(is.null(w)) {
    return(NULL)
  }
  
  log_print(sprintf("---------- START: %s", "##################"), hide_notes = TRUE)
  
  # conterrà gli oggetti GAM calcolati sulle stazioni
  models <- map(w, \(y) eval(parse(text = y)))
  
  aicVar <- bestMod(models) # AIC del modello migliore
  
  # log_print(unlist(w), hide_notes = TRUE)
  # log_print(unlist(aicVar[[3]]) , hide_notes = TRUE)
  log_print(cbind(unlist(w), unlist(aicVar[[3]])) , hide_notes = TRUE)
  
  # una lista di appoggio da concatenare in ICS
  tmp <- list()
  tmp[[last(aicVar[[2]])]] <- c(tmp, aicVar[[1]])
  
  # Log delle risultanze
  log_print(sprintf("Variabile scelta: %s - AIC %s", last(aicVar[[2]]), round( as.numeric(aicVar[1]), 2)), hide_notes = TRUE )
  
  # Salvataggio lista con gli AIC dei modelli elaborati finora
  ICS <- c(ICS, tmp)
  assign("ICS", ICS, envir = .GlobalEnv)
  
  log_print(sprintf("Variabili in ICS: %s", length(ICS) ), hide_notes = TRUE)
  
  if(length(ICS) < 2) {
    sceltaVar()
    return("NA")
  }
  
  log_print(sprintf("Gli ICS %s ----- %s", length(ICS) , length(ICS)-1 ))
  if( ICS[[length(ICS)]][[1]] < ICS[[length(ICS)-1]][[1]] ) {
    log_print("Il modello N è migliore del precedente, verifica del backward", hide_notes = TRUE)
    
    w <- buildMods(backward = TRUE)
    
    if( !is.null(w) || length(w == 0) ) {
      models <- map(w, \(y) eval(parse(text = y)))
      aicBack <- bestMod(models)  # AIC del backward
      
      log_print(sprintf("Dati backward:  %s - %s", last(aicBack[[2]]), aicBack[[1]]), hide_notes = TRUE)
      
      # log_print(unlist(w), hide_notes = TRUE)
      # log_print(unlist(aicBack[[3]]), hide_notes = TRUE)
      log_print( data.frame(unlist(w), unlist(aicBack[[3]])), hide_notes = TRUE )
      
      if(is.null(aicBack)) {
        return(NA)
      }
      
      if( aicBack[[1]] < ICS[[length(ICS)]][[1]] ) {
        n_1 <- names(ICS)[-c(length(names(ICS))-1 )]
        
        backwardVars <- c(n_1, last(aicBack[[2]]))
        
        q <- cor(df %>% select(all_of(backwardVars)), use = "pairwise.complete.obs") %>% 
          data.frame()
        
        # check di correlazione sulle variabili del backward
        if( all(abs(q[last(aicBack[[2]]), 1:(ncol(q)-1)]) < 0.7) ) {
          
          ICS <- ICS[-c(length(ICS)-1)]
          ICS[[last(aicBack[[2]])]] <- list(aicBack[[1]])
          assign("ICS", ICS, envir = .GlobalEnv)
          
          assign("v_meteo", v_meteo[!v_meteo %in% c(names(ICS), v_dead)], envir = .GlobalEnv)
          
          log_print("Scelgo il BACKWARD", hide_notes = TRUE)
          # sceltaVar()
        }else{
          # metto la variabile nelle dead perché troppo correlata
          log_print(sprintf("Variabile altamente correlata: %s", last(aicBack[[2]])), hide_notes = TRUE)
          log_print(q[last(aicBack[[2]]), 1:(ncol(q)-1)] )
          log_print("Scelgo il modello N ", hide_notes = TRUE)
          
          assign("v_dead", c(v_dead, last(aicBack[[2]])), envir = .GlobalEnv)
          assign("v_meteo", v_meteo[!v_meteo %in% c(names(ICS), v_dead)], envir = .GlobalEnv)
          ## WARNING: qui dovremmo verificare se in ICS le variabili sono ancora correlate ####
        }
        sceltaVar()
        return("=>>> the end")
      }
      
      # se siamo qui...
      log_print("Il backward non è migliore. \nVerifica della correlazione per mod(N)", hide_notes = TRUE)
      log_print(sprintf("Modello N: %s", paste(names(ICS), collapse = " - " )), hide_notes = TRUE)
      
      # sono qui perché il modello backward non migliora quindi controllo la correlazione del modello N
      q <- cor(df %>% select(names(ICS)), use = "pairwise.complete.obs") %>% data.frame()
      
      if( all(abs(q[last(names(ICS)), 1:(ncol(q)-1)]) < 0.7) ) {
        log_print("Scelgo il modello N", hide_notes = TRUE)
        
        # tolgo dalla lista delle variabili quelle del modello scelto visto che non sono correlate
        assign("v_meteo", v_meteo[!v_meteo %in% c(names(ICS), v_dead)], envir = .GlobalEnv)
      }else{
        # devo eliminare la variabile perché troppo correlata
        log_print("Variabile correlata > 0.7", hide_notes = TRUE)
        log_print(q[last(names(q)), 1:(ncol(q)-1)], hide_notes = TRUE)
        
        assign("v_dead", c(v_dead, c(last(names(ICS))) ), envir = .GlobalEnv)
        assign("v_meteo", v_meteo[!v_meteo %in% c(names(ICS), v_dead) ], envir = .GlobalEnv)
        
        ICS[[last(names(ICS))]] <- NULL
        assign("ICS", ICS, envir = .GlobalEnv)
        
        log_print("Scelgo il modello N-1", hide_notes = TRUE)
      }
      sceltaVar()
      return("=>>> the end")
      
    }else{
      log_print("Fine per scelta MODELLO iniziale", hide_notes = TRUE)
      
      # qui devo togliere l'ultima variabile
      assign("v_dead", c(v_dead, c(last(names(ICS)))), envir = .GlobalEnv)
      ICS[[last(names(ICS))]] <- NULL
      
      assign("ICS", ICS, envir = .GlobalEnv)
      assign("v_meteo", v_meteo[!v_meteo %in% c(names(ICS), v_dead)], envir = .GlobalEnv)

      return("=>>> the end")
    }
  }
  
  
  # se sono qui devo eliminare l'ultia variabile con AIC non migliore del modello precedente
  ICS[[length(ICS)]] <- NULL
  assign("ICS", ICS, envir = .GlobalEnv)
  
  assign("v_meteo", v_meteo[!v_meteo %in% c(names(ICS), v_dead)], envir = .GlobalEnv)
  
  log_print("Fine per scelta MODELLO", hide_notes = TRUE)
  log_print(paste(names(ICS), collapse = " + "), hide_notes = TRUE)
  
}