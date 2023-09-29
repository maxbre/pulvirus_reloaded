# reportistica ####

# init ####
{
  library(RColorBrewer)
  library(datiInquinanti)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  library(glue)
  library(janitor)
  library(knitr)
  library(pander)
  library(psych)
  library(purrr)
  library(readr)
  library(reshape2)
  library(sf)
  library(skimr)
  library(classInt)
  library(tibble)
}

# setwd("~/R/pulvirus_reloaded/report")

# aggiungo la classificazione 
stazioniAria <- stazioniAria %>%
  mutate(
    tipoS = case_when(
      zona_tipo == "FU" ~ "Fondo urbano/suburbano",
      zona_tipo == "FS" ~ "Fondo urbano/suburbano",
      zona_tipo == "TU" ~ "Traffico",
      zona_tipo == "TS" ~ "Traffico",
      tipo_zona == "R" ~ "Rurale",
      tipo_zona == "R-nearcity" ~ "Rurale",
      tipo_zona == "R-regional" ~ "Rurale",
      tipo_zona == "R-remote" ~ "Rurale",
      tipo_stazione == "I" ~ "Industriale",
      tipo_stazione == "F/I" ~ "Industriale",
    )
  )


if(!exists("ita_reg")) {
  ita_reg <- st_read("/home/rmorelli/R/corso/dati-istat/Limiti01012019_g/Reg01012019_g/Reg01012019_g_WGS84.shp", 
                     quiet = TRUE)
}

pulvirus_names <- list(
  'rmse_20' = 'RMSE 20',
  'rmse_80' = 'RMSE 80', 
  'rsq_20' = 'RSQ 20', 
  'rsq_80' = 'RSQ 80', 
  'FAC2' = 'FAC2',
  'FB' = 'FB',
  'NMSE' = 'NMSE'
)

theme_pulvirus <- function() {
  theme_grey() %+replace%    #replace elements we want to change
    theme(
      plot.title = element_text(size = 20, face = 'bold', hjust = 0, vjust = 2),
      plot.subtitle = element_text(size = 14), 
      plot.caption = element_text(size = 9, hjust = 1),
      axis.title = element_blank(),
      axis.text = element_text(size = 9),
      legend.position = "right",
      legend.title = element_blank()
    )
}

pulvirus_labeller <- function(variable, value){
  return(pulvirus_names[value])
}

serievalide <- function(pltnt) {
  df <- read_delim(glue::glue("~/R/pulvirus_reloaded/validazione/validazione_{pltnt}.csv"), delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  df[(df[["FAC2"]] >= 0.8 & 
        df[["rsq_80"]] >= 0.5 &
        between( df[["rsq_20"]] / df[["rsq_80"]], 0.8, 1.2) &  
        between( df[["rmse_20"]] / df[["rmse_80"]], 0.5, 1.5) & 
        df[["FB"]] <= 0.5 & 
        df[["NMSE"]] <= 0.5),] %>% select(station_eu_code) -> ss
  
  return(ss)
}
# serievalide("no2")

getdesc <- function(pltnt, cod_reg, formato_out = "markdown") {
  df <- read_delim(glue("~/R/pulvirus_reloaded/validazione/{cod_reg}/validazione_{pltnt}.csv"), 
                   ";", 
                   escape_double = FALSE, 
                   trim_ws = TRUE, 
                   show_col_types = FALSE
                   )
  
  tit <- case_when(
    pltnt == "pm25" ~ "PM~25~",
    pltnt == "pm10" ~ "PM~10~",
    pltnt == "co" ~ "CO",
    pltnt == "o3" ~ "O~3~",
    pltnt == "nox" ~ "NOx",
    pltnt == "no2" ~ "NO~2~"
  ) 
  
  pulvirus_names <- list(
    'rmse_20' = 'RMSE 20',
    'rmse_80' = 'RMSE 80', 
    'rsq_20' = 'RSQ 20', 
    'rsq_80' = 'RSQ 80', 
    'FAC2' = 'FAC2'
  )
  
  df %>% 
    select(-c(station_eu_code)) %>%
    psych::describe(skew = FALSE) -> stats
  
  rownames(stats) <- c('RMSE 20', 'RMSE 80', 'MSE 20', 'MSE 80', 'RSQ 20', 'RSQ 80',  'FAC2', 'FB', 'NMSE')
  colnames(stats) <- c("vars", "n", "Media", "Std. dev", "Min", "Max", "Range", "Std. err")
  
  stats %>% select(-c(vars, n)) %>%
    kable(format = formato_out, 
                 digits = 3, 
                 caption = glue("Indici statistici di validazione e prestazioni {tit}")
    )
}
# getdesc("no2", "3")

riassumi <- function(pltnt, tipo, cod_reg) {
  df <- read_delim(glue::glue("~/R/pulvirus_reloaded/validazione/{cod_reg}/validazione_{pltnt}.csv"), 
                   delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE, 
                   show_col_types = FALSE)
  
  
  cl <- read_csv(glue::glue("~/R/pulvirus_reloaded/contributo/{cod_reg}/contributo_lock_{pltnt}.csv"), 
                 show_col_types = FALSE)
  
  ss <- serievalide(pltnt)
  
  cl[ cl[["station_eu_code"]] %in% ss[["station_eu_code"]], ] %>% 
    inner_join(stazioniAria, by = c("station_eu_code")) -> wdf
  
  names(wdf)[1:4] <- c("Marzo","Aprile", "Maggio", "Giugno")
  
  tipo_new <- gsub("/", "", tipo)

  wdf %>% 
    select(c(station_eu_code, st_x, st_y, tipoS, "Marzo","Aprile", "Maggio", "Giugno")) %>% 
    reshape2::melt(id.vars = c("station_eu_code", "st_x", "st_y", "tipoS")) -> t
  
  tdf <- mutate(t, Contributo = cut(value, breaks = c(-20,-10,-5, 0, 10, 11)))
  
  tdf %>%
    select(tipoS, variable, value) %>%
    filter(tipoS == tipo) %>% 
    dplyr::group_by(tipoS, variable) %>%
    skim() %>%
    select(variable, numeric.mean, numeric.p25, numeric.p50, numeric.p75, numeric.sd) %>%
    knitr::kable(caption = tipo, digits = 6, format = "markdown",
                 col.names = c("Mese", "Media", "25th", "Mediana", "75th", "Std. dev") 
    )
}
# riassumi("no2", "Traffico", 3)

riassumiBoxplot <- function(pltnt, tipo, cod_reg) {
  df <- read_delim(glue::glue("~/R/pulvirus_reloaded/validazione/{cod_reg}/validazione_{pltnt}.csv"), 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                   show_col_types = FALSE)
  
  cl <- read_csv(glue::glue("~/R/pulvirus_reloaded/contributo/{cod_reg}/contributo_lock_{pltnt}.csv"), 
                 show_col_types = FALSE)
  
  ss <- serievalide(pltnt)
  cl[cl[["station_eu_code"]] %in% ss[["station_eu_code"]],] %>% 
    inner_join(stazioniAria, by = c("station_eu_code")) -> wdf
  
  # wdf_sf <- mutate(wdf, Contributo = cut(lockL3, breaks = c(-20, -10, -5, 0, 10)))
  
  names(wdf)[1:4] <- c("Marzo","Aprile", "Maggio", "Giugno")
  
  wdf %>% filter(tipoS == tipo) %>% 
    select(c(station_eu_code, "Marzo","Aprile", "Maggio", "Giugno")) %>% 
    reshape2::melt(id.vars = c("station_eu_code")) -> t
  
  tdf <- mutate(t, Contributo = cut(value, breaks = c(-20, -10, -5, 0, 10, 11)))
  
  titoli <- list("pm25" = bquote("Contributo lockdown" ~ PM[25]), 
                 "pm10" = bquote("Contributo lockdown" ~ PM[10]), 
                 "no2" = bquote("Contributo lockdown" ~ NO[2]),
                 "o3" = bquote("Contributo lockdown" ~ O[3])
  )
  
  ggplot(tdf) +
    geom_boxplot(aes(y = value, group = variable, fill = variable)) + 
    facet_wrap(~variable, nrow = 1) + ylab("Concentrazione") +
    theme(axis.text.x = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
    ggtitle(titoli[[pltnt]], subtitle = tipo)
}
# riassumiBoxplot("no2", "Fondo urbano/suburbano", "12")

getbxplt <- function(pltnt, vars, cod_reg, val = TRUE) {
  df <- read_delim(glue("~/R/pulvirus_reloaded/validazione/{cod_reg}/validazione_{pltnt}.csv"), 
                   ";", 
                   escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

  if(val == TRUE) {
    df[ (df[["FAC2"]] >= 0.8 & 
           df[["rsq_80"]] >= 0.5 &
           between( df[["rsq_20"]] / df[["rsq_80"]], 0.8, 1.2) &  
           between( df[["rmse_20"]] / df[["rmse_80"]], 0.5, 1.5) & 
           df[["FB"]] <= 0.5 & 
           df[["NMSE"]] <= 0.5), ] -> df
  }
  
  df %>% 
    select(c("station_eu_code", vars)) %>%
    melt(id.vars = c("station_eu_code")) %>%
    ggplot(aes(x = variable, y = value, fill = variable)) +
    geom_boxplot(position = "identity", size = 0.5, outlier.size = 0.2) +
    scale_fill_brewer(palette = "Spectral") +
    facet_wrap(~variable, ncol = 7, scales = "free", labeller = pulvirus_labeller) +
    theme(strip.text.x = element_text(size = 7), 
          legend.position = "none",
          # axis.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 6),
          axis.title = (element_text(size = 6))
    ) -> g
  if(pltnt == "pm10") {
    # g <- g + xlab(bquote("Validazione modelli" ~ PM[10]))
    g <- g + xlab(("Validazione modelli" ))
  }
  if(pltnt == "no2") {
    # g <- g + xlab(bquote("Validazione modelli" ~ NO[2])) 
    g <- g + xlab(("Validazione modelli" )) 
  }
  
  return(g)
}
# getbxplt("no2", c("rmse_20", "rmse_80", "rsq_20", "rsq_80"), 12, TRUE)
# getbxplt("pm10", c("rmse_20", "rmse_80", "rsq_20", "rsq_80", "FAC2", "FB", "NMSE"))

mappe <- function(pltnt, cod_reg, tipo = FALSE) {
  cl <- read_csv(glue::glue("~/R/pulvirus_reloaded/contributo/{cod_reg}/contributo_lock_{pltnt}.csv"),
                 show_col_types = FALSE)
  
  ss <- serievalide(pltnt)
  
  cl[cl[["station_eu_code"]] %in% ss[["station_eu_code"]],] %>% 
    inner_join(stazioniAria, by = c("station_eu_code")) -> wdf
  
  titoli <- list("pm25" = bquote("Contributo lockdown - concentrazione" ~ PM[25]), 
                 "pm10" = bquote("Contributo lockdown - concentrazione" ~ PM[10]), 
                 "no2" = bquote("Contributo lockdown - concentrazione" ~ NO[2]),
                 "o3" = bquote("Contributo lockdown - concentrazione" ~ O[3])
  )
  # titoli[["no2"]]
  
  names(wdf)[1:4] <- c("Marzo","Aprile", "Maggio", "Giugno")
  
  wdf %>% 
    select(
      c(station_eu_code, st_x, st_y, tipoS, "Marzo","Aprile", "Maggio", "Giugno")
    ) %>% 
    reshape2::melt(id.vars = c("station_eu_code", "st_x", "st_y", "tipoS")) -> t
  
  brks <- classIntervals(t$value, dataPrecision = 1, n = 6, style = "pretty")
  
  brks <- classIntervals(t$value, dataPrecision = 1, n = 6, style = "pretty")$brks
  
  wdf <- mutate(t, Contributo = cut(value, breaks = brks) )
  lng <- length(brks)

  if(tipo == FALSE) {
    pts <- 1
  }else{
    pts <- 0.7
  }
  
  filter(ita_reg, COD_REG == cod_reg) %>% 
    ggplot() +
    geom_sf(fill = "grey70", size = 0.1, color = "grey99") +
    geom_point(data = wdf, 
               aes(x = st_x, y = st_y, color = factor(Contributo)), na.rm = FALSE, size = 2) +
    coord_sf(crs = "+init=epsg:4326") -> g
  
  if(pltnt == "co") {
    lgnd <- bquote(~mg/m^3)
  }else{
    lgnd <- bquote(~mu~g/m^3)
  }
  g <- g + scale_colour_brewer(na.value = "grey35", palette = "RdYlGn",
                               guide = "legend", direction = -1,
                               name = lgnd)
  
  g <- g + scale_shape_manual(values = rep(19, lng), na.value = 1.5, guide = "none") +
    scale_size_manual(values = rep(1, lng), na.value = 0.1, guide = "none")
  
  g + 
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(size = 10),
      legend.key.size = unit(24, "point"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.key = element_rect(colour = NA, fill = "grey70"),
      legend.position = "right",
      plot.title = element_text(size = 20)
    ) -> g
  
  g <- g + 
    guides(colour = guide_legend(override.aes = list(size = 4)))
  
  if(tipo == TRUE) {
    g <- g + facet_grid(vars(tipoS), vars(variable)) 
  }else{
    g <- g + facet_wrap(vars(variable), nrow = 2)     
  }
  
  g <- g + ggtitle(titoli[[pltnt]])
  return(g)
}
# mappe("no2", 3, TRUE)
# mappe("no2", 12)

contributoLock <- function(pltnt, cod_reg, perc = FALSE) {
  stazioniAria -> stazioni

  cl <- read_csv(glue("~/R/pulvirus_reloaded/contributo/{cod_reg}/contributo_lock_{pltnt}.csv"), 
                   show_col_types = FALSE)
  
  df <- read_delim(glue::glue("~/R/pulvirus_reloaded/validazione/{cod_reg}/validazione_{pltnt}.csv"), 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  ss <- serievalide(pltnt)
  
  names(cl)[1:4] <- c("Marzo", "Aprile", "Maggio", "Giugno")
  sub <- "Contributo lockdown - concentrazione"
  

  titoli <- list("pm25" = bquote("Contributo lockdown - concentrazione" ~ PM[25]), 
                 "pm10" = bquote("Contributo lockdown - concentrazione" ~ PM[10]), 
                 "no2" = bquote("Contributo lockdown - concentrazione" ~ NO[2]),
                 "o3" = bquote("Contributo lockdown - concentrazione" ~ O[3])
  )
  
  if(pltnt == "co") {
    cl <- filter(cl, station_eu_code != "IT2146A")
  }
  
  cl[cl[["station_eu_code"]] %in% ss[["station_eu_code"]],] %>%
    inner_join(stazioni, by = c("station_eu_code")) %>%
    select(c(station_eu_code, tipoS, "Marzo", "Aprile", "Maggio", "Giugno")) %>%
    melt(id.vars = c("station_eu_code", "tipoS")) %>%
    ggplot(aes(value, group = variable, fill = variable)) +
    geom_histogram(alpha = 0.6, bins = 30, size = 0.5) +
    scale_fill_brewer(palette = "RdYlGn") -> g
  
  if(perc == TRUE) {
    g <- g + scale_x_continuous(labels = scales::percent) 
  }
  
  g +
    facet_wrap(vars(tipoS)) + 
    theme(legend.position = "right", 
          legend.title = element_blank(), 
          plot.title = element_text(size = 11)) + 
    ylab("n. stazioni") + 
    xlab("") + ggtitle(titoli[[pltnt]])
  
  return(g)
}
# contributoLock("no2", 3)

bootstrapmodelli <- function(pltnt, cod_reg) {
  load(glue("~/R/pulvirus_reloaded/incertezza/{cod_reg}/ic_{pltnt}.RData"))
  
  stats <- function(x) {
    # Mean <- mean(x, na.rm = TRUE)
    SD <- sd(x, na.rm = TRUE)
    MAD <- mad(x, na.rm = TRUE)
    return(c(SD = SD, MAD = MAD))
  }
  
  my_list %>%
    map( ~ t( 
      apply(.x[, 2:30], 1, stats) ) %>% 
        as.data.frame() 
    ) -> out_list
  
  do.call(rbind, out_list %>% map(~.x[,1])) %>% 
    as.data.frame() %>% 
    setNames(c("Marzo", "Aprile", "Maggio", "Giugno")) %>% 
    reshape2::melt() %>% 
    ggplot(aes(variable, value, fill = variable)) + 
    geom_boxplot() + 
    theme(legend.position = "none") + xlab("") + ylab("Standard deviation")
}
# bootstrapmodelli("no2", 3)

scarto <- function(perc = FALSE) {
  pltnts <- c("no2", "pm10", "pm25", "co", "o3", "nox")
  my_list <- list()
  
  for(pltnt in pltnts) {
    cat(pltnt, "\n\n")
    df <- read_delim(glue::glue("~/R/pulvirus_reloaded/validazione/validazione_{pltnt}.csv"), 
                     delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE, 
                     show_col_types = FALSE )
    if(perc == TRUE) {
      cl <- read_csv(glue::glue("~/R/pulvirus_reloaded/contributo/contributo_lock_{pltnt}_perc.csv"), 
                     show_col_types = FALSE)
    }else{
      cl <- read_csv(glue::glue("~/R/pulvirus_reloaded/contributo/contributo_lock_{pltnt}.csv"), 
                     show_col_types = FALSE)
    }
    
    ss <- serievalide(pltnt)
    
    my_list[[pltnt]] <- c(dim(ss)[1], dim(cl)[1] )
  }
  
  as.data.frame(my_list) -> df
  # scarto()[1,] / scarto()[2,]*100
  return(df)
}
# scarto()
