# grafici contributo lockdown ####
{
  library(readr)
  library(ggplot2)
  library(ggthemes)
  library(reshape2)
  library(dplyr)
  library(glue)
  
  setwd("~/R/pulvirus_reloaded")
  rm(list = ls())
}


datiInquinanti::stazioniAria %>%
  mutate(tipo_s = case_when(
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
  )) -> stazioni

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

pltnt <- "no2"

df <- read_csv(glue("contributo/contributo_lock_{pltnt}.csv"))

names(df)[1:4] <- c("Marzo", "Aprile", "Maggio", "Giugno")

ifelse(
  pltnt == "no2", frml <- expression(paste("Contributo lockdown - concentrazione NO"["2"])),
  ifelse(
    pltnt == "pm10", frml <- expression(paste("Contributo lockdown - concentrazione PM"["10"])),
    ifelse(
      pltnt == "nox", frml <- expression(paste("Contributo lockdown - concentrazione NO"["x"]))
    )
  )
)


inner_join(df, stazioni, by = c("station_eu_code") ) %>%
  select( c(station_eu_code, tipo_s, "Marzo", "Aprile", "Maggio", "Giugno") ) %>%
  melt( id.vars = c("station_eu_code", "tipo_s") ) %>%
  ggplot(aes(value, group = variable, fill = variable)) +
  geom_histogram(bins = 20, alpha = 0.5) +
  scale_fill_brewer(palette = "RdYlGn") +
  # scale_x_continuous(breaks = seq(-1, 1, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 50, by = 2)) +
  facet_wrap(vars(tipo_s)) +
  ggtitle(frml) +
  theme_pulvirus()


melt(df) %>% ggplot(aes(y = value, fill = variable)) + 
  geom_histogram(binwidth = 0.2) + 
  coord_flip() + 
  facet_grid(~variable) + 
  theme_pulvirus() +
  theme(legend.position  = "none")

