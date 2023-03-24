{
  library(readr)
  library(ggplot2)
  library(ggthemes)
  library(reshape2)
  library(dplyr)
  library(glue)
  
  setwd("~/R/pulvirus/presentazione/contributo")
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

df <- read_csv(glue("contributi_lock_{pltnt}.csv"))

names(df)[1:4] <- c("Marzo", "Aprile", "Maggio", "Giugno")

ifelse(
  pltnt == "no2", frml <- expression(paste("Contributo lockdown - concentrazione NO"["2"])),
  ifelse(
    pltnt == "pm10", frml <- expression(paste("Contributo lockdown - concentrazione PM"["10"]))
  )
)

inner_join(df, stazioni, by = c("station_eu_code")) %>% 
  select(c(station_eu_code, tipo_s, "Marzo", "Aprile", "Maggio", "Giugno")) %>% 
  melt(id.vars = c("station_eu_code", "tipo_s")) %>% 
  ggplot(aes(value, group = variable, fill = variable)) +
  geom_histogram(bins = 80, alpha = 0.5) +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.25)) +
  facet_wrap(vars(tipo_s)) + 
  ggtitle(frml) +
  theme_pulvirus()

ggsave(glue("graf_{pltnt}.png"), width = 8)

