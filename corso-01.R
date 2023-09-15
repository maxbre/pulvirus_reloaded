## librerie ####
library(dplyr)
# library(glue)
# library(lubridate)
library(naniar)
library(purrr)
library(readr)
library(readxl)
library(reshape2)
library(tidyverse)
library(RPostgreSQL)
library(writexl)


setwd("~/R/pulvirus_reloaded")


## list.files ####
list.files(path = ".", pattern = "*.csv") # bla bla bla b

list.files(path = ".", pattern = "*.csv", recursive = TRUE)

list.files(path = ".", pattern = "^2020_Dati_Lazio.*", recursive = TRUE)

list.files(path = "~/R/pulvirus/presentazione/dati-lazio", pattern = "*.csv")

setwd("~/R/pulvirus/presentazione/dati-lazio")

list.files(pattern = "*.csv")

file.dati <- list.files(pattern = "*.csv")

file.dati[1]

## read_csv ####
tryCatch({
  read_csv(file.dati[1])
},
error = function(cond) {
  print("Attenzione...")
})

# f <- read_csv(file.dati[1])
f <- read_csv(file.dati[1], locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)

head(f, n = 1)

## read_delim ####
f <- read_csv(file.dati[1], locale = locale(encoding = "ISO-8859-1"))

f <- read_delim(file.dati[1], delim = ";", locale = locale(encoding = "ISO-8859-1"))

f <- read_delim(file.dati[1], delim = ";", locale = locale(encoding = "ISO-8859-1"), skip = 2)

f <- read_delim(file.dati[1], delim = ";", locale = locale(encoding = "ISO-8859-1", decimal_mark = ","), 
                col_types = cols(`...1` = col_date(format = "%d/%m/%Y")), 
                skip = 2)

f <- read_delim(file.dati[1], delim = ";", col_types = cols(...1 = col_date(format = "%d/%m/%Y"), 
                                                            ...2 = col_skip(),
                                                            `Cod. EU Stazione` = col_skip()), 
                locale = locale(encoding = "ISO-8859-1", decimal_mark = ","), 
                trim_ws = TRUE, skip = 2)

f <- read.csv(file.dati[1], sep = ";", fileEncoding = "ISO-8859-1", skip = 2, dec = ",")

f <- f[, -c(2,3)]

## caricamento: lapply ####
data <- lapply(file.dati, function(x) 
  read_delim(x, delim = ";", col_types = cols(...1 = col_date(format = "%d/%m/%Y"), 
                                              ...2 = col_skip(),
                                              `Cod. EU Stazione` = col_skip()), 
             locale = locale(encoding = "ISO-8859-1", decimal_mark = ","), 
             trim_ws = TRUE, skip = 2)
  )

## oggetti nel global ennv con purrr ####
file.dati %>%
  map(function(fname){ # iterate through each sheet name
    assign(x = fname,
           value = read_delim(fname, locale = locale(encoding = "ISO-8859-1", decimal_mark = ",")),
           envir = .GlobalEnv)
  })


for (i in seq_along(data)) {
  colnames(data[[i]])[1]  <- "data"
}

## map ####
map(file.dati, 
    function(x) str_split(x, pattern = "_") %>% 
      unlist()
) %>% 
  map(\(x) x[4] %>% unlist() )

pltnt <- map(file.dati, 
             function(x) str_split(x, pattern = "_") %>% unlist()
) %>% map(\(x) gsub(".csv", "", x[4])) %>% unlist()

names(data) <- c(pltnt)

m.data <- map(data, function(df) {
  df %>% 
    group_by(data) %>% 
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
    )
})


names(m.data)

setwd("~/R/pulvirus/presentazione")

m.data %>%
  names(.) %>%
  walk(~write_csv(m.data[[.]], glue::glue("tmp/{.}.csv")))

## imap ####
imap(m.data, ~write.csv(melt(.x, id.vars = c("data")), 
                        file = glue::glue("tmp/{.y}.csv"), row.names = FALSE))

imap(m.data, ~write.csv( 
  melt(.x, id.vars = c("data")) %>% setNames(c("data", "station_eu_code", "value")),  
  file = glue::glue("dati-lazio/per-inquinante/{.y}.csv"), row.names = FALSE))

## pgsql ####
# tryCatch({
#   drv <- dbDriver("PostgreSQL")
#   print("Connecting to Database")
#   connec <- dbConnect(drv, 
#                       dbname = dsn_database,
#                       host = dsn_hostname, 
#                       port = dsn_port,
#                       user = dsn_uid, 
#                       password = dsn_pwd)
#   print("Database Connected!")
# },
# error = function(cond) {
#   print("Unable to connect to Database.")
# })

## storico ####
no2_2013_2019 <- read_csv("storico/NO2_2013-2019.csv")
no2_2020 <- read_csv("dati-lazio/per-inquinante/NO2.csv")

## stazioni ####
stazioni <- read_csv("stazioni/stazioni-metadati.csv")

staz_lazio <- filter(stazioni, region_id == 12) %>% select(station_eu_code, `nome stazione`, st_x, st_y)

no2Lazio <- no2_2013_2019 %>% filter(station_eu_code %in% staz_lazio$station_eu_code)

no2_2020 <- no2_2020 %>% mutate(reporting_year = format(data, "%Y"))
no2_2020 <- no2_2020 %>% mutate(reporting_year = lubridate::year(data))

names(no2Lazio)
names(no2_2020)

select(no2Lazio, c("reporting_year", "date", "station_eu_code", "value"))
select(no2_2020, c("reporting_year", "data", "station_eu_code", "value"))

no2_2013_2020 <- bind_rows(
  select(no2Lazio, c("reporting_year", "date", "station_eu_code", "value")),
  select(no2_2020, c("reporting_year", "data", "station_eu_code", "value") %>% 
           setNames(c("reporting_year", "date", "station_eu_code", "value")))
)

rm(f, file.dati, no2_2020, no2Lazio, no2_2013_2019, data, stazioni)

## controllo dati ####
# - una serie e'valida se lunga almeno 5 anni 
# - un mese e' valido se contiene almeno il 75% di dati validi (senza ulteriori sulla continuit dei blocchi dei dati mancanti all'interno del mese)
# - un anno e' valido se contiene tutte le stagioni valide (almeno due mesi validi per stagione)
# - le serie devono avere tutti i mesi validi nel 2020
                            
serie <- as_tibble_col( seq(lubridate::ymd('2016-01-01'), lubridate::ymd('2020-06-30'), by = 'days'), column_name = "date" )

no2_2013_2020 %>% 
  mutate(
    mese = lubridate::month(date),
    stagione =
      case_when(
        format(date, "%m") %in% c("01", "02", "12") ~ "inverno",
        format(date, "%m") %in% c("03", "04", "05") ~ "primavera",
        format(date, "%m") %in% c("06", "07", "08") ~ "estate",
        TRUE ~ "autunno"
      ) 
  ) -> no2_2013_2020

no2 <- left_join(serie, no2_2013_2020, by = "date")
# no2 <- full_join(serie, no2_2013_2020, by = "date")

no2 %>% 
  group_by(station_eu_code, reporting_year) %>% 
  summarise(doy = n()) %>% View()

# calcolo i mesi validi ####

no2 %>% 
  split(.$station_eu_code) %>% 
  map(\(df) {
    df %>% 
      group_by(reporting_year, mese, stagione) %>%
      miss_var_summary() %>% 
      filter(variable == "value" ) %>%
      mutate(flag_mese = ifelse(pct_miss > 25, 0, 1) )
  }) -> mc.mesi


# calcolo stagioni valide ####
mc.mesi %>% 
  map(\(df) {
    df %>% 
      group_by(reporting_year, stagione) %>% 
      summarise(n = sum(flag_mese), .groups = 'drop') %>% 
      mutate(flag_stagione = ifelse(n > 1, 1, 0))    
  }) -> mc.stagioni
 
# calcolo anni validi ####
mc.stagioni %>% 
  map(\(df) {
    df %>% 
      select(reporting_year, flag_stagione) %>% 
      group_by(reporting_year) %>% 
      summarise(n = sum(flag_stagione),  .groups = 'drop') %>% 
      mutate(flag_anno = ifelse(n != 4, 0, 1) )    
  }) -> mc.anni


# stazioni che superano i criteri ####
mc.anni %>% 
  map(\(df) {
    df %>% 
      filter(reporting_year > 2015 & reporting_year < 2020 & flag_anno == 1) %>% 
      nrow() == 4
  }) %>% unlist() -> l1

mc.mesi %>% 
  map(\(df) {
    df %>% 
      filter(reporting_year == 2020 & mese < 7 & flag_mese == 1) %>% 
      nrow() == 6
  }) %>% unlist() -> l2

do.call(cbind, list(l1, l2)) %>% 
  as.data.frame() %>% 
  setNames(c("v1519", "v20")) %>% 
  rownames_to_column(var = "station_eu_code") -> df_valide


rm(l1, l2, no2_2013_2020)

# associazione dati meteo ####
datiMeteo::dati_meteo %>% 
  filter(station_eu_code %in% staz_lazio$station_eu_code) %>% 
  mutate(jd = as.numeric( date - lubridate::ymd(20130101) ) ) -> meteo_lazio

meteo_lazio %>%
  dplyr::group_split(station_eu_code) -> dfs

no2 %>% 
  split(.$station_eu_code) %>% 
  map(\(df) {
    left_join(df, meteo_lazio, by = c("station_eu_code", "date"))
  }) -> dfs

names(dfs)

imap(dfs, ~write.csv(.x,
  file = glue::glue("dati-lazio/per-stazione/{.y}.csv"), row.names = FALSE))

## solo le valide ####

df_valide %>% 
  filter(v1519 & v20) -> stazioni_valide

saveRDS(stazioni_valide$station_eu_code, file = "valide.RDS")

dfs[names(dfs) %in% stazioni_valide$station_eu_code] %>% 
  imap(~write.csv(.x, file = glue::glue("dati-lazio/per-stazione/valide/{.y}.csv"), row.names = FALSE))

## modello su tutte le stazioni ####
dfs %>% 
  map(\(df) {
    if( nrow(na.omit(df)) > 0 )
      lm(value ~ jd + t2m + pblmax + wspeed + rh, data = df)
  })

