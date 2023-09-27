#!/bin/bash

Rscript --vanilla corso-05a.R staz_lombardia.txt "no2" &
Rscript --vanilla corso-05a.R staz_lombardia.txt "pm10" &
Rscript --vanilla corso-05a.R staz_lombardia.txt "pm25" &