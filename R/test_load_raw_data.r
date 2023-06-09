library(tidyverse)
library(lazyeval)
library(lubridate)

source("load_raw_data.r")
path <- "../raw_data/DescargaEntomologicoe26_10(1).txt"

df_ <- load_raw_data(path)

# TODO: TERMINAR PRUEBA 