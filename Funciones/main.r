library(tidyverse)
library(lazyeval)
library(lubridate)
library(ggplot2)
library(rlang)



source("load_raw_data.r")
source("bar_trs.r")

path <- "C:/Users/Javier Edgar Verdugo/Documents/CursoQR/PIE/indice_estudio_entomologico/01enero/DescargaEntomologicoe26_10(1).txt"

col_select <- c("Tipo de Estudio",
               "Jurisdiccion", "Localidad",
               "Sector", "Fecha de Inicio",
               "Semana Epidemiologica",
               "Recipientes Tratables",
               "Recipientes Controlables",
               "Recipientes Eliminables")



df <- load_raw_data(path, col_name = col_select)
g_tr_sector <- bar_trs(df,  "Sector" )
