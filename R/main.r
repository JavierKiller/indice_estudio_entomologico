library(tidyverse)
library(lazyeval)
library(lubridate)
library(ggplot2)
library(rlang)
#library(here)

source("get_breteau_idx_by_te_geo.r")
source()
source()
source("load_raw_data.r")
source("bar_trs.r")
source("gra_id.r")

path <- "../raw_data/DescargaEntomologicoe26_10(1).txt"

col_select1 <- c("Tipo de Estudio",
                 "Jurisdiccion", "Localidad",
                 "Sector", "Fecha de Inicio",
                 "Semana Epidemiologica",
                 "Casas Revisadas",
                 "Casas Positivas",
                 "Total de Recipientes con Agua",
                 "Total de Recipientes Positivos")


#col_select_ <- c("Tipo de Estudio",
#               "Jurisdiccion", "Localidad",
#               "Sector", "Fecha de Inicio",
#               "Semana Epidemiologica",
#               "Recipientes Tratables",
#               "Recipientes Controlables",
#               "Recipientes Eliminables")

#df <- load_raw_data(path, col_name = col_select_)

#df

#filepath <- "../visualization//dias.jpg"

#g_tr_sector <- bar_trs(df,  "Sector", file_name = filepath)






df1 <- load_raw_data(path, col_name = col_select1)
indices <- gra_id(df1,  "Sector",   file_n_icp="icp1.jpg",
                   file_n_irp="irp1.jpg", file_n_ib="ib1.jpg")
print(indices)

df1

get_breteau_idx_by_te_geo( df1, te ="Verificacion", var = "569")

# setwd(here("Funciones"))
# setwd(file.path(getwd(), "Funciones"))


# file_n_icp="icp1.jpg",
# file_n_irp="irp1.jpg",
#path <- "C:/Users/Javier Edgar Verdugo/Documents/CursoQR/PIE/indice_estudio_entomologico/01enero/DescargaEntomologicoe26_10(1).txt"
