library(tidyverse)
library(lazyeval)
library(lubridate)
library(ggplot2)
library(rlang)



source("load_raw_data.r")
source("bar_trs.r")
source("gra_id.r")

path <- "C:/Users/Javier Edgar Verdugo/Documents/CursoQR/PIE/indice_estudio_entomologico/01enero/DescargaEntomologicoe26_10(1).txt"

col_select <- c("Tipo de Estudio",
               "Jurisdiccion", "Localidad",
               "Sector", "Fecha de Inicio",
               "Semana Epidemiologica",
               "Recipientes Tratables",
               "Recipientes Controlables",
               "Recipientes Eliminables")



df <- load_raw_data(path, col_name = col_select)
g_tr_sector <- bar_trs(df,  "Fecha_de_Inicio", file_name = "dias.jpg"  )



col_select1 <- c("Tipo de Estudio",
                "Jurisdiccion", "Localidad",
                "Sector", "Fecha de Inicio",
                "Semana Epidemiologica",
                "Casas Revisadas",
                "Casas Positivas",
                "Total de Recipientes con Agua",
                "Total de Recipientes Positivos")



df1 <- load_raw_data(path, col_name = col_select1)
indices <- gra_id(df1,  "Fecha_de_Inicio",   file_n_icp="icp1.jpg",
                   file_n_irp="irp1.jpg", file_n_ib="ib1.jpg")
print(indices)

df1

# file_n_icp="icp1.jpg",
# file_n_irp="irp1.jpg",