library(tidyverse)
library(lubridate)

change_type_data <- function(df) {
  
  
  # Convertir las columnas seleccionadas a factores
  cols_factor <- c("Tipo_de_Estudio", "Jurisdiccion", "Localidad", "Sector", "Semana_Epidemiologica")
  df[cols_factor] <- lapply(df[cols_factor], factor)
  
  # Convertir la columna Fecha de Inicio a tipo de dato fecha
  df$Fecha_de_Inicio <- dmy(df$Fecha_de_Inicio)
  
  # Convertir las demás columnas a enteros
  cols_entero <- setdiff(names(df), c(cols_factor, "Fecha_de_Inicio"))
  df[cols_entero] <- lapply(df[cols_entero], as.integer)
  
  # Devolver el dataframe transformado
  return(df)
}





#unique()


#col_type = list(
#  Tipo de Estudio = col_factor(c("Encuesta", "Verificacion")),
#  Jurisdiccion = "f",
#  Localidad = "f",
#  Sector = "f",
#  Fecha de Inicio = "D",
#  Semana Epidemiologica = "i",
#  Recipientes_Tratables = "i",
#  Recipientes_Controlables = "i" ,
#  Recipientes_Eliminables = "i"
#)