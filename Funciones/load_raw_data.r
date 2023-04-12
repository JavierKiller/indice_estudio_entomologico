
colt = list(
    Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
    Jurisdiccion = "f",
    Localidad = "f",
    Sector = "f",
    Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
    Semana_Epidemiologica = "f"
    #Recipientes_Tratables = "i",
    #Recipientes_Controlables = "i" ,
    #Recipientes_Eliminables = "i"
  )



load_raw_data <- function(
    path,
    path_out = "qr.csv",
    col_name = c(
      "Tipo de Estudio",
      "Jurisdiccion",
      "Localidad",
      "Sector",
      "Fecha de Inicio",
      "Semana Epidemiologica"
    )
){
  dftr <- read_tsv(
    path,
    #delim  = "\t",
    col_select = col_name, 
    
    locale = locale(encoding = "UTF-16" )
  )
  colnames(dftr) <- str_replace_all(colnames(dftr), pattern = " ", replacement = "_")
  
  write.csv(dftr, path_out, row.names=FALSE)
  
  dftr1 <- read_csv(
    "qr.csv",
      col_types = colt,
  )
  write.csv(dftr1, path_out, row.names=FALSE)
  
  return(dftr1)
} 
