#'  @title 
#'  load_raw_data
#'  @description
#'  Importa datos de un archivo .txt de la actividad de estudio entomológico y los guarda en un archivo .csv.
#'
#'  @param 
#'  * `path` La ruta del archivo .txt que contiene los datos.
#'  * `path_out` La ruta del archivo .csv en el que se guardarán los datos.
#'  * `col_name` Un vector de caracteres con los nombres de las columnas a importar del archivo .txt.
#' 
#'  @return 
#'  Un dataframe con los datos importados.
#'
#'  @importFrom readr read_tsv read_csv
#'  @importFrom lubridate col_date
#'  @importFrom dplyr col_factor
#'  @importFrom stringr str_replace_all
#'
#'  @examples
#' load_raw_data("ruta/al/archivo.txt", "ruta/al/archivo.csv",
#'               c("Tipo de Estudio", "Jurisdiccion", "Localidad", "Sector", "Fecha de Inicio", "Semana Epidemiologica"))
#' 
#' @export
#'
#'  
#'
#' 
#'
#'
#' 

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
  colt = list(
    Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
    Jurisdiccion = "f",
    Localidad = "f",
    Sector = "f",
    Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
    Semana_Epidemiologica = "f",
    Casas_Revisadas = "d",
    Casas_Positivas = "d",
    Total_de_Recipientes_con_Agua = "d",
    Total_de_Recipientes_Positivos = "d"
    
  )
  dftr <- read_tsv(
    path,
    col_select = col_name, 
    
    locale = locale(encoding = "UTF-16" )
  )
  colnames(dftr) <- 
    str_replace_all(colnames(dftr), pattern = " ", replacement = "_")
  
  write.csv(dftr, path_out, row.names=FALSE)
  
  dftr1 <- read_csv(
    "qr.csv",
      col_types = colt,
  )
  write.csv(dftr1, path_out, row.names=FALSE)
  
  return(dftr1)
} 
