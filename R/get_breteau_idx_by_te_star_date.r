#'  @title 
#'  get_breteau_idx_by_te_star_date
#'  @description
#'  Calcula el índice de Breteau, el índice de casa positiva y el índice de 
#'  recipiene positivo a partir de un dataframe filtrado por tipo de estudio y 
#'  fecha de inicio.
#'
#'  @param 
#'  * `df` El dataframe que contiene los datos.
#'  * `te` El tipo de estudio a filtrar. Por defecto, se establece como "Encuesta".
#'  * `fecha` La fecha de inicio a filtrar. Por defecto, se establece como "2021/01/07".
#' 
#'  @return 
#'  Un dataframe con los resultados del cálculo del índice de Breteau.
#'
#'  @importFrom dplyr filter select summarize ungroup
#'  @importFrom lubridate ymd
#'
#'  @examples
#' get_breteau_idx_by_te_star_date(df1, "Encuesta", "2021/01/07")
#' 
#' @export
#'

get_breteau_idx_by_te_star_date <- function(
    df1,
    te = "Encuesta",
    fecha = "2021/01/07"
){
  # calculo de indice de bretau se potiene dividiendo el total de recipientes 
  # positivos entre el total de casas revisadas 
  
  #Filtrar el dataframe por tipo de estudio y fecha de inicio
  dfti <- df1 %>%
    filter(Tipo_de_Estudio ==  te, Fecha_de_Inicio == ymd(fecha)) %>%
    select(Casas_Revisadas,
           Casas_Positivas,
           Total_de_Recipientes_con_Agua,
           Total_de_Recipientes_Positivos) %>%
    summarize(
      ICP = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
      IRP = sum(Total_de_Recipientes_Positivos)/ 
        sum(Total_de_Recipientes_con_Agua)*100,
      IB = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    ungroup()
  return(dfti)
  
  # regresa el indice de breteau, el índice de casa positiva y el índice de 
  # recipiene positivo deacuerdo al nivel de agregacion  en las variables
  
  
}

