#'  @title 
#'  get_breteau_idx_by_te_star_date_geo
#'  @description
#'  Calcula el índice de Breteau, el índice de casa positiva y el índice de 
#'  recipiente positivo a partir de un dataframe filtrado por tipo de estudio, 
#'  fecha de inicio y sector.
#'
#'  @param 
#'  * `df1` El dataframe que contiene los datos.
#'  * `te` El tipo de estudio a filtrar. Por defecto, se establece como "Encuesta".
#'  * `fecha` La fecha de inicio a filtrar. Por defecto, se establece como "2021/01/07".
#'  * `sc` El sector a filtrar.
#' 
#'  @return 
#'  Un dataframe con los resultados del cálculo del índice de Breteau por tipo de estudio, fecha de inicio y sector.
#'
#'  @importFrom dplyr filter select summarize ungroup
#'  @importFrom lubridate ymd
#'
#'  @examples
#' get_breteau_idx_by_te_star_date_geo(df, "Encuesta", "2021/01/07", "SectorA")
#' 
#' @export
#' Regresa el índice de Breteau, el índice de casa positiva y el índice de 
# recipiente positivo de acuerdo al nivel de agregación en la variable geográfica


get_breteau_idx_by_te_star_date_geo <- function( df1, te ="Encuesta",
                                                 fecha = "2021/01/07", sc){
  # calculo de indice de bretau se potiene dividiendo el total de recipientes 
  # positivos entre el total de casas revisadas 
  
  #Filtrar el dataframe por tipo de estudio y fecha de inicio
  dfti <- df1 %>%
    filter(Tipo_de_Estudio ==  te, Fecha_de_Inicio == ymd(fecha),
           Sector == sc) %>%
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
  
  
  
}


