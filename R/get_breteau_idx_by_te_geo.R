#'  @title 
#'  get_breteau_idx_by_te_geo
#'  @description
#'  Calcula el índice de Breteau, el índice de casa positiva y el índice de 
#'  recipiente positivo a partir de un dataframe filtrado por tipo de estudio y 
#'  variable geográfica.
#'
#'  @param 
#'  * `df` El dataframe que contiene los datos.
#'  * `te` El tipo de estudio a filtrar. Por defecto, se establece como "Verificacion".
#'  * `var` La variable geográfica utilizada para el cálculo del índice de Breteau.
#' 
#'  @return 
#'  Un dataframe con los resultados del cálculo del índice de Breteau por variable geográfica.
#'
#'  @importFrom dplyr filter group_by select summarize ungroup
#'
#'  @examples
#' get_breteau_idx_by_te_geo(df, "Verificacion", "Localidad")
#' 
#' @export
# Regresa el índice de Breteau, el índice de casa positiva y el índice de 
# recipiente positivo de acuerdo al nivel de agregación en la variable geográfica


get_breteau_idx_by_te_geo <- function( df, te ="Verificacion", var){
  # calculo de indice de bretau se potiene dividiendo el total de recipientes 
  # positivos entre el total de casas revisadas 
  
  #Filtrar el dataframe por tipo de estudio y fecha de inicio
    dfti <- df %>%
      filter(Tipo_de_Estudio ==  te) %>%
      group_by( !!sym(var)) %>% 
      select(var, Casas_Revisadas,
              Casas_Positivas,
              Total_de_Recipientes_con_Agua,
              Total_de_Recipientes_Positivos) %>%
      summarize(ICP = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
              IRP = sum(Total_de_Recipientes_Positivos)/ sum(Total_de_Recipientes_con_Agua)*100,
              IB = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
              )%>%
      ungroup()
   return(dfti)
    
    
    
    
}

