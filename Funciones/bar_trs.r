
#' @title
#' bar_trs
#'
#' @description
#' Esta función genera un gráfico de barras apiladas con porcentajes de 
#' recipientes tratables, controlables y eliminables para cada categoría de 
#' estudio. También guarda el gráfico generado en un archivo JPG.
#'
#' @param
#' * dft - El dataframe que contiene los datos de los recipientes.
#' * var - El nombre de la variable categórica utilizada para dividir los datos
#'  en categorías.
#' * file_name - El nombre del archivo JPG donde se guardará el gráfico 
#' generado. Por defecto, es "gg_bar.jpg".
#'
#' @return
#' Un gráfico de barras apiladas con porcentajes de recipientes tratables, 
#' controlables y eliminables para cada categoría de estudio, y también guarda 
#' el gráfico generado en un archivo JPG.
#'
#' @importFrom ggplot2 ggplot geom_bar labs scale_x_discrete facet_wrap ggsave
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom rlang sym as.name
#'
#' @examples
#' # Generar un gráfico de barras apiladas con porcentajes de recipientes 
#' tratables, controlables y eliminables para cada categoría de estudio y 
#' guardar el gráfico generado en un archivo JPG llamado "my_bar_chart.jpg".
#' bar_trs(df, "Tipo_de_Estudio", "my_bar_chart.jpg")
#'
#' @export

bar_trs <- function(dft, var, file_name="gg_bar.jpg") {
  
  dfts <- dft %>% 
    filter(Recipientes_Tratables != 0 |
             Recipientes_Controlables != 0 |
             Recipientes_Eliminables !=0) %>%
    group_by(Tipo_de_Estudio, !!sym(var)) %>% 
    summarise(Recipientes_Tratables,  Recipientes_Controlables, 
              Recipientes_Eliminables) %>%
    mutate(Pct_Recipientes_Tratables = Recipientes_Tratables/ 
             (Recipientes_Tratables + Recipientes_Controlables + 
                Recipientes_Eliminables)*100,
           Pct_Recipientes_Controlables = Recipientes_Controlables/ (
             Recipientes_Tratables + Recipientes_Controlables + 
               Recipientes_Eliminables)*100,
           Pct_Recipientes_Eliminables = Recipientes_Eliminables/ (
             Recipientes_Tratables + Recipientes_Controlables + 
               Recipientes_Eliminables)*100) 
  
  
  bar_tr_plot <- ggplot(data = dfts) + 
    geom_bar(
      aes(x = "", y = Pct_Recipientes_Tratables, fill = Tipo_de_Estudio),
      stat = "identity", 
      position = "dodge", 
      width = 0.2) +
    geom_bar(
      aes(x = "1", y = Pct_Recipientes_Controlables, fill = Tipo_de_Estudio), 
      stat = "identity", 
      position = "dodge", 
      width = 0.2) + 
    geom_bar(
      aes(x = "2", y = Pct_Recipientes_Eliminables, fill = Tipo_de_Estudio), 
      stat = "identity", 
      position = "dodge", 
      width = 0.2) +
    scale_x_discrete(labels = c("Recipientes 
                                Tratables",
                                "Recipientes 
                                Controlables", 
                                "Recipientes 
                                Eliminables"),
                     limits = c("", "1", "2")) +
    labs(x = "", y = "Porcentaje", fill = "Tipo de Estudio") +
    facet_wrap(as.name(var), ncol = 3)
  print(bar_tr_plot)
  
  ggsave(file_name, plot = bar_tr_plot, width = 10, height = 8, dpi = 300)
  return(bar_tr_plot)
}