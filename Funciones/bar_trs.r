

bar_trs <- function(dft, var){
  
  dfts <- dft %>% 
    filter(Recipientes_Tratables != 0 |
             Recipientes_Controlables != 0 |
             Recipientes_Eliminables !=0) %>%
    group_by_('Tipo_de_Estudio', var) %>% 
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
  
  print(names(dfts)) # Verificar nombres de columnas
  print(dfts) # Verificar datos
  
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
    facet_wrap( eval(substitute(var)), ncol = 3)
  print(bar_tr_plot)
  
  ggsave("G_bar.jpg", plot = bar_tr_plot, width = 10, height = 8, dpi = 300)
  return(bar_tr_plot)
}
