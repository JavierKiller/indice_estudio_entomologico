

gra_id <- function(dft1,
                   var,
                   file_n_icp="icp.jpg",
                   file_n_irp="irp.jpg", 
                   file_n_ib="ib.jpg"){
  
  dfti <- dft1 %>% 
    group_by(Tipo_de_Estudio, !!sym(var)) %>% 
    summarise(Casas_Revisadas,
              Casas_Positivas,
              Total_de_Recipientes_con_Agua,
              Total_de_Recipientes_Positivos) %>%
    mutate(ICP = Casas_Positivas/ Casas_Revisadas*100,
           IRP = Total_de_Recipientes_Positivos/ Total_de_Recipientes_con_Agua*100,
           IB = Total_de_Recipientes_Positivos/ Casas_Revisadas*100) 

  
print(dfti)   
  
  # Graficar serie de tiempo para ICP
  icp_plot <- ggplot(dfti, aes(x = !!sym(var),
                               y = ICP,
                               color = Tipo_de_Estudio,
                               group = Tipo_de_Estudio)) +
    geom_line() +
    labs(title = "Serie de tiempo para ICP", 
         x = "Tiempo", y = "ICP")
  
  ggsave(file_n_icp, plot = icp_plot,
         width = 10, height = 8, dpi = 300)
  
  
  # Graficar serie de tiempo para IRP
  irp_plot <- ggplot(dfti, aes(x = !!sym(var),
                               y = IRP,
                               color = Tipo_de_Estudio,
                               group = Tipo_de_Estudio)) +
    geom_line() +
    labs(title = "Serie de tiempo para IRP",
         x = "Tiempo", y = "IRP")
  
  ggsave(file_n_irp, plot = irp_plot,
         width = 10, height = 8, dpi = 300)
  
  # Graficar serie de tiempo para IB
  ib_plot <- ggplot(dfti, aes(x = !!sym(var),
                              y = IB,
                              color = Tipo_de_Estudio,
                              group = Tipo_de_Estudio)) +
    geom_line() +
    labs(title = "Serie de tiempo para IB",
         x = "Tiempo", y = "IB")
  
  ggsave(file_n_ib, plot = ib_plot,
         width = 10, height = 8, dpi = 300)
}
