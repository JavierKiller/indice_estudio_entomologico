library(dplyr)
library(lazyeval)

not.uniq.per.group <- function(df, grp.var, uniq.var) {
  df %>%
    group_by_(grp.var) #%>%
#    summarise_( n_uniq=interp(~n_distinct(v), v=as.name(uniq.var)) ) %>%
#    filter(n_uniq > 1)
}

xx <- not.uniq.per.group(iris, "Species", "Sepal.Width")


dfx <- read_tsv(path)
str(df)




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
  
  #  print(names(dfts)) # Verificar nombres de columnas
  
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
  
  ggsave("g_bar.jpg", plot = bar_tr_plot, width = 10, height = 8, dpi = 300)
  return(bar_tr_plot)
}



https://dplyr.tidyverse.org/articles/programming.html







bar_tr_plot <- ggplot(data = dfti) + 
  geom_bar(
    aes(x = "", y = ICP, fill = Tipo_de_Estudio),
    stat = "identity", 
    position = "dodge", 
    width = 0.2) +
  geom_bar(
    aes(x = "1", y = IRP, fill = Tipo_de_Estudio), 
    stat = "identity", 
    position = "dodge", 
    width = 0.2) + 
  geom_bar(
    aes(x = "2", y = IB, fill = Tipo_de_Estudio), 
    stat = "identity", 
    position = "dodge", 
    width = 0.2) +
  scale_x_discrete(labels = c("ICP",
                              "IRP", 
                              "IB"),
                   limits = c("", "1", "2")) +
  labs(x = "", y = "Porcentaje", fill = "Tipo de Estudio") +
  facet_wrap(as.name(var), ncol = 3)
print(bar_tr_plot)








gra_id <- function(dft, var, file_name="gra_ind.jpg") {
  
  dfti <- dft %>% 
    
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
  ggplot(dfti, aes(x = !!sym(var), y = ICP,
                   color = Tipo_de_Estudio,
                   group = Tipo_de_Estudio)) +
    geom_line() +
    labs(title = "Serie de tiempo para ICP",
         x = "Tiempo", y = "ICP")
  
  ggsave("icp.jpg",
         plot = icp_ev_plot, width = 10,
         height = 8, dpi = 300)
  
  
  # Graficar serie de tiempo para IRP
  ggplot(dfti, aes(x = !!sym(var), y = IRP,
                   color = Tipo_de_Estudio,
                   group = Tipo_de_Estudio)) +
    geom_line() +
    labs(title = "Serie de tiempo para IRP",
         x = "Tiempo", y = "IRP")
  
  ggsave("irp.jpg",
         plot = icp_ev_plot, width = 10,
         height = 8, dpi = 300)
  
  # Graficar serie de tiempo para IB
  ggplot(dfti, aes(x = !!sym(var), y = IB,
                   color = Tipo_de_Estudio,
                   group = Tipo_de_Estudio)) +
    geom_line() +
    labs(title = "Serie de tiempo para IB",
         x = "Tiempo", y = "IB")
  
  ggsave("ib.jpg",
         plot = icp_ev_plot, width = 10,
         height = 8, dpi = 300)
}









library(dplyr)
library(ggplot2)

# Agrupar por década y calcular la tasa de suicidios por cada 100,000 habitantes
suicidios_decada <- suicidios %>%
  group_by(Decade = paste0(substr(Year, 1, 3), "0s")) %>%
  summarise(Total_Suicides = sum(Suicides),
            Population = sum(Population),
            Suicide_Rate = (Total_Suicides / Population) * 100000)

# Ordenar por tasa de suicidios de mayor a menor
suicidios_decada <- suicidios_decada %>%
  arrange(desc(Suicide_Rate))

# Graficar con ggplot2
ggplot(suicidios_decada, aes(x = Decade, y = Suicide_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Tasa de suicidios a nivel mundial por década",
       x = "Década",
       y = "Tasa de suicidios (por 100,000 habitantes)") +
  theme_minimal()


dft_<- get_breteau_idx_by_te_geo(df1, var="Sector")

write.csv(dft_, "prueva1.csv", row.names=FALSE)


df2 <- df1 %>%
  filter( Sector == 569) %>%
  select(Casas_Revisadas, Total_de_Recipientes_Positivos) %>%
  summarize(ib=(sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100))

df2
