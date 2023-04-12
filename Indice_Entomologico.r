# Librerias necesarias 
library(tidyverse)
#library(dplyr)
#library(ggplot2)

# cargar base de datos de Estudio entomologico solo las columnas para 
# analisis de la tipologia de recipiente positivo


dftr <- read_delim("01enero/DescargaEntomologicoe26_10(1).txt", 
                   delim  = "\t", col_select = c("Tipo de Estudio",
                                                 "Jurisdiccion", "Localidad",
                                                 "Sector", "Fecha de Inicio",
                                                 "Semana Epidemiologica",
                                                 "Recipientes Tratables",
                                                 "Recipientes Controlables",
                                                 "Recipientes Eliminables"),
                   locale = locale(encoding = "UTF-16" ))

# cambiar los espacios en los titulos de las variables por "_"

colnames(dftr) <- str_replace_all(colnames(dftr), pattern = " ", replacement = "_")

# imprimir las primeras lineas del objeto
str(dftr)

# crear variables que representen el porcentaje de los resipientes
dftrs <- dftr %>% 
  filter(
    Recipientes_Tratables != 0 |
    Recipientes_Controlables != 0 |
    Recipientes_Eliminables !=0
  ) %>%
  group_by(Tipo_de_Estudio, Sector) %>% 
  summarise(Recipientes_Tratables,  Recipientes_Controlables, 
            Recipientes_Eliminables) %>%
  mutate( Pct_Recipientes_Tratables = Recipientes_Tratables/ 
           (Recipientes_Tratables + Recipientes_Controlables + 
              Recipientes_Eliminables)*100,
          Pct_Recipientes_Controlables = Recipientes_Controlables/ (
      Recipientes_Tratables + Recipientes_Controlables + 
        Recipientes_Eliminables)*100,
      Pct_Recipientes_Eliminables = Recipientes_Eliminables/ (
      Recipientes_Tratables + Recipientes_Controlables + 
        Recipientes_Eliminables)*100) 


head(dftrs)

# grafica de barras de la tipologia de resipiente por sector 

bar_tr_plot <- ggplot(data = dftrs, aes(x = "", y = Pct_Recipientes_Tratables,
                         fill = Tipo_de_Estudio)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2) +
  geom_bar(aes(x = "1", y = Pct_Recipientes_Controlables,
               fill = Tipo_de_Estudio), stat = "identity", 
           position = "dodge", width = 0.2) + 
  geom_bar(aes(x = "2", y = Pct_Recipientes_Eliminables,
               fill = Tipo_de_Estudio), stat = "identity", 
           position = "dodge", width = 0.2) +
  scale_x_discrete(labels = c("Recipientes Tratables", 
                              "Recipientes Controlables", 
                              "Recipientes Eliminables"), 
                   limits = c("", "1", "2")) +
  labs(x = "", y = "Porcentaje", fill = "Tipo de Estudio") +
  facet_wrap(~ Sector, ncol = 4)
bar_tr_plot

ggsave("G_bar.jpg", plot = bar_tr_plot, width = 10, height = 8, dpi = 300)
