library(tidyverse)


#cargar base de datos de lectura de ovitrampas
#dfov <- read.csv(file = "enero/DescargaLecturase26_10.txt",
 #                header = TRUE, sep = "\t", fileEncoding = 'utf-16')
#head(dfov)

#cargar base de datos de Estudio entomologico
dfee0 <- read.csv(file = "mayo/DescargaEntomologicoe26_10.txt", 
                  header = TRUE, sep = "\t", fileEncoding = 'utf-16')
#revisar lo s datos 
head(dfee0)

#revisar el tipo de dato que son
str(dfee0)

glimpse(dfee0)
#agrupar en sumatoria por semana epidemiologica y por tipo de estudio, 
#esto para analizar los datos a nivel estatal
dfeeS<-dfee0 %>%
  group_by(Semana.Epidemiologica, Tipo.de.Estudio) %>% 
  summarise(across(where(is.numeric), .fns = sum))


#graficar la cantidad de casas encuetadas y verificadas para compararlas a lo 
#largo de las semanas epidemiologicas
ggplot(data = dfeeS,
       mapping = aes(x = Semana.Epidemiologica, 
                     y = Casas.Revisadas,
                     fill = Tipo.de.Estudio 
       )) +
  geom_bar(position = 'dodge', stat="identity")


#filtrar por localidad en este ejeplo por obregon para analizar sus datos de 
#forma puntual
dfeeobregon<-dfee0 %>% filter(Localidad == '0001 CIUDAD OBREGÓN')

glimpse(dfeeobregon)

#agrupar en sumatoria por semana epidemiologica y por tipo de estudio, 
#esto para analizar los datos a nivel localidad
dfeeobregon<-dfeeobregon %>%
  group_by(Semana.Epidemiologica, Tipo.de.Estudio) %>% 
  summarise(across(where(is.numeric), .fns = sum)) %>%
  
  #agregar calculos de indices de riesgo entomologicos al nuevo data frame 
  mutate(ICP=(Casas.Positivas/Casas.Revisadas*100),
         IB=(Total.de.Recipientes.Positivos/Casas.Revisadas*100),
         (Total.de.Recipientes.Positivos/Total.de.Recipientes.con.Agua*100))

#graficar la cantidad de casas encuetadas y verificadas para compararlas a lo 
#largo de las semanas epidemiologicas
ggplot(data = dfeeobregon,
       mapping = aes(x = Semana.Epidemiologica, 
                     y = Casas.Revisadas,
                     fill = Tipo.de.Estudio 
       )) +
  geom_bar(position = 'dodge', stat="identity")

#graficar el ICP de casas encuetadas y verificadas para compararlas a lo 
#largo de las semanas epidemiologicas
ggplot(data = dfeeobregon,
       mapping = aes(x = Semana.Epidemiologica, 
                     y = ICP,
                     color = Tipo.de.Estudio 
       )) +
  geom_point()
