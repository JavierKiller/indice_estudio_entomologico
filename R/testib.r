
df1 <- read.csv("test_get_breteau_idx_by_te_geo.csv")
df1

df2 <- df1 %>%
  filter( Tipo_de_Estudio == "Encuesta") %>%
  group_by(Sector)%>%
  select(
    Casas_Revisadas,
    Casas_Positivas,
    Total_de_Recipientes_Positivos,
    Total_de_Recipientes_con_Agua
  ) %>%
  summarize(
    ICP = sum(Casas_Positivas) / sum(Casas_Revisadas) * 100,
    IRP = sum(Total_de_Recipientes_Positivos) /
      sum(Total_de_Recipientes_con_Agua) * 100,
    IB = sum(Total_de_Recipientes_Positivos) / sum(Casas_Revisadas) * 100
  )

write.csv(df2, "control_get_breteau_idx_by_te_geo.csv")


#resultado 36.58
# 1305   10     3.08  8
df2

df3 <- df1 %>%
  filter( Tipo_de_Estudio ==  "Verificacion",
          Fecha_de_Inicio == ymd("2021/01/06")) %>%
  select(
    Casas_Revisadas,
    Casas_Positivas,
    Total_de_Recipientes_Positivos,
    Total_de_Recipientes_con_Agua
    ) %>%
  summarize(
    ICP = sum(Casas_Positivas) / sum(Casas_Revisadas) * 100,
    IRP = sum(Total_de_Recipientes_Positivos) /
      sum(Total_de_Recipientes_con_Agua) * 100,
    IB = sum(Total_de_Recipientes_Positivos) / sum(Casas_Revisadas) * 100
  )

#resultado      ICP      IRP       IB
#            8.510638 3.658537 9.574468
df3

df4 <- df1 %>%
  filter( Tipo_de_Estudio == "Verificacion",
          Fecha_de_Inicio ==  ymd("2021/01/06"), Sector == "540") %>%
  select(
    Casas_Revisadas,
    Casas_Positivas,
    Total_de_Recipientes_Positivos,
    Total_de_Recipientes_con_Agua
    ) %>%
  summarize(
    ICP = sum(Casas_Positivas) / sum(Casas_Revisadas) * 100,
    IRP = sum(Total_de_Recipientes_Positivos) /
      sum(Total_de_Recipientes_con_Agua) * 100,
    IB = sum(Total_de_Recipientes_Positivos) / sum(Casas_Revisadas) * 100
  )

#resultado     ICP      IRP       IB
#           10.20408 3.205128 10.20408
df4

