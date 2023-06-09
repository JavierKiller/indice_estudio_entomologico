library(tidyverse)
library(lazyeval)
library(lubridate)

source("get_breteau_idx_by_te_star_date.r")
df1<- read.csv("test_get_breteau_idx_by_te_geo.csv")


dft_<-get_breteau_idx_by_te_star_date(df1, te = "Verificacion",
                                      fecha = "2021/01/06" )
dft_

#dft_<- get_breteau_idx_by_te_geo(df1, te= "Encuesta", var="Sector")

print(dft_)

# 8.510638 3.658537 9.574468
# 8.510638 3.658537 9.574468