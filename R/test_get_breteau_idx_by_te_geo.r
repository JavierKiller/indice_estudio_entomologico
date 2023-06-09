library(tidyverse)
library(lazyeval)
library(lubridate)

source("get_breteau_idx_by_te_geo.r")

colt_<- "fffffDfdddd" 

df1 <- read_csv("../data/qr_for_test.csv", col_types = colt_ )

dft_ <- get_breteau_idx_by_te_geo(
  df1,
  te = "Verificacion",
  var = "clave_Localidad"
)

print(dft_)

