library(readr)
library(tidyverse)
library(lazyeval)
library(lubridate)

source("get_breteau_idx_by_te_star_date_geo.r")


colt_<- "fffffDfdddd" 

df1 <- read_csv("../data/qr_for_test.csv", col_types = colt_ )

show_col_types = TRUE
dft_ <- get_breteau_idx_by_te_star_date_geo(df1, te = "Verificacion",
                                      fecha = "2021/01/06", sc = 540)
print(dft_)


#ICP      IRP      IB
#  12    5.357143  12