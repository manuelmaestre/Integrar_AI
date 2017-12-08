## Carga los ficheros exportados de cobertura e inventario desde la BD
## Formatea los ficheros de acuerdo a necesidades:
## UTF-8, con BOM, sin encabezado y terminando en ; cada línea
## CH_904_030_AAMMDD_VV_NNNNNNNN.csv
## Trazado_AAMMDD_VV_NNNNNNNN.csv

## Library load

library(readxl)
library(stringr)
library(data.table)
library(readr)

## Environment cleanning

rm(list = ls())

file.cobertura <- './04_Inventory_DB/02_Direcciones_incrementales_SI.txt'
file.trazado <- './04_Inventory_DB/03_Trazado_incremental_SI.txt'

#file.cobertura <- './04_Inventory_DB/00_Alineacion_con_inventario_sistemas/ficheros incrementales cargar sistemas/41_direcciones_incrementales_CTO_existentes.txt'
#file.trazado <- './04_Inventory_DB/00_Alineacion_con_inventario_sistemas/ficheros incrementales cargar sistemas/50_Trazado_ctos_incrementales_a_cargado_sistemas.txt'


cobertura <- data.table(read.csv(file = file.cobertura,
                                 header = T,
                                 sep = ";",
                                 quote = "",
                                 dec = ",",
                                 colClasses = 'character',
                                 comment.char = "",
                                 encoding = 'UTF-8'))

NNNNNNNN <- str_pad(nrow(cobertura),width = 8,side = 'left',pad = '0')
VV <- '01'
AAMMDD <- gsub('-', '', Sys.Date())
AAMMDD <- substr(AAMMDD, 3, nchar(AAMMDD))


cobertura$ult_col <- ''

cobertura <- unique(cobertura)

out.cobertura.name <- str_c('./04_Inventory_DB/sent_SI/CH_904_030_', AAMMDD, "_01_", NNNNNNNN, '.csv', sep = '', collapse = T)

write_delim(data.frame(cobertura), path = out.cobertura.name,delim = ";", col_names = F)

trazado <- data.table(read.csv(file = file.trazado,
                                 header = T,
                                 sep = ";",
                                 quote = "",
                                 dec = ",",
                                 colClasses = 'character',
                                 comment.char = "",
                                 encoding = 'UTF-8'))

NNNNNNNN <- str_pad(nrow(trazado),width = 8,side = 'left',pad = '0')

trazado$ult_col <- ''

trazado <- unique(trazado)

out.trazado.name <- str_c('./04_Inventory_DB/sent_SI/Trazado_', AAMMDD, "_01_", NNNNNNNN, '.csv', sep = '', collapse = T)


write_delim(data.frame(trazado), path = out.trazado.name,delim = ";", col_names = F)






