## Carga el fichero de hogares exportado desde la BD
## Formatea los ficheros de acuerdo a necesidades:
## UTF-8, con BOM, sin encabezado y terminando en ; cada línea
## CH_904_030_AAMMDD_VV_NNNNNNNN.csv

## Library load

library(readxl)
library(stringr)
library(data.table)
library(readr)

## Environment cleanning

rm(list = ls())

file.cobertura <- './04_Inventory_DB/90_Extraccion_direcciones_SI.txt'


cobertura <- data.table(read.csv(file = file.cobertura,
                                 header = T,
                                 sep = ";",
                                 quote = "",
                                 dec = ",",
                                 colClasses = 'character',
                                 comment.char = "",
                                 encoding = 'UTF-8'))

cobertura <- unique(cobertura)

NNNNNNNN <- str_pad(nrow(cobertura),width = 8,side = 'left',pad = '0')
VV <- '01'
AAMMDD <- gsub('-', '', Sys.Date())
AAMMDD <- substr(AAMMDD, 3, nchar(AAMMDD))

cobertura$Codigo.CD <- NULL
cobertura$UBICACION_CD <- NULL
cobertura$GESCAL_37 <- NULL
cobertura$Fecha_alta <- str_c('20', cobertura$Fecha_alta)
cobertura$Codigo.Censal <- ''

cobertura$ult_col <- ''




out.cobertura.name <- str_c('./04_Inventory_DB/sent_OSP/CH_904_030_', AAMMDD, "_01_", NNNNNNNN, '.csv', sep = '', collapse = T)

write_delim(data.frame(cobertura), path = out.cobertura.name,delim = ";", col_names = F)





