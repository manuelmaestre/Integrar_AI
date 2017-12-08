## Carga el fichero de hogares exportado desde la BD
## Formatea los ficheros de acuerdo a necesidades:
## UTF-8, con BOM, sin encabezado y terminando en ; cada línea
## CH_904_030_AAMMDD_VV_NNNNNNNN.csv

## Library load

#library(readxl)
library(stringr)
library(data.table)
library(readr)

## Environment cleanning

rm(list = ls())

file.cobertura <- './04_Inventory_DB/23_Extraccion_domicilios_totales.txt'
file.bloqueos <- './04_Inventory_DB/21_Extraccion_domicilios_bloqueados.txt'
file.desbloqueos <- './04_Inventory_DB/35_Extraccion_domicilios_desbloqueados.txt'


#####  TOTALES  ######

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

#cobertura$Codigo.CD <- NULL
#cobertura$UBICACION_CD <- NULL
#cobertura$GESCAL_37 <- NULL
#cobertura$Fecha_alta <- str_c('20', cobertura$Fecha_alta)
#cobertura$Codigo.Censal <- ''

cobertura$ult_col <- ''


out.cobertura.name <- str_c('./04_Inventory_DB/sent_OSP/CH_904_030_', AAMMDD, "_01_", NNNNNNNN, '.csv', sep = '', collapse = T)

write_delim(data.frame(cobertura), path = out.cobertura.name,delim = ";", col_names = F)

#######  BLOQUEOS ########


bloqueos <- data.table(read.csv(file = file.bloqueos,
                                 header = T,
                                 sep = ";",
                                 quote = "",
                                 dec = ",",
                                 colClasses = 'character',
                                 comment.char = "",
                                 encoding = 'UTF-8'))

bloqueos <- unique(bloqueos)

NNNNNNNN <- str_pad(nrow(bloqueos),width = 8,side = 'left',pad = '0')
VV <- '01'
AAMMDD <- gsub('-', '', Sys.Date())
AAMMDD <- substr(AAMMDD, 3, nchar(AAMMDD))

#bloqueos$Codigo.CD <- NULL
#bloqueos$UBICACION_CD <- NULL
#bloqueos$GESCAL_37 <- NULL
#bloqueos$Fecha_alta <- str_c('20', bloqueos$Fecha_alta)
#bloqueos$Codigo.Censal <- ''

bloqueos$ult_col <- ''

out.bloqueos.name <- str_c('./04_Inventory_DB/sent_OSP/BO_904_030_', AAMMDD, "_01_", NNNNNNNN, '.csv', sep = '', collapse = T)

write_delim(data.frame(bloqueos), path = out.bloqueos.name,delim = ";", col_names = F)

# #####  DESBLOQUEOS ######
# 
# desbloqueos <- data.table(read.csv(file = file.desbloqueos,
#                                 header = T,
#                                 sep = ";",
#                                 quote = "",
#                                 dec = ",",
#                                 colClasses = 'character',
#                                 comment.char = "",
#                                 encoding = 'UTF-8'))
# 
# desbloqueos <- unique(desbloqueos)
# 
# NNNNNNNN <- str_pad(nrow(desbloqueos),width = 8,side = 'left',pad = '0')
# VV <- '01'
# AAMMDD <- gsub('-', '', Sys.Date())
# AAMMDD <- substr(AAMMDD, 3, nchar(AAMMDD))
# 
# #bloqueos$Codigo.CD <- NULL
# #bloqueos$UBICACION_CD <- NULL
# #bloqueos$GESCAL_37 <- NULL
# #bloqueos$Fecha_alta <- str_c('20', bloqueos$Fecha_alta)
# #bloqueos$Codigo.Censal <- ''
# 
# bloqueos$ult_col <- ''
# 
# out.desbloqueos.name <- str_c('./04_Inventory_DB/sent_OSP/DB_904_030_', AAMMDD, "_01_", NNNNNNNN, '.csv', sep = '', collapse = T)
# 
# write_delim(data.frame(desbloqueos), path = out.desbloqueos.name,delim = ";", col_names = F)



