## Library load

library(readxl)
library(stringr)
library(data.table)

## Environment cleanning

rm(list = ls())

## Path and static variables definition

## Definicion de rutas y variables estáticas
fecha <- format(Sys.time(), "%Y%m%d")

out.dir <- './03_Generated_csv_files'
aggregated.dir <- str_c(out.dir, '/aggregated', sep = '', collapse = T)
out.AI.file <- str_c(aggregated.dir, '/AI.csv', sep = '', collapse = T)
out.CTO.file <- str_c(aggregated.dir, '/CTO.csv', sep = '', collapse = T)

ficheros <-  list.files(aggregated.dir, full.names = T)

for (in.process.file in ficheros){
  
  print(in.process.file)
  if (.Platform$OS.type == "windows") flush.console()
  
  
  ## Nombre de fichero contiene máscara de ai _ai.csv
  if (substr(in.process.file, start = nchar(in.process.file)-6, stop = nchar(in.process.file)) == '_ai.csv'){
    
    AI <- data.table(read.table(in.process.file, header = T, sep = ',', colClasses = 'character'))
    #AI <- data.table(sapply(AI, as.character))
    
    # Si ya hay datos en la tabla destino, agregamos
    if (nrow(AI)>0){
      if (exists("current.AI")){
        
        current.AI <- (rbind(current.AI, AI, fill = T))
        
      }
      
      # Si no existe la tabla destino, la creamos
      if (!exists("current.AI")){
        current.AI <- AI
      }
      
      rm(AI)
    }

  }
  
  if (substr(in.process.file, start = nchar(in.process.file)-7, stop = nchar(in.process.file)) == '_cto.csv'){
    
    CTO <- data.table(read.table(in.process.file, header = T, sep = ',', colClasses = 'character'))
    #CTO <- data.table(sapply(CTO, as.character))
    
    if (nrow(CTO)>0){
    
      # Si ya hay datos en la tabla destino, agregamos
      if (exists("current.CTO")){
        current.CTO <- (rbind(current.CTO, CTO, fill = T))
        
      }
      
      # Si no existe la tabla destino, la creamos
      if (!exists("current.CTO")){
        current.CTO <- CTO
      }
      
      rm(CTO)
    }
    
  }
}

current.CTO <- current.CTO[, 1:17, with = F]
current.AI <- current.AI[, 1:62, with = F]

current.AI <- current.AI[is.na(ERROR)==F]
current.CTO <- current.CTO[is.na(Tarjeta.OLT)==F]

current.AI <- unique(current.AI)
current.CTO <- unique(current.CTO)


write.table(current.AI,
            file = out.AI.file,
            sep=",",
            na = "",
            dec=",",
            row.names = F)


write.table(current.CTO,
            file = out.CTO.file,
            sep=",",
            na = "",
            dec=",",
            row.names = F)


