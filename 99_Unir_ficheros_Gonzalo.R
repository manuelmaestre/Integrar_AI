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
report.dir <- 'C:/Users/MMAESTRE/Desktop/Nueva carpeta/coordenadas cargadas'
out.error.file <- 'C:/Users/MMAESTRE/Desktop/Nueva carpeta/coordenadas cargadas/agregado.txt'

ficheros <-  list.files(report.dir, full.names = T)

for (in.process.file in ficheros){
  
  print(in.process.file)
  if (.Platform$OS.type == "windows") flush.console()
  
  
  ## Nombre de fichero contiene máscara de error _ficheros_error.csv
  if (substr(in.process.file, start = nchar(in.process.file)-3, stop = nchar(in.process.file)) == '.csv'){
    
    ficheros.error <- data.table(read.table(in.process.file, header = T, sep = ';', colClasses = 'character'))
    #ficheros.error <- data.table(sapply(ficheros.error, as.character))
    
    # Si ya hay datos en la tabla destino, agregamos
    if (nrow(ficheros.error)>0){
      if (exists("current.errores")){
        
        current.errores <- (rbind(current.errores, ficheros.error, fill = T))
        
      }
      
      # Si no existe la tabla destino, la creamos
      if (!exists("current.errores")){
        current.errores <- ficheros.error
      }
      
      rm(ficheros.error)
    }

  }
  
}

current.errores <- unique(current.errores)

write.table(current.errores,
            file = out.error.file,
            sep=",",
            na = "",
            dec=",",
            row.names = F)

write.csv(current.errores, file = out.error.file)
