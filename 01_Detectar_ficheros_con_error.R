## Analiza los ficheros de los directorios de entrada
## Marca en la tabla de ficheros los que tienen algún error del tipo:
## 1- Más de un CTO en el fichero. E varios CTOs
## 2- CTO_AI <> CTO_direcciones
## 3- CTO-AI = CTO_direcciones pero no coinciden con nombre de fichero
## Genera tabla de errores para reportar a las EECC
## Elimina los ficheros con error detectado para que no se integren en BD


## Library load

library(readxl)
library(stringr)
library(data.table)

## Environment cleanning

rm(list = ls())


## Utilities

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}


## Path and static variables definition

## Definicion de rutas y variables estáticas
in.files <- './01_infiles'
hoja.AI <- 'AREA INFLUENCIA'
offset.filas.AI <- 2
hoja.CTO <- 'Ubicación CTOs'
offset.CTOs <- 0
fecha <- format(Sys.time(), "%Y%m%d")
e.varios.ctos <- "Error varios CTOs en fichero"
e.distinto.ctoid <- "Error distinto CTO-ID en AI y trazado"
e.distinto.nombre.fichero <- "Error nombre fichero no coincide con CTO-ID"
e.ptos.cto.repetidos <- "Error puertos CTO duplicados"
e.sin.ptos.cto <- "Error puertos CTO no informados"
e.sin.slot <- "Error SLOT OLT no informados"
e.sin.ptos.olt <- "Error puertos OLT no informados"
e.varios.slot <- "Error varios SLOT OLT informados"
e.varios.ptos.OLT <- "Error varios puertos OLT informados"
e.sin.tipo.cto <- "Error sin tipo CTO"
e.sin.spliter <- "Error sin SPLITTER"
e.sin.tipo.spliter <- "Error sin tipo SPLITTER"

out.dir <- './03_Generated_csv_files'
report.dir <- str_c(out.dir, '/report_files', sep = '', collapse = T)

directorios <- list.dirs(in.files,recursive = F)

## Files control & report data table

## Create a data.table with files names in directories to proccess

for (in.process.dir in directorios){
  
  ficheros <-  list.files(in.process.dir, full.names = T)
  zona.EECC <- strsplit(in.process.dir, "/")[[1]]
  zona.EECC <- zona.EECC[length(zona.EECC)]
  
  
  zona.EECC <- strsplit(zona.EECC, '_')[[1]]
  zona <- zona.EECC[1]
  EECC <- zona.EECC[2]
  
  
  #print(zona.EECC)	# You must use print explicitly within a loop
  # or, better, use: cat("loop", i, "\n")
  # Next command is to overcome buffered output in RGui
  #if (.Platform$OS.type == "windows") flush.console()
  
  
  for (in.process.file in ficheros){
    
    
    nombre.fichero <- strsplit(in.process.file, "/")[[1]]
    nombre.fichero <- nombre.fichero[length(nombre.fichero)]
  
    
    print(nombre.fichero)	# You must use print explicitly within a loop
    # or, better, use: cat("loop", i, "\n")
    # Next command is to overcome buffered output in RGui
    if (.Platform$OS.type == "windows") flush.console()
    
    ## Cargamos los ficheros eliminando las filas sin datos que pueda haber
    
    
    if (exists("ficheros.total")){
      ficheros.total <- rbind(ficheros.total, data.table( RutaCompleta = in.process.file,
                                                          NombreFichero = nombre.fichero,
                                                          contrata = EECC,
                                                          zona.MM = zona,
                                                          FechaModificacion = file.mtime(in.process.file),
                                                          Size = file.size(in.process.file),
                                                          Procesado = 'No',
                                                          FechaProcesado = fecha))
    }
    
    
    if (!exists("ficheros.total")){
      ficheros.total <- data.table( RutaCompleta = in.process.file,
                                    NombreFichero = nombre.fichero,
                                    contrata = EECC,
                                    zona.MM = zona,
                                    FechaModificacion = file.mtime(in.process.file),
                                    Size = file.size(in.process.file),
                                    Procesado = 'No',
                                    FechaProcesado = fecha)
    }
    }
    
  }

## Eliminamos los ficheros que no sean Excel

ficheros.total <- ficheros.total[str_to_upper(str_sub(NombreFichero, start = str_length(NombreFichero)-3)) == "XLSX"]

ficheros.no.procesados <- subset(unique(ficheros.total))

## Agregamos una columna de error

ficheros.no.procesados[, c('Error'):= 'OK']

if (nrow(ficheros.no.procesados)>0){
  
  for (i in seq(nrow(ficheros.no.procesados))){
    
    
    fila <- ficheros.no.procesados[i]
    print(nrow(ficheros.no.procesados)-i)
    print(fila$NombreFichero)
    if (.Platform$OS.type == "windows") flush.console()
    
    ## Intentamos abrir el fichero
    
    error.open.file <- try(data.table(read_excel(as.character(fila$RutaCompleta), sheet = hoja.AI, col_names = T, skip = offset.filas.AI)))
    
    if (class(error.open.file) != "try-error"){
    
      AI <- data.table(read_excel(as.character(fila$RutaCompleta), sheet = hoja.AI, col_names = T, skip = offset.filas.AI))
      
      ## Si el fichero tiene el número de columnas correcto, las renombramos para evitar errores con los nombres de columnas
      
      if (ncol(AI) == 58){
        
        colnames(AI) <- c("#", "ERROR", "REPETIDO", "GESCAL_37", "Población", "Provincia", "GIS_Apartment_id", "Calle", "Número", "Bis", "BLOQUE(T)", "BLOQUE(XX)", "PORTAL(O)", "PUERTA(Y)", "LETRA ", "S1", "S2", "Planta", "Mano1", "Texto libre Mano1", "Mano2", "Texto libre Mano2", "PP", "EEEEE", "CCCCC", "FFFFF", "B", "TXX", "OY", "L", "SS", "AAA", "MMMM", "NNNN", "CHECK LONGITUD", "CHECK_OBLIGATORIOS", "Flag_dummy", "SITUACION CTO", "CTO-ID", "ID_CAJA DERIVACIÓN", "Código Gescal", "DENOM.CALLE", "Nº / Nos", "PORTAL", "BLOQUE", "Aclarador", "ESC", "Ubicación CD", "Area caja", "Letra", "Medida CTO JZZ", "S2_id", "S2_Tipo", "S2_Ubicación", "S1_ID", "S1_Tipo", "S1_Puerto", "Observaciones") 
        
      }
      
      
      AI <- AI[is.na(Planta) == F]
      
      CTO <- data.table(read_excel(as.character(fila$RutaCompleta), sheet = 2, col_names = T, skip = offset.CTOs))
      CTO <- CTO[is.na(CTO_ID) == F]
      
      ## Asignamos por nombre de columna, puede que haya menos columnas en el fichero de entrada
      test <- AI[,.(`#`, `ERROR`, `REPETIDO`, `GESCAL_37`, `Población`, `Provincia`, `GIS_Apartment_id`, `Calle`, `Número`, `Bis`, `BLOQUE(T)`, `BLOQUE(XX)`, `PORTAL(O)`, `PUERTA(Y)`, `LETRA `, `S1`, `S2`, `Planta`, `Mano1`, `Texto libre Mano1`, `Mano2`, `Texto libre Mano2`, `PP`, `EEEEE`, `CCCCC`, `FFFFF`, `B`, `TXX`, `OY`, `L`, `SS`, `AAA`, `MMMM`, `NNNN`, `CHECK LONGITUD`, `CHECK_OBLIGATORIOS`, `Flag_dummy`, `SITUACION CTO`, `CTO-ID`, `ID_CAJA DERIVACIÓN`, `Código Gescal`, `DENOM.CALLE`, `Nº / Nos`, `PORTAL`, `BLOQUE`, `Aclarador`, `ESC`, `Ubicación CD`, `Area caja`, `Letra`)]
      test[, c("Medida CTO JZZ"):=NA]
      test <- cbind(test, AI[,.(`S2_id`, `S2_Tipo`, `S2_Ubicación`, `S1_ID`, `S1_Tipo`, `S1_Puerto`, `Observaciones`)])
      
      AI <- test
      colnames(AI) <- c("#", "ERROR", "REPETIDO", "GESCAL_37", "Población", "Provincia", "GIS_Apartment_id", "Calle", "Número", "Bis", "BLOQUE(T)", "BLOQUE(XX)", "PORTAL(O)", "PUERTA(Y)", "LETRA ", "S1", "S2", "Planta", "Mano1", "Texto libre Mano1", "Mano2", "Texto libre Mano2", "PP", "EEEEE", "CCCCC", "FFFFF", "B", "TXX", "OY", "L", "SS", "AAA", "MMMM", "NNNN", "CHECK LONGITUD", "CHECK_OBLIGATORIOS", "Flag_dummy", "SITUACION CTO", "CTO-ID", "ID_CAJA DERIVACIÓN", "Código Gescal", "DENOM.CALLE", "Nº / Nos", "PORTAL", "BLOQUE", "Aclarador", "ESC", "Ubicación CD", "Area caja", "Letra", "Medida CTO JZZ", "S2_id", "S2_Tipo", "S2_Ubicación", "S1_ID", "S1_Tipo", "S1_Puerto", "Observaciones")
      ## Agregamos columnas identificadoras del procesado: Zona, EECC, nombre.fichero origen y fecha de procesado
      AI[, c('Zona', 'EECC', 'nombre.fichero', 'fecha_procesado') := list(as.character(fila$zona.MM), as.character(fila$contrata), as.character(fila$NombreFichero), as.character(fila$FechaProcesado))]
      
      
      CTO <- CTO[, 1:13, with = F]
      colnames(CTO) <- c(" Código OLT", "Tarjeta OLT", "Puerto OLT", "CTO_ID", "TIPO_CTO", "Calle ubicación CTO", "Portal ubicación CTO", "GESCAL17_DIR_CTO", "SPLITTER2_ID", "TIPO_SPLITTER", "Puerto CTO", "GESTOR_VERTICAL", "Provider")
      ## Agregamos columnas identificadoras del procesado: Zona, EECC, nombre.fichero origen y fecha de procesado
      CTO[, c('Zona', 'EECC', 'nombre.fichero', 'fecha_procesado') := list(as.character(fila$zona.MM), as.character(fila$contrata), as.character(fila$NombreFichero), as.character(fila$FechaProcesado))]
      
      ctos.CTO <- unique(CTO[, .(CTO_ID, nombre.fichero)])
      ctos.AI <- unique(AI[, .(`CTO-ID`, nombre.fichero)])
      
      n.CTOs.CTO <- nrow(ctos.CTO)
      n.CTOs.AI <- nrow(ctos.AI)
      
      ## Marcado de error en fichero
      
      ## Comprobamos si hay puertos de CTO repetidos
      ptos.CTO.max <- max(table(CTO$`Puerto CTO`))
      
      
      if (is.integer(ptos.CTO.max) & ptos.CTO.max > 1){
        ficheros.no.procesados[i, Error := e.ptos.cto.repetidos]
      }
      ## Comprobamos que los puertos de CTO están informados
      
      if(length(CTO[is.na(`Puerto CTO`)==F, `Puerto CTO`])==0){
        ficheros.no.procesados[i, Error := e.sin.ptos.cto]
      }
      
      ## Eliminada la verificación de puerto/tarjeta de OLT ya que se cogerá de las PS
      
      # ## Verificar que hay dato Slot/Puerto OLT y es único
      # if(length(CTO[is.na(`Tarjeta OLT`)==F, `Tarjeta OLT`])==0){
      #   ficheros.no.procesados[i, Error := e.sin.slot]
      # } else {
      #   
      #   if (length(unique(CTO$`Tarjeta OLT`))>1){
      #     ficheros.no.procesados[i, Error := e.varios.slot]
      #   }
      #   
      # }
      # 
      # if(length(CTO[is.na(`Puerto OLT`)==F, `Puerto OLT`])==0){
      #   ficheros.no.procesados[i, Error := e.sin.ptos.olt]
      # } else {
      #   
      #   if (length(unique(CTO$`Puerto OLT`))>1){
      #     ficheros.no.procesados[i, Error := e.varios.ptos.OLT]
      #   }
      #   
      # }
      # 
      ## Verificar que hay tipo CTO
      
      if(length(CTO[is.na(TIPO_CTO)==T, TIPO_CTO])>1){
        ficheros.no.procesados[i, Error := e.sin.tipo.cto]
      }
      
      ## Verificar que hay splitter
      
      if(length(CTO[is.na(SPLITTER2_ID)==T, SPLITTER2_ID])>1){
        ficheros.no.procesados[i, Error := e.sin.spliter]
      }
      
      ## Verificar que hay tipo spliter
      
      if(length(CTO[is.na(TIPO_SPLITTER)==T, TIPO_SPLITTER])>1){
        ficheros.no.procesados[i, Error := e.sin.tipo.spliter]
      }
      
      if (n.CTOs.AI == 0 | n.CTOs.CTO == 0){
        
        ficheros.no.procesados[i, Error := e.distinto.ctoid]
        
      } else {
        if (max(n.CTOs.AI, n.CTOs.CTO) > 1){
          ficheros.no.procesados[i, Error := e.varios.ctos]
        }
        
        if (max(n.CTOs.AI, n.CTOs.CTO) == 1){
          
          ## No coinciden los codigos de CTO de direcciones y trazado
          if (ctos.AI[1, .(`CTO-ID`)] != ctos.CTO[1, .(CTO_ID)]){
            ficheros.no.procesados[i, Error := e.distinto.ctoid]
            }
          
          ## Coinciden los códigos de CTO
          if (ctos.AI[1, .(`CTO-ID`)] == ctos.CTO[1, .(CTO_ID)]){
            ## No coincide el nombre del fichero con el de la CTO
            
            if (str_replace(str_to_upper(fila$NombreFichero), ".XLSX", "") != str_to_upper(ctos.CTO[1, .(CTO_ID)])){
              ficheros.no.procesados[i, Error := e.distinto.nombre.fichero]
              }
          }
        }
      }
      
      rm(CTO)
      rm(AI)
      
    }
    
    if (class(error.open.file) == "try-error"){
      
    }
  }
}

## Del listado de ficheros no procesados:
## 1.- Exportar la tabla de errores para comunicar a las EECC
## 2.- Borrar estos ficheros de los directorios para no procesarlos en el paso siguiente

ficheros.error <- ficheros.no.procesados[Error != 'OK', ]

copy.table(ficheros.no.procesados[Error == 'OK',])


if (nrow(ficheros.error) > 0){
  
  
  write.table(ficheros.error,
              file = str_c(report.dir, '/', str_pad(length(list.files(report.dir)), width = 3, pad = "0"), "_", fecha, "_" , 'ficheros_error.csv', sep = '', collapse = T),
              sep=",",
              na = "",
              dec=".",
              row.names = F)
}

if (nrow(ficheros.error)>0){
  
  for (i in seq(nrow(ficheros.error))){

    fila <- ficheros.error[i]
    print(nrow(ficheros.no.procesados)-i)
    print(fila$NombreFichero)
    if (.Platform$OS.type == "windows") flush.console()
    
    file.remove(fila$RutaCompleta)
    
    
  }
}



# #######TEST NUEVAS FUNCIONALIDADES EXCLUYE EL FOR PARA VERIFICACIONES##############
# #######                     COMENTAR EN PRODUCCION                   ##############
# 
# 
# i <- 1
# fila <- ficheros.no.procesados[i]
# #fila$RutaCompleta <- './01_infiles/Z1_ZENER/489-28-177073-CTO-52.xlsx'
# #fila$NombreFichero <- '489-28-177073-CTO-52.xlsx'
# 
# 
# 
# 
#   AI <- data.table(read_excel(as.character(fila$RutaCompleta), sheet = hoja.AI, col_names = T, skip = offset.filas.AI))
#   
#   ## Si el fichero tiene el número de columnas correcto, las renombramos para evitar errores con los nombres de columnas
#   
#   if (ncol(AI) == 58){
#     
#     colnames(AI) <- c("#", "ERROR", "REPETIDO", "GESCAL_37", "Población", "Provincia", "GIS_Apartment_id", "Calle", "Número", "Bis", "BLOQUE(T)", "BLOQUE(XX)", "PORTAL(O)", "PUERTA(Y)", "LETRA ", "S1", "S2", "Planta", "Mano1", "Texto libre Mano1", "Mano2", "Texto libre Mano2", "PP", "EEEEE", "CCCCC", "FFFFF", "B", "TXX", "OY", "L", "SS", "AAA", "MMMM", "NNNN", "CHECK LONGITUD", "CHECK_OBLIGATORIOS", "Flag_dummy", "SITUACION CTO", "CTO-ID", "ID_CAJA DERIVACIÓN", "Código Gescal", "DENOM.CALLE", "Nº / Nos", "PORTAL", "BLOQUE", "Aclarador", "ESC", "Ubicación CD", "Area caja", "Letra", "Medida CTO JZZ", "S2_id", "S2_Tipo", "S2_Ubicación", "S1_ID", "S1_Tipo", "S1_Puerto", "Observaciones") 
#     
#   }
#   
#   
#   AI <- AI[is.na(Planta) == F]
#   
#   CTO <- data.table(read_excel(as.character(fila$RutaCompleta), sheet = 2, col_names = T, skip = offset.CTOs))
#   CTO <- CTO[is.na(CTO_ID) == F]
#   
#   ## Asignamos por nombre de columna, puede que haya menos columnas en el fichero de entrada
#   test <- AI[,.(`#`, `ERROR`, `REPETIDO`, `GESCAL_37`, `Población`, `Provincia`, `GIS_Apartment_id`, `Calle`, `Número`, `Bis`, `BLOQUE(T)`, `BLOQUE(XX)`, `PORTAL(O)`, `PUERTA(Y)`, `LETRA `, `S1`, `S2`, `Planta`, `Mano1`, `Texto libre Mano1`, `Mano2`, `Texto libre Mano2`, `PP`, `EEEEE`, `CCCCC`, `FFFFF`, `B`, `TXX`, `OY`, `L`, `SS`, `AAA`, `MMMM`, `NNNN`, `CHECK LONGITUD`, `CHECK_OBLIGATORIOS`, `Flag_dummy`, `SITUACION CTO`, `CTO-ID`, `ID_CAJA DERIVACIÓN`, `Código Gescal`, `DENOM.CALLE`, `Nº / Nos`, `PORTAL`, `BLOQUE`, `Aclarador`, `ESC`, `Ubicación CD`, `Area caja`, `Letra`)]
#   test[, c("Medida CTO JZZ"):=NA]
#   test <- cbind(test, AI[,.(`S2_id`, `S2_Tipo`, `S2_Ubicación`, `S1_ID`, `S1_Tipo`, `S1_Puerto`, `Observaciones`)])
#   
#   AI <- test
#   colnames(AI) <- c("#", "ERROR", "REPETIDO", "GESCAL_37", "Población", "Provincia", "GIS_Apartment_id", "Calle", "Número", "Bis", "BLOQUE(T)", "BLOQUE(XX)", "PORTAL(O)", "PUERTA(Y)", "LETRA ", "S1", "S2", "Planta", "Mano1", "Texto libre Mano1", "Mano2", "Texto libre Mano2", "PP", "EEEEE", "CCCCC", "FFFFF", "B", "TXX", "OY", "L", "SS", "AAA", "MMMM", "NNNN", "CHECK LONGITUD", "CHECK_OBLIGATORIOS", "Flag_dummy", "SITUACION CTO", "CTO-ID", "ID_CAJA DERIVACIÓN", "Código Gescal", "DENOM.CALLE", "Nº / Nos", "PORTAL", "BLOQUE", "Aclarador", "ESC", "Ubicación CD", "Area caja", "Letra", "Medida CTO JZZ", "S2_id", "S2_Tipo", "S2_Ubicación", "S1_ID", "S1_Tipo", "S1_Puerto", "Observaciones")
#   ## Agregamos columnas identificadoras del procesado: Zona, EECC, nombre.fichero origen y fecha de procesado
#   AI[, c('Zona', 'EECC', 'nombre.fichero', 'fecha_procesado') := list(as.character(fila$zona.MM), as.character(fila$contrata), as.character(fila$NombreFichero), as.character(fila$FechaProcesado))]
#   
#   
#   CTO <- CTO[, 1:13, with = F]
#   colnames(CTO) <- c(" Código OLT", "Tarjeta OLT", "Puerto OLT", "CTO_ID", "TIPO_CTO", "Calle ubicación CTO", "Portal ubicación CTO", "GESCAL17_DIR_CTO", "SPLITTER2_ID", "TIPO_SPLITTER", "Puerto CTO", "GESTOR_VERTICAL", "Provider")
#   ## Agregamos columnas identificadoras del procesado: Zona, EECC, nombre.fichero origen y fecha de procesado
#   CTO[, c('Zona', 'EECC', 'nombre.fichero', 'fecha_procesado') := list(as.character(fila$zona.MM), as.character(fila$contrata), as.character(fila$NombreFichero), as.character(fila$FechaProcesado))]
#   
#   ctos.CTO <- unique(CTO[, .(CTO_ID, nombre.fichero)])
#   ctos.AI <- unique(AI[, .(`CTO-ID`, nombre.fichero)])
#   
#   n.CTOs.CTO <- nrow(ctos.CTO)
#   n.CTOs.AI <- nrow(ctos.AI)
#   
#   ## TODO
#   
#   ## Comprobamos si hay puertos de CTO repetidos
#   ptos.CTO.max <- max(table(CTO$`Puerto CTO`))
#   
#   
#   if (is.integer(ptos.CTO.max) & ptos.CTO.max > 1){
#     ficheros.no.procesados[i, Error := e.ptos.cto.repetidos]
#   }
#   ## Comprobamos que los puertos de CTO están informados
#   
#   if(length(CTO[is.na(`Puerto CTO`)==F, `Puerto CTO`])==0){
#     ficheros.no.procesados[i, Error := e.sin.ptos.cto]
#   }
#   
#   ## Verificar que hay dato Slot/Puerto OLT y es único
#   if(length(CTO[is.na(`Tarjeta OLT`)==F, `Tarjeta OLT`])==0){
#     ficheros.no.procesados[i, Error := e.sin.slot]
#   } else {
#     
#     if (length(unique(CTO$`Tarjeta OLT`))>1){
#       ficheros.no.procesados[i, Error := e.varios.slot]
#     }
#     
#   }
#   
#   if(length(CTO[is.na(`Puerto OLT`)==F, `Puerto OLT`])==0){
#     ficheros.no.procesados[i, Error := e.sin.ptos.olt]
#   } else {
#     
#     if (length(unique(CTO$`Puerto OLT`))>1){
#       ficheros.no.procesados[i, Error := e.varios.ptos.OLT]
#     }
#     
#   }
#   
#   ## Verificar que hay tipo CTO
#   
#   if(length(CTO[is.na(TIPO_CTO)==T, TIPO_CTO])>1){
#     ficheros.no.procesados[i, Error := e.sin.tipo.cto]
#   }
#   
#   ## Verificar que hay splitter
#   
#   if(length(CTO[is.na(SPLITTER2_ID)==T, SPLITTER2_ID])>1){
#     ficheros.no.procesados[i, Error := e.sin.spliter]
#   }
#   
#   ## Verificar que hay tipo spliter
#   
#   if(length(CTO[is.na(TIPO_SPLITTER)==T, TIPO_SPLITTER])>1){
#     ficheros.no.procesados[i, Error := e.sin.tipo.spliter]
#   }
#   
#   ################
#   ################
#   
#   
#   
#   ## Marcado de error en fichero
#   
#   if (n.CTOs.AI == 0 | n.CTOs.CTO == 0){
#     
#     ficheros.no.procesados[i, Error := e.distinto.ctoid]
#     
#   } else {
#     if (max(n.CTOs.AI, n.CTOs.CTO) > 1){
#       ficheros.no.procesados[i, Error := e.varios.ctos]
#     }
#     
#     if (max(n.CTOs.AI, n.CTOs.CTO) == 1){
#       
#       ## No coinciden los codigos de CTO de direcciones y trazado
#       if (ctos.AI[1, .(`CTO-ID`)] != ctos.CTO[1, .(CTO_ID)]){
#         ficheros.no.procesados[i, Error := e.distinto.ctoid]
#       }
#       
#       ## Coinciden los códigos de CTO
#       if (ctos.AI[1, .(`CTO-ID`)] == ctos.CTO[1, .(CTO_ID)]){
#         ## No coincide el nombre del fichero con el de la CTO
#         
#         if (str_replace(str_to_upper(fila$NombreFichero), ".XLSX", "") != str_to_upper(ctos.CTO[1, .(CTO_ID)])){
#           ficheros.no.procesados[i, Error := e.distinto.nombre.fichero]
#         }
#       }
#     }
#   }
#   
#   rm(CTO)
#   rm(AI)
#   


