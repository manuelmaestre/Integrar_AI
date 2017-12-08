## Integra los nuevos ficheros que se encuentren en los directorios de entrada
## Compara con el log de ficheros ya integrados anteriormente para procesar sólo los adicionales
## Mueve los ficheros integrados OK y KO a los directorios de histórico KO y OK, a un subdirectorio con la fecha de proceso
## Crea los ficheros de salida con los datos integrados de AI y CTO con la fecha actual, y numero de fichero, para su posterior validación
## Crea/Modifica el nuevo log de ficheros integrados para ejecuciones posteriores

## Library load

library(readxl)
library(stringr)
library(data.table)

## Environment cleanning

rm(list = ls())

## Path and static variables definition

## Definicion de rutas y variables estáticas
in.files <- './01_infiles'
hoja.AI <- 'AREA INFLUENCIA'
offset.filas.AI <- 2
hoja.CTO <- 'Ubicación CTOs'
offset.CTOs <- 0
fecha <- format(Sys.time(), "%Y%m%d")

out.dir <- './03_Generated_csv_files'
aggregated.dir <- str_c(out.dir, '/aggregated', sep = '', collapse = T)
aggregated.AI.file <- str_c(aggregated.dir, '/', fecha, '_', 'ai.csv', sep = '', collapse = T)
aggregated.CTO.file <- str_c(aggregated.dir, '/', fecha, '_', 'cto.csv', sep = '', collapse = T)

log.file.name <- 'log_integrated_files.csv'

log.int.file <- str_c(out.dir, '/log_files/', log.file.name, sep = '', collapse = T)

KO.dir <- str_c('./02_processed_files/KO/', fecha, '/', sep = '', collapse = T)
KO.ESTRUCTURA.dir <- str_c('./02_processed_files/KO_ESTRUCT/', fecha, '/', sep = '', collapse = T)
OK.dir <- str_c('./02_processed_files/OK/', fecha, '/', sep = '', collapse = T)

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

## Los ficheros que no han devuelto dato de fecha y/0 tamaño ponemos uno a mano
ficheros.total[is.na(FechaModificacion) == T, FechaModificacion := Sys.time()]
ficheros.total[is.na(Size) == T, Size := 0]

## Si hay log de ficheros procesados

if (file.exists(log.int.file) == T){
  
  ## Cargamos el log
  log.integrados <- as.data.table(read.csv(log.int.file))
  log.integrados <- log.integrados[Procesado == 'Si']
  
  ## Eliminamos las columnas del log que no utilizaremos
  log.integrados <- log.integrados[,.(NombreFichero, contrata, Size, FechaProcesado)]
  
  
  ## Seleccionamos solo los ficheros nuevos, en base a nombre y tamaño. Si ya se procesó el fichero y ha cambiado de tamaño puede
  ## que haya nueva información no incluida y validada antes
  
  ficheros.no.procesados <- merge(ficheros.total, log.integrados, by.x = c('NombreFichero', 'contrata', 'Size'), by.y = c('NombreFichero', 'contrata', 'Size'), all.x = T)
  setcolorder(ficheros.no.procesados, c("RutaCompleta", "NombreFichero", "contrata", "zona.MM", "FechaModificacion", "Size", "Procesado", "FechaProcesado.x", "FechaProcesado.y"))
  ficheros.procesados <- ficheros.no.procesados[is.na(FechaProcesado.y) == F]
  ficheros.no.procesados <- ficheros.no.procesados[is.na(FechaProcesado.y) == T]
  
  ficheros.no.procesados[, FechaProcesado.y := NULL]
  setnames(ficheros.no.procesados, "FechaProcesado.x", "FechaProcesado")
  
  ficheros.procesados[, FechaProcesado.y := NULL]
  setnames(ficheros.procesados, "FechaProcesado.x", "FechaProcesado")
  
}

## Si no hay log de ficheros procesados

## La tabla de no procesados es la de ficheros totales leidos del HD

if (file.exists(log.int.file) == F){
  ficheros.no.procesados <- ficheros.total
}

## Eliminamos de disco los ficheros procesados

if (file.exists(log.int.file) == T){
  for (fichero in ficheros.procesados$RutaCompleta){
    file.remove(fichero)
  }
}

# for (fichero in ficheros.no.procesados$RutaCompleta){
#   
#   
#   new_name <- gsub('-','^', fichero, fixed = T)
#   old_name <- gsub('^','-', new_name, fixed = T)
#   
#   file.rename(fichero, new_name)
#   file.rename(new_name, old_name)
#   
# }


ficheros.no.procesados <- subset(unique(ficheros.no.procesados))

### Cargamos los datos de los ficheros no procesados

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
      
      
      if (nrow(AI) == 1){
        
        AI <- rbind(AI,AI)
        
      }
      
      AI <- data.table(sapply(AI, as.character))
      AI <- unique(AI)
      
      
      # Si ya hay datos en la tabla destino, agregamos
      if (exists("current.AI")){
        current.AI <- rbind(current.AI, AI, fill = T)
        
      }
      
      # Si no existe la tabla destino, la creamos
      if (!exists("current.AI")){
        current.AI <- AI
      }
      
      CTO <- CTO[, 1:13, with = F]
      colnames(CTO) <- c(" Código OLT", "Tarjeta OLT", "Puerto OLT", "CTO_ID", "TIPO_CTO", "Calle ubicación CTO", "Portal ubicación CTO", "GESCAL17_DIR_CTO", "SPLITTER2_ID", "TIPO_SPLITTER", "Puerto CTO", "GESTOR_VERTICAL", "Provider")
      ## Agregamos columnas identificadoras del procesado: Zona, EECC, nombre.fichero origen y fecha de procesado
      CTO[, c('Zona', 'EECC', 'nombre.fichero', 'fecha_procesado') := list(as.character(fila$zona.MM), as.character(fila$contrata), as.character(fila$NombreFichero), as.character(fila$FechaProcesado))]
      
      
      if (nrow(CTO) == 1){
        
        CTO <- rbind(CTO,CTO)
        
      }
      
      CTO <- data.table(sapply(CTO, as.character))
      CTO <- unique(CTO)
      
      
      # Si ya hay datos en la tabla destino, agregamos
      if (exists("current.CTO")){
        current.CTO <- rbind(current.CTO, CTO, fill = T)
      }
      
      # Si no existe la tabla destino, la creamos
      if (!exists("current.CTO")){
        current.CTO <- CTO
      }
      
      rm(CTO)
      rm(AI)
      
      if (dir.exists(OK.dir)==F) dir.create(OK.dir, recursive = T)
      ficheros.no.procesados[i, Procesado := 'Si']
      file.copy(from = as.character(fila$RutaCompleta) , to = OK.dir )
      #file.remove(as.character(fila$RutaCompleta))
      
    }
    
    if (class(error.open.file) == "try-error"){
      
      if (dir.exists(KO.dir)==F) dir.create(KO.dir, recursive = T)
      file.copy(from = as.character(fila$RutaCompleta) , to = KO.dir)
      #file.remove(as.character(fila$RutaCompleta))
      
    }
  }
}


## Exportamos los integrados actuales, con la fecha de integracion y el número de fichero en el nombre
## Así permite realizar más de una integración diaria

current.AI <- as.data.table(current.AI)
#current.AI <- current.AI[ERROR != TRUE]


if (nrow(current.AI) > 0){
  
  
  write.table(current.AI,
              file = str_c(aggregated.dir, '/', str_pad(length(list.files(aggregated.dir)), width = 3, pad = "0"), "_", fecha, "_" , 'ai.csv', sep = '', collapse = T),
              sep=",",
              na = "",
              dec=".",
              row.names = F)
}

if (nrow(current.CTO) > 0){
  
  write.table(current.CTO,
              file = str_c(aggregated.dir, '/', str_pad(length(list.files(aggregated.dir)), width = 3, pad = "0"), "_", fecha, "_" , 'cto.csv', sep = '', collapse = T),
              sep=",",
              na = "",
              dec=".",
              row.names = F)
}


## Actualizamos el log de ficheros integrados para en posteriores ejecuciones no procesar ficheros ya integrados
## Hacemos un backup del existente primero

if (file.exists(log.int.file) == T){
 
  file.copy(log.int.file, str_c(out.dir, '/log_files/', fecha, "_" , log.file.name, sep = '', collapse = T)) 
  log.integrados <- as.data.table(read.csv(log.int.file))
  ficheros.no.procesados <- unique(rbind(data.table(sapply(log.integrados, as.character)), data.table(sapply(ficheros.no.procesados, as.character))))

}


write.table(ficheros.no.procesados,
            file = log.int.file,
            sep=",",
            na = "",
            dec=".",
            row.names = F)



# # #######TEST NUEVAS FUNCIONALIDADES EXCLUYE EL FOR PARA VERIFICACIONES##############
# # #######                     COMENTAR EN PRODUCCION                   ##############
# # 
# # 
# 
# ## Library load
# 
# library(readxl)
# library(stringr)
# library(data.table)
# 
# ## Environment cleanning
# 
# rm(list = ls())
# 
# hoja.AI <- 'AREA INFLUENCIA'
# offset.filas.AI <- 2
# hoja.CTO <- 'Ubicación CTOs'
# offset.CTOs <- 0
# 
# rutas.completas <- c('C:/Users/MMAESTRE/Desktop/ai_temp/489-18-021166.01-SAT-40.xlsx', 'C:/Users/MMAESTRE/Desktop/ai_temp/489-18-021305.02-SAT-40.XLSX')
# ficheros <- c('489-18-021166.01-SAT-40.xlsx', '489-18-021305.02-SAT-40.XLSX')
# 
# ficheros.no.procesados <- data.table(rutas.completas,ficheros)
# names(ficheros.no.procesados) <- c("RutaCompleta", "NombreFichero")
# 
# i <- 1
# fila <- ficheros.no.procesados[i]
# 
#     print(nrow(ficheros.no.procesados)-i)
#     print(fila$NombreFichero)
#     if (.Platform$OS.type == "windows") flush.console()
#     
#     ## Intentamos abrir el fichero
#     
#     error.open.file <- try(data.table(read_excel(as.character(fila$RutaCompleta), sheet = hoja.AI, col_names = T, skip = offset.filas.AI)))
#     
#     #if (class(error.open.file) != "try-error"){
#       
#       
#       
#       
#       AI <- data.table(read_excel(as.character(fila$RutaCompleta), sheet = hoja.AI, col_names = T, skip = offset.filas.AI))
#       
#       ## Si el fichero tiene el número de columnas correcto, las renombramos para evitar errores con los nombres de columnas
#       
#       if (ncol(AI) == 58){
#         
#         colnames(AI) <- c("#", "ERROR", "REPETIDO", "GESCAL_37", "Población", "Provincia", "GIS_Apartment_id", "Calle", "Número", "Bis", "BLOQUE(T)", "BLOQUE(XX)", "PORTAL(O)", "PUERTA(Y)", "LETRA ", "S1", "S2", "Planta", "Mano1", "Texto libre Mano1", "Mano2", "Texto libre Mano2", "PP", "EEEEE", "CCCCC", "FFFFF", "B", "TXX", "OY", "L", "SS", "AAA", "MMMM", "NNNN", "CHECK LONGITUD", "CHECK_OBLIGATORIOS", "Flag_dummy", "SITUACION CTO", "CTO-ID", "ID_CAJA DERIVACIÓN", "Código Gescal", "DENOM.CALLE", "Nº / Nos", "PORTAL", "BLOQUE", "Aclarador", "ESC", "Ubicación CD", "Area caja", "Letra", "Medida CTO JZZ", "S2_id", "S2_Tipo", "S2_Ubicación", "S1_ID", "S1_Tipo", "S1_Puerto", "Observaciones") 
#         
#       }
#       
#       
#       AI <- AI[is.na(Planta) == F]
#       
#       CTO <- data.table(read_excel(as.character(fila$RutaCompleta), sheet = 2, col_names = T, skip = offset.CTOs))
#       CTO <- CTO[is.na(CTO_ID) == F]
#       
#       ## Asignamos por nombre de columna, puede que haya menos columnas en el fichero de entrada
#       test <- AI[,.(`#`, `ERROR`, `REPETIDO`, `GESCAL_37`, `Población`, `Provincia`, `GIS_Apartment_id`, `Calle`, `Número`, `Bis`, `BLOQUE(T)`, `BLOQUE(XX)`, `PORTAL(O)`, `PUERTA(Y)`, `LETRA `, `S1`, `S2`, `Planta`, `Mano1`, `Texto libre Mano1`, `Mano2`, `Texto libre Mano2`, `PP`, `EEEEE`, `CCCCC`, `FFFFF`, `B`, `TXX`, `OY`, `L`, `SS`, `AAA`, `MMMM`, `NNNN`, `CHECK LONGITUD`, `CHECK_OBLIGATORIOS`, `Flag_dummy`, `SITUACION CTO`, `CTO-ID`, `ID_CAJA DERIVACIÓN`, `Código Gescal`, `DENOM.CALLE`, `Nº / Nos`, `PORTAL`, `BLOQUE`, `Aclarador`, `ESC`, `Ubicación CD`, `Area caja`, `Letra`)]
#       test[, c("Medida CTO JZZ"):=NA]
#       test <- cbind(test, AI[,.(`S2_id`, `S2_Tipo`, `S2_Ubicación`, `S1_ID`, `S1_Tipo`, `S1_Puerto`, `Observaciones`)])
#       
#       AI <- test
#       colnames(AI) <- c("#", "ERROR", "REPETIDO", "GESCAL_37", "Población", "Provincia", "GIS_Apartment_id", "Calle", "Número", "Bis", "BLOQUE(T)", "BLOQUE(XX)", "PORTAL(O)", "PUERTA(Y)", "LETRA ", "S1", "S2", "Planta", "Mano1", "Texto libre Mano1", "Mano2", "Texto libre Mano2", "PP", "EEEEE", "CCCCC", "FFFFF", "B", "TXX", "OY", "L", "SS", "AAA", "MMMM", "NNNN", "CHECK LONGITUD", "CHECK_OBLIGATORIOS", "Flag_dummy", "SITUACION CTO", "CTO-ID", "ID_CAJA DERIVACIÓN", "Código Gescal", "DENOM.CALLE", "Nº / Nos", "PORTAL", "BLOQUE", "Aclarador", "ESC", "Ubicación CD", "Area caja", "Letra", "Medida CTO JZZ", "S2_id", "S2_Tipo", "S2_Ubicación", "S1_ID", "S1_Tipo", "S1_Puerto", "Observaciones")
#       ## Agregamos columnas identificadoras del procesado: Zona, EECC, nombre.fichero origen y fecha de procesado
#       AI[, c('Zona', 'EECC', 'nombre.fichero', 'fecha_procesado') := list(as.character(fila$zona.MM), as.character(fila$contrata), as.character(fila$NombreFichero), as.character(fila$FechaProcesado))]
#       
#       AI <- data.table(sapply(AI, as.character))
#       
#       # Si ya hay datos en la tabla destino, agregamos
#       if (exists("current.AI")){
#         current.AI <- rbind(current.AI, AI, fill = T)
#         
#       }
#       
#       # Si no existe la tabla destino, la creamos
#       if (!exists("current.AI")){
#         current.AI <- AI
#       }
#       
#       CTO <- CTO[, 1:13, with = F]
#       colnames(CTO) <- c(" Código OLT", "Tarjeta OLT", "Puerto OLT", "CTO_ID", "TIPO_CTO", "Calle ubicación CTO", "Portal ubicación CTO", "GESCAL17_DIR_CTO", "SPLITTER2_ID", "TIPO_SPLITTER", "Puerto CTO", "GESTOR_VERTICAL", "Provider")
#       ## Agregamos columnas identificadoras del procesado: Zona, EECC, nombre.fichero origen y fecha de procesado
#       CTO[, c('Zona', 'EECC', 'nombre.fichero', 'fecha_procesado') := list(as.character(fila$zona.MM), as.character(fila$contrata), as.character(fila$NombreFichero), as.character(fila$FechaProcesado))]
#       
#       
#       if (nrow(CTO) == 1){
#         
#         CTO <- rbind(CTO,CTO)
#         
#       }
#       
#       CTO <- data.table(sapply(CTO, as.character))
#       CTO <- unique(CTO)
#       
#       
#       # Si ya hay datos en la tabla destino, agregamos
#       if (exists("current.CTO")){
#         current.CTO <- rbind(current.CTO, CTO, fill = T)
#       }
#       
#       # Si no existe la tabla destino, la creamos
#       if (!exists("current.CTO")){
#         current.CTO <- CTO
#       }
#       
#       rm(CTO)
#       rm(AI)
#       
#       if (dir.exists(OK.dir)==F) dir.create(OK.dir, recursive = T)
#       ficheros.no.procesados[i, Procesado := 'Si']
#       file.copy(from = as.character(fila$RutaCompleta) , to = OK.dir )
#       #file.remove(as.character(fila$RutaCompleta))
#       
#     #}
#     
#     









