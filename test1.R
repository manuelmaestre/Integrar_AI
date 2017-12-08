ficheros.eliminar.log <- data.table(ficheros.eliminar.log)

cruce.corto <- ficheros.eliminar.log[, .(NOMBRE_FICHERO1, NOMBRE_FICHERO2)]

test1 <- merge(log.integrados, cruce.corto, all.x = T, by.x = c("NombreFichero"), by.y = c("NOMBRE_FICHERO1"))

test1 <- test1[is.na(NOMBRE_FICHERO2)==T]
test1$NOMBRE_FICHERO2 <- NULL

test1 <- merge(test1, cruce.corto, all.x = T, by.x = c("NombreFichero"), by.y = c("NOMBRE_FICHERO2"))

test1 <- test1[is.na(NOMBRE_FICHERO1)==T]
test1$NOMBRE_FICHERO1 <- NULL

cols.tabla.comillas(log.integrados)
setcolorder(test1, c("RutaCompleta", "NombreFichero", "contrata", "zona.MM", "FechaModificacion", "Size", "Procesado", "FechaProcesado"))

write.table(test1,
            file = log.int.file,
            sep=",",
            na = "",
            dec=".",
            row.names = F)