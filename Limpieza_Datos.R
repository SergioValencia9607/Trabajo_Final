
library(sf)
library(dplyr, warn.conflicts = FALSE)

# Encontrar las coordenadas de la BD.
for (year in 2014:2019){
  Archivo <- gsub(" ", "", paste("Accidentalidad/",year,"/Accidentalidad_georreferenciada_",year))
  Datos_sin_Coordenadas <- st_read(gsub(" ","",paste(Archivo,".shp")), stringsAsFactors=FALSE)
  Datos_Coordenadas <- st_transform(Datos_sin_Coordenadas, "+proj=longlat +ellps=WGS84 +datum=WGS84")
  if (year == 2014) {
    Accidentalidad_Total <- Datos_Coordenadas
  } else {
    Accidentalidad_Total <- rbind(Accidentalidad_Total,Datos_Coordenadas)
  }
}

# Eliminar los datos que no tienen barrios ni comunas asociados.
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$TIPO_GEOCO != "ZONA RURAL",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "0",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "AU",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "In",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "NA",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "SN",]
Accidentalidad_Total <- Accidentalidad_Total[!is.na(Accidentalidad_Total$COMUNA),]

Accidentalidad_Total <- Accidentalidad_Total[!is.na(Accidentalidad_Total$BARRIO),]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$BARRIO != "0",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$BARRIO != "Inst",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$BARRIO != "Sin nombre",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$BARRIO != "AUC1",]

# Eliminación de las columnas que no se necesitan para analizar.
Accidentalidad_Total <- select(Accidentalidad_Total, -OBJECTID, -X, -Y, -RADICADO, -DIRECCION_, -CBML, -TIPO_GEOCO, -DISENO, -MES_NOMBRE)

# Separación de la información de latitud y longitud, y agregar a la BD.
coordenadas <- Accidentalidad_Total$geometry
coordenadas <-as.character(coordenadas)
coordenadas <-gsub("c(","",coordenadas,fixed=TRUE)
coordenadas <-gsub(")","",coordenadas,fixed=TRUE)
coordenadas <-strsplit(coordenadas,split=",",fixed = TRUE)
coordenadas <-unlist(coordenadas)
coordenadas <-as.numeric(coordenadas)
longitud <-coordenadas[seq(1,length(coordenadas),by=2)]
latitud <-coordenadas[seq(2,length(coordenadas),by=2)]
Accidentalidad_Total$Longitud <- longitud
Accidentalidad_Total$Latitud <- latitud
Accidentalidad_Total$geometry <- NULL

# Separación de fecha del accidente
Accidentalidad_Total$f_accidente <- substr(Accidentalidad_Total$FECHA,1,10)
Accidentalidad_Total$FECHA <- NULL

# Indicar cuales días son festivos
festivos <- read.csv("Festivos_Total.csv",header = TRUE,sep = ";")
names(festivos) <- c("FechaFestivo")
Accidentalidad_Total$Festivo <- ifelse(Accidentalidad_Total$f_accidente %in% festivos$FechaFestivo,"Festivo","No Festivo")

# Establecer los valores unicos de barrio y clase de accidente
Accidentalidad_Total$BARRIO <- gsub(". 1",".1",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub(". 2",".2",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub("B. Cerro  El Volador","B. Cerro El Volador",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub("Barrios de Jesús","Barrio de Jesús",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub("Berlín","Berlin",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub("Villa Lilliam","Villa Liliam",Accidentalidad_Total$BARRIO,fixed=TRUE)

Accidentalidad_Total$COMUNA <- gsub("Laureles Estadio","Laureles",Accidentalidad_Total$COMUNA,fixed=TRUE)
Accidentalidad_Total$COMUNA <- gsub("Laureles","Laureles Estadio",Accidentalidad_Total$COMUNA,fixed=TRUE)

Accidentalidad_Total$CLASE <- gsub("Caída de Ocupante","Caída Ocupante",Accidentalidad_Total$CLASE,fixed=TRUE)
Accidentalidad_Total$CLASE <- gsub("Caida Ocupante","Caída Ocupante",Accidentalidad_Total$CLASE,fixed=TRUE)


# Corregir los valores erroneos de barrios y comunas
Comunas_Med <- c("Popular", "Santa Cruz", "Manrique", "Aranjuez", "Castilla", "Doce de Octubre", "Robledo", "Villa Hermosa", "Buenos Aires", "La Candelaria", "Laureles Estadio", "La América", "San Javier", "El Poblado", "Guayabal", "Belén","Corregimiento de San Antonio de Prado","Corregimiento de Santa Elena","Corregimiento de Altavista","Corregimiento de San Cristóbal","Corregimiento de San Sebastián de Palmitas")
Accidentalidad_Total$Comuna2 <- ifelse(Accidentalidad_Total$COMUNA %in% Comunas_Med, "Se encuentra","No se encuentra")
for (fila in 1:length(Accidentalidad_Total$Comuna2)){
  if (Accidentalidad_Total[fila,"Comuna2"] == "No se encuentra"){
    Barrio_Cambio <- Accidentalidad_Total[fila,"COMUNA"]
    Comuna_Cambio <- Accidentalidad_Total[fila,"BARRIO"]
    
    Accidentalidad_Total[fila,"BARRIO"] <- Barrio_Cambio
    Accidentalidad_Total[fila,"COMUNA"] <- Comuna_Cambio
  }
}
Accidentalidad_Total$Comuna2 <- NULL

write.csv(Accidentalidad_Total,"Accidentalidad_Total_3.csv")
