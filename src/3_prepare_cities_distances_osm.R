#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# KB 2018
# Process tripadvisor attractions
# Step 3 : prepare cities data and calculate *route* distances and time (in meters and seconds)
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
rm(list = ls())
library(data.table)
library(sf)
library(googlePolylines)
library(jsonlite)
library(ggplot2)

# lodging criteria: 1 stars, 2 stars, cuart and tercera categories excluded

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Load cities (Capitales provinciales y cabeceras cantonales)
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# http://www.geoportaligm.gob.ec/portal/wp-content/uploads/2012/10/poblados.zip

# load and prepare cities data
cities <- read_sf(dsn = "data/shp/poblados", layer = "poblados")
cities <- as.data.table(cities)
cities[, lon:= sapply(geometry,function(g)unlist(g)[1])][, lat:= sapply(geometry,function(g)unlist(g)[2])]
cities[, geometry := NULL]

cities <- cities[tipo %in% c("Capital Provincial","Cabecera Cantonal","Cabecera Parroquial")]
cities[,unique(tipo)]

# cities populations (2010 census)
# https://www.ecuadorencifras.gob.ec/base-de-datos-censo-de-poblacion-y-vivienda-2010/
pop <- fread("data/input/poblacion_inec_c2010.csv")
pop_c <- pop[,.(poblacion=sum(poblacion), superficie=sum(superficie)),by=.(provincia,canton)]
pop_c <- pop_c[!(canton %in% pop[,parroquia])]
pop <- rbindlist(list(pop[,.(provincia,parroquia,poblacion,superficie)],pop_c))
rm(pop_c)
setnames(pop,"parroquia","nombre")

pop[,nombre := gsub("\\((.+)\\)","",nombre)][, nombre := trimws(nombre)]
cities[,nombre := gsub("\\((.+)\\)","",nombre)][, nombre := trimws(nombre)]
pop <- pop[!duplicated(pop[,.(provincia, nombre)])]

setdiff(cities[,nombre],pop[,nombre])

cities[nombre== "SAN MIGUEL DE IBARRA", nombre:= "IBARRA"]
cities[nombre== "FRANCISCO DE ORELLANA", nombre:= "PUERTO FRANCISCO DE ORELLANA"]
cities[nombre== "GENERAL LEONIDAS PLAZA GUTIERREZ", nombre:= "GENERAL LEONIDAS PLAZA G."]
cities[nombre== "BAÑOS DE AGUA SANTA", nombre:= "BAÑOS"]
cities[nombre== "SAN FELIPE DE OÑA", nombre:= "OÑA"]
cities[nombre== "SAN JACINTO DE YAGUACHI", nombre:= "YAGUACHI"]
cities[nombre== "EL DORADO DE CASCALES, LUMBAQUI", nombre:= "LUMBAQUI"]
cities[nombre== "PABLO SEXTO", nombre:= "PABLO VI"]
cities[nombre== "PUEBLO VIEJO", nombre:= "PUEBLOVIEJO"]

cities <- merge(cities, pop, by = c("provincia","nombre"),all.x = T)

# lodging industry
# https://servicios.turismo.gob.ec/index.php/portfolio/catastro-turistico-nacional
lodging <- fread("data/input/catastro_2017_final.csv", colClasses = c(rep('character',20),rep('numeric',8)))

lodging.canton <- lodging[ACTIVIDAD_TURISTICA=="ALOJAMIENTO" &
                            !(CATEGORIA %in% c("CUARTA", "TERCERA", "1 ESTRELLA", "2 ESTRELLAS")),
                          .(lodging = .N), by = .(NOMBRE_PROVINCIA,NOMBRE_CANTON)]

lodging.parroquia <- lodging[ACTIVIDAD_TURISTICA=="ALOJAMIENTO" &
                               !(CATEGORIA %in% c("CUARTA", "TERCERA", "1 ESTRELLA", "2 ESTRELLAS")),
                             .(lodging = .N), by = .(NOMBRE_PROVINCIA,NOMBRE_PARROQUIA)]

lodging <- rbindlist(list(lodging.canton,lodging.parroquia))
rm(lodging.canton,lodging.parroquia)
setnames(lodging,c("NOMBRE_PROVINCIA","NOMBRE_CANTON"),c("provincia", "nombre"))
lodging <- lodging[,.(lodging=max(lodging)), by = .(provincia,nombre)]

lodging[,nombre:=iconv(nombre, to='ASCII//TRANSLIT')]
lodging[,provincia:=iconv(provincia, to='ASCII//TRANSLIT')]
lodging[nombre=="PINAS",nombre:="PIÑAS"][
  nombre=="BANOS DE AGUA SANTA",nombre:="BAÑOS"][
    nombre=="CANAR",nombre:="CAÑAR"][
      nombre=="GENERAL VILLAMIL (PLAYAS)",nombre:="GENERAL VILLAMIL"][
        nombre=="PUERTO FRANCISCO DE ORELLANA (EL COCA)",nombre:="PUERTO FRANCISCO DE ORELLANA"][
          provincia=="CANAR",provincia:="CAÑAR"]

setdiff(cities[,nombre],lodging[,nombre])
setdiff(cities[,provincia],lodging[,provincia])

cities <- merge(cities, lodging, by = c("provincia", "nombre"), all.x = T)
cities <- cities[!is.na(lodging) | lodging > 0]

#Explore populations city distributions
# ggplot(cities) + geom_histogram(aes(x=lodging))

#cities <- cities[lodging>=35]
cities[,id:=.I]

rm(lodging, pop)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Distance matrix for cities
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
lonlat <- cities[,.(coordinates = paste(lon, lat, sep=","))][, coordinates]

distances <- CJ(origin=cities[,id],destination=cities[,id])
distances[, ':='(distance=Inf,duration=Inf)]

for(i in 1:nrow(cities)){
  for(j in 1:i){
    if(i == j){
      distances[origin == i & destination == j,
                ':='(distance = 0, duration = 0)]
    }else{
      coordinates <- paste(lonlat[i], lonlat[j], sep = ";")
      surl <- paste0("http://router.project-osrm.org/route/v1/driving/",coordinates)
      res <- tryCatch({
        fromJSON(URLencode(surl))
      }, error = function(e){
        return(list(routes=list(distance=Inf,duration=Inf)))
        message(e)
      },finally = {
        
      })
      distances[origin == i & destination == j,
                ':='(distance = res$routes$distance/1000,
                     duration = res$routes$duration/3600)]
    }
    
    print(distances[origin == i & destination == j])
  }
}

for(i in 1:nrow(cities)){
  for(j in (i+1):nrow(cities)){
      distances[origin == i & destination == j,
                ':='(distance = distances[origin == j & destination == i, distance],
                     duration = distances[origin == j & destination == i, duration])]
  }
}

#write.csv(distances,"data/intermediate/distances_osm.csv",fileEncoding = "UTF-8",na = "", row.names = F)
#write.csv(cities,"data/intermediate/cities.csv",fileEncoding = "UTF-8",na = "", row.names = F)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Optiona code to add a new item
# WARNING: beware of duplicity
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

rm(list = ls())

cities <- fread("data/intermediate/cities.csv", encoding = "UTF-8")
distances <- fread("data/intermediate/distances_osm.csv", encoding = "UTF-8")

cities <- cities[id!=213]
distances <- distances[origin!=213 & destination!=213]

# check names(cities) first
new.item <- data.table(provincia = "SANTA ELENA",
                       nombre = "MONTAÑITA",
                       f_code = "AL105", 
                       tipo = "Recinto",
                       soc = "ECU",
                       lon = -80.754010,
                       lat = -1.827427,
                       poblacion = 29512,
                       superficie = 426.00,
                       lodging = 38,
                       id = cities[,max(id)]+1)

cities <- rbindlist(list(cities, new.item))

# check names(distances) first
new.distances <- rbindlist(list(CJ(origin = new.item[,id],
                              destination = cities[,unique(id)]),
                           CJ(origin = cities[,unique(id)],
                              destination = new.item[,id])))

new.distances <- data.table(origin=new.item[,id], 
                            destination = cities[,unique(id)],
                            distance = Inf,
                            duration = Inf)

lonlat <- cities[,.(coordinates = paste(lon, lat, sep=","))][, coordinates]

for(i in 1:nrow(cities)){
  coordinates <- paste(lonlat[new.distances[i,origin]], lonlat[new.distances[i,destination]], sep = ";")
  surl <- paste0("http://router.project-osrm.org/route/v1/driving/",coordinates)
  res <- tryCatch({
    fromJSON(URLencode(surl))
  }, error = function(e){
    return(list(routes=list(distance=Inf,duration=Inf)))
    message(e)
  },finally = {
    
  })
  new.distances[i, ':='(distance = res$routes$distance/1000,
                        duration = res$routes$duration/3600)]
  print(new.distances[i])
}

#backup <- copy(new.distances)
#new.distances <- copy(backup)

new.distances2 <- copy(new.distances)
new.distances2 <- new.distances2[,.(destination, origin, distance, duration)]
setnames(new.distances2,names(new.distances))
new.distances <- rbindlist(list(new.distances, new.distances2))
new.distances <- new.distances[!duplicated(new.distances)]

distances <- rbindlist(list(distances, new.distances))

write.csv(distances,"data/intermediate/distances_osm.csv",fileEncoding = "UTF-8",na = "", row.names = F)
write.csv(cities,"data/intermediate/cities.csv",fileEncoding = "UTF-8",na = "", row.names = F)
