#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# KB 2018
# Process tripadvisor attractions
# Step 3 : prepare cities data and calculate distances and time (in meters and seconds)
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
lodging[nombre=="PINAS",nombre:="PIÑAS"][
  nombre=="BANOS DE AGUA SANTA",nombre:="BAÑOS"][
    nombre=="CANAR",nombre:="CAÑAR"][
      nombre=="GENERAL VILLAMIL (PLAYAS)",nombre:="GENERAL VILLAMIL"][
        nombre=="PUERTO FRANCISCO DE ORELLANA (EL COCA)",nombre:="PUERTO FRANCISCO DE ORELLANA"]

setdiff(cities[,nombre],lodging[,nombre])

cities <- merge(cities, lodging, by = c("provincia", "nombre"), all.x = T)
cities <- cities[!is.na(lodging) | lodging > 0]

#Explore populations city distributions
ggplot(cities) + geom_histogram(aes(x=lodging))

#cities <- cities[lodging>=35]
cities[,id:=.I]

rm(lodging, pop)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Distance matrix for cities
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# encode polylines for google api
lonlat <- cities[,.(lon,lat)] %>% t %>% as.data.table %>% as.list

encpoly <- lapply(lonlat, function(z)encodeCoordinates(z[1],z[2]))
encpoly <- paste0("enc:",encpoly,":")


googledist <- function(encori, encdes, r){
  encdes <- paste(encdes, collapse = "|")
  
  key <- read.delim("keys/api_keys", sep=",", header=FALSE)[[2]]
  surl <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?",
                 "&origins=", encori,
                 "&destinations=", encdes,
                 "&key=",key)
  
  res <- fromJSON(URLencode(surl))
  print(res)
  if (res$rows$elements[[1]]$status != 'OK') {
    print(res$rows$elements[[1]]$status)  
    return(data.table(origin = i,
                      destination = r,
                      distance = rep(NA,length(r)),
                      duration = rep(NA,length(r))))
  }
  
  return(data.table(origin = i,
                    destination = r,
                    distance = res$rows$elements[[1]]$distance$value,
                    duration = res$rows$elements[[1]]$duration$value))
  
}


distances <- list()


for (i in 1:nrow(cities)) {
  j <- 45

  while (j < nrow(cities)) {
    print(paste("i:",i,"j:",j))
    r <- seq(j-44,j)
    d <- googledist(encpoly[i],encpoly[r], r)
    distances[[length(distances)+1]] <- d
    j <- j + 45
  }
  
  if (j-44 < nrow(cities)) {
    r <- seq(j-44,nrow(cities))
    d <- googledist(encpoly[i],encpoly[r], r)
    distances[[length(distances)+1]] <- d
    j <- j + 45
  }
  
}

distances <- rbindlist(distances)

distances[,distance := distance/1000] # in kilometers
distances[,duration := duration/3600] # to hours

#write.csv(distances,"data/intermediate/distances_google.csv",fileEncoding = "UTF-8",na = "", row.names = F)
#write.csv(cities,"data/intermediate/cities.csv",fileEncoding = "UTF-8",na = "", row.names = F)

