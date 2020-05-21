#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# KB 2018
# Process tripadvisor attractions
# Step 4 : calculate metrics from cities data
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
rm(list=ls())
library(data.table)
library(magrittr)
library(geosphere)
library(ggplot2)

attractions <- fread("data/intermediate/attractions_clean.csv")
cities <- fread("data/intermediate/cities.csv", drop = c("f_code", "soc", "superficie", "tipo"), encoding = "UTF-8")
CC <- fread("data/intermediate/distances_osm.csv", encoding = "UTF-8")
descriptions <- fread("data/input/cluster_descriptions.csv")
# remove Galapagos destinations from list, to simpify the processing and the trip recommendation results.
cities <- cities[!(nombre %in% c("PUERTO AYORA","PUERTO BAQUERIZO MORENO", "PUERTO VILLAMIL"))]
# change Santo Domingo de los Colorados to Santo Domingo
cities[nombre == "SANTO DOMINGO DE LOS COLORADOS", nombre := "SANTO DOMINGO"]

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Set up useful objects
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# types <- attractions[,types] %>% unique %>% strsplit(split = ",") %>% unlist %>% trimws %>% unique

types_cat <- fread("data/input/types.csv")
categorylist <- setdiff(types_cat[,unique(category)],"Other")

for (cat in categorylist) {
  
  typeslist <- types_cat[category==cat,type]
  attractions[,eval(cat) := 0]
  
  for (t in typeslist) {
    attractions[grepl(t,types,fixed = T), eval(cat) := 1]
  }
}

CC[,distance := round(distance, 0)] #rounded in kilometers
CC[,duration := round(duration*.8, 2)] #round to hours, 2 decimals

GetNearCities <- function(city.id, dur){
  CC[origin == city.id & duration <= dur, destination]
}

# cities-attractions distances (arithmetic squeared inverse) in kilometers
D <- CJ(city=cities[,id],attraction=attractions[,id])
D <- merge(D,cities[,.(city=id,c_lat=lat,c_lon=lon)],by="city")
D <- merge(D,attractions[,.(attraction=id,a_lat=lat,a_lon=lon)],by="attraction")
D[,distance := distGeo(D[,.(c_lon,c_lat)],D[,.(a_lon,a_lat)])/1000] #in kilometers

GetNearestCity <- function(attraction.id){
  dt <- D[attraction==attraction.id, .(city,distance)] %>% setorder(distance)
  return(dt[1,city])
}

# Assign attractions to cities
attractions[, city := sapply(id,GetNearestCity)]

# Calculate cities attractions
dt <- attractions[, lapply(.SD, sum), by=city, .SDcols = categorylist]
cities <- merge(cities, dt, by.x = "id", by.y = "city", all.x = T)
cities[is.na(cities)] <- 0

# To guarantee some level of touristic services in the city, filter by ratings and lodging
cities[, tot.rating := rowSums(.SD), .SDcols = categorylist]
cities <- cities[tot.rating > 4]
cities[, tot.rating := NULL]
setnames(cities, c("nombre","poblacion"), c("name","population"))
rm(t, cat, dt)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Define clusters
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Select main cities by tourist lodging capacity
maincities <- cities[lodging>=12, id]

clusters <- lapply(1:length(maincities), function(z)data.table(maincity.id=maincities[z],
                                                         maincity.name=cities[id %in% maincities[z],name]))

clusters <- rbindlist(clusters)

# Add cities to clusters: Cities as near as max.duration seconds
max.duration <- 1.65 #1.5 hour near
a <- lapply(clusters[,maincity.id], function(z)cities[id %in% GetNearCities(z, max.duration),.(id,name)][,maincity.id:=z])
a <- rbindlist(a)
setnames(a, c("id", "name"), c("city.id", "city.name"))
clusters <- merge(clusters, a, by = "maincity.id", all = T)
rm(a)
# add distance to maincity by city
clusters <- merge(clusters, CC[,.(origin, destination, distance, duration)], 
                  by.x = c("maincity.id", "city.id"), 
                  by.y = c("origin", "destination"))

# clusters table (dt) and normalized ratings
clusters.dt <- merge(clusters, cities[, .SD, .SDcols = c("id", categorylist)], by.x = "city.id", by.y = "id")
clusters.dt <- clusters.dt[, lapply(.SD, sum), by = .(maincity.id, maincity.name), .SDcols = categorylist]

# Normalize ratings
cities[, (categorylist) := lapply(.SD, log1p), .SDcols=categorylist]
cities[, (categorylist) := lapply(.SD, function(z)z/max(z)), .SDcols=categorylist]
clusters.dt[, (categorylist) := lapply(.SD, log1p), .SDcols=categorylist]
clusters.dt[, (categorylist) := lapply(.SD, function(z)z/max(z)), .SDcols=categorylist]

#View(attractions[ city %in% clusters[maincity.id==195,city.id]])

clusters[, duration.h := as.integer(floor(duration))]
clusters[, duration.m := as.integer(round((duration-duration.h)*4,0))*15]

# set names for django database handling:
setnames(clusters, gsub(".","_", names(clusters), fixed = T))
setnames(clusters, tolower(names(clusters)))
setnames(clusters.dt, gsub(" and ",".", names(clusters.dt), fixed = T))
setnames(clusters.dt, gsub(" of ",".", names(clusters.dt), fixed = T))
setnames(clusters.dt, gsub(".","_", names(clusters.dt), fixed = T))
setnames(clusters.dt, gsub(" ","_", names(clusters.dt), fixed = T))
setnames(clusters.dt, tolower(names(clusters.dt)))
setnames(cities, gsub(" and ",".", names(cities), fixed = T))
setnames(cities, gsub(" of ",".", names(cities), fixed = T))
setnames(cities, gsub(".","_", names(cities), fixed = T))
setnames(cities, gsub(" ","_", names(cities), fixed = T))
setnames(cities, tolower(names(cities)))
setnames(cities, "id","city_id")

names(cities)
names(clusters.dt)
names(clusters)

# add clusters descriptions
clusters.dt <- merge(clusters.dt, descriptions, by = c("maincity_id", "maincity_name"))

# add id variable for django database
cities[, id:=NA]
clusters[, id:=NA]
clusters.dt[, id:=NA]
CC <- CC[origin %in% clusters[,maincity_id] & destination %in% clusters[,maincity_id]]
CC[, id:=NA]

# names as will be used in django database
write.csv(clusters[,.(maincity_id, city_id, distance, duration, id)], "results/cluster.city.csv", row.names = F, na = "")
write.csv(clusters.dt, "results/clusters.csv", row.names = F, na = "")
write.csv(cities, "results/cities.csv", row.names = F, na = "")
write.csv(CC, "results/osm.distances.csv", row.names = F, na = "")
