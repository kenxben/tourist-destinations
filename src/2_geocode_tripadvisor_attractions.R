#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# KB 2018
# Process tripadvisor attractions
# Step 3 : geocode attractions
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

library(data.table)
library(ggplot2)
library(ggmap)
library(jsonlite)


attractions <- fread("data/scraped/tripadvisor_attractions.csv", encoding = "UTF-8")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Geocode missing coordinates
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# geocode lat/lon missings
while (nrow(attractions[is.na(lat)])>0) {
  n <- nrow(attractions[is.na(lat)])
  attractions[is.na(lat), (c("lon","lat")) := geocode(formatted_address)]
  if(nrow(attractions[is.na(lat)]==n))break()
}

# geocode remaining missings
attractions[is.na(lat), (c("lon","lat")) := geocode(title)]
attractions[is.na(lat), (c("lon","lat")) := geocode(paste(tail(unlist(strsplit(formatted_address,",")),2),collapse = ","))]


# fix locations outside Ecuador using Google Places Api
geocode_google <- function(query){
  
  key <- "AIzaSyBF2cblKBqfXP8HhOinoDEccKKirL0kDkA"
  query <- gsub(" ","+",query,fixed = T)
  surl <- paste0("https://maps.googleapis.com/maps/api/place/findplacefromtext/json?input=", query,
                 "&fields=","geometry",
                 "&inputtype=textquery",
                 "&key=",key)
  
  res <- fromJSON(URLencode(surl))
  out <- data.table(lon = ifelse(is.null(res$candidates$geometry$location$lng),NA,res$candidates$geometry$location$lng),
                    lat = ifelse(is.null(res$candidates$geometry$location$lat),NA,res$candidates$geometry$location$lat))
  print(out)
  return(out)
}

# aproximate lat lon ranges for Ecuador
lat.range <- c(-5,1.5)
lon.range <- c(-93.5,-75.2)

nrow(attractions[!between(lat,lat.range[1],lat.range[2]) | 
           !between(lon,lon.range[1],lon.range[2])])

nrow(attractions[is.na(lat)])

# fill missings with Google Places Api
attractions[!between(lat,lat.range[1],lat.range[2]) | 
              !between(lon,lon.range[1],lon.range[2] )|
                         is.na(lat),
            (c("lon","lat")) := rbindlist(lapply(formatted_address, geocode_google))]

#still some unplaced attractions left
View(attractions[is.na(lat)])

#attractions_backup <- copy(attractions)
#attractions <- copy(attractions_backup)

#fix them searching for approximate locations
attractions[is.na(lat),aprox_address:= lapply(formatted_address,
                                              function(s)s %>% strsplit(split = ",") %>% 
                                                unlist %>% tail(2) %>% paste(collapse = ",") %>%
                                                gsub(pattern = "593|539",replacement = ""))]

attractions[is.na(lat),(c("lon","lat")) := rbindlist(lapply(aprox_address, geocode_google))]

attractions[,aprox_address:=NULL]

# (fixed after many adjustments)

attractions[,id:=.I]

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Graph points in map
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# attractions <- attractions[!is.na(lat)]

# getting the map
attractionsMap <- get_map(location = attractions[,c(lon = (min(lon)+max(lon))/2,lat = (min(lat)+max(lat))/2)], zoom = 6,
                          maptype = "satellite", scale = 1)

# plot the attractions
ggmap(attractionsMap) +
  geom_point(data = attractions, aes(x = lon, y = lat, fill = "red", alpha = 0.9), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


#fwrite(attractions,"data/intermediate/attractions_geo.csv")
