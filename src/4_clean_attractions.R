#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# KB 2018
# Process tripadvisor attractions
# Step 4 : clean and prepare attractions data
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
rm(list=ls())
library(data.table)
library(magrittr)
library(geosphere)
library(ggplot2)

attractions <- fread("data/intermediate/attractions_geo.csv", encoding = "UTF-8")

attractions[is.na(attractions)] <- 0

attractions[, rating := (5*Excellent + 4*`Very good` + 3*Average + 2*Poor + 1*Terrible)/5]
attractions[is.na(rating), rating := 0.75] #too many NAs to discard
attractions <- attractions[rating>=0.75]

attractions[, province := sapply(url, function(z)(strsplit(z,'-') %>% unlist %>% tail(1)) )]
attractions[, province := gsub("_Province.html", "", province)]
attractions[, province := gsub("_", " ", province)]

GetProvince <- function(text){
  provincias <- c("Pichincha", "Tungurahua", "Esmeraldas", "Imbabura", "Napo", "Orellana",
                  "Guayas", "Santa Elena", "Azuay", "Morona Santiago", "Chimborazo", "Carchi", 
                  "Manabi", "El Oro", "Canar", "Loja", "Cotopaxi", "Zamora Chinchipe", "Sucumbios",
                  "Pastaza", "Galapagos", "Santo Domingo", "Bolivar")
  res <- sapply(provincias, function(p)grepl(p,text))
  res <- provincias[res]
  if(length(res)==0)res<-""
  res <- paste(res, collapse = '|')
  return(res)
}

attractions[, province := sapply(province, GetProvince)]

attractions[,.(title,province)]

# explore as a map in tableau
#fwrite(attractions,"data/intermediate/attractions_for_visualinsp.csv", row.names = F)

attractions[id==5, ':='(lat=-0.238827, lon=-78.515494)]
attractions[id==590, ':='(lat=-0.660392, lon=-90.416658)]
attractions[id==1664, ':='(lat=-0.472462, lon=-76.457852)]
attractions[id==659, ':='(lat=-1.397335, lon=-78.422390)]
attractions[id==1072, ':='(lat=-0.254293, lon=-79.168120)]
attractions[id==978, ':='(lat=-2.201080, lon=-78.845980)]
attractions[id==1579, ':='(lat=-1.086474, lon=-79.156885)]
attractions[id==1398, ':='(lat=-1.396213, lon=-78.422199)]
attractions[id==979, ':='(lat=-2.202862, lon=-78.844736)]
attractions[id==1064, ':='(lat=-2.214729, lon=-80.896164)]
attractions[id==622, ':='(lat=-0.740628, lon=-90.315293)]
attractions[id==825, ':='(lat=-3.996483, lon=-79.201124)]
attractions[id==1504, ':='(lat=-1.560549, lon=-80.814101)]
attractions[id==1184, ':='(lat=-2.457351, lon=-78.173719)]
attractions[id==1138, ':='(lat=-0.911269, lon=-77.808352)]
attractions[id==296, ':='(lat=-0.219180, lon=-78.508923)]
attractions[id==1069, ':='(lat=-0.152774, lon=-79.309462)]
attractions[id==1053, ':='(lat=-1.826577, lon=-80.753408)]
attractions[id==1121, ':='(lat=-1.979145, lon=-80.753928)]
attractions[id==657, ':='(lat=-1.419003, lon=-78.425684)]

attractions[province=="Galapagos" & lon>-80, ':='(lat=-0.743629, lon=-90.312889)]

attractions[, province := NULL]

fwrite(attractions,"data/intermediate/attractions_clean.csv", row.names = F)
