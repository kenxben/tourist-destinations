#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# KB 2018
# Process tripadvisor attractions
# Step 1 : rvest attractions
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

library(data.table)
library(rvest)
library(ggplot2)
library(js)
library(jsonlite)

# starting urls
url_roots <- c("https://www.tripadvisor.com/Attractions-g294307-Activities-Ecuador.html", #page 1
               "https://www.tripadvisor.com/Attractions-g294307-Activities-oa20-Ecuador.html", #page 2
               "https://www.tripadvisor.com/Attractions-g294307-Activities-oa70-Ecuador.html", #page 3
               "https://www.tripadvisor.com/Attractions-g294307-Activities-oa120-Ecuador.html") #page 4 121-153

# 1. get all links to attractions in alls cities in the starting urls 
get_links_cities <- function(u){
  links1 <- u %>% read_html %>% html_nodes(".taLnk") %>% html_attr("href") 
  links2 <- u %>% read_html %>% html_nodes(".Attractions.prodp13n_jfy_overflow_visible ul > li > a") %>% html_attr("href") 
  links <- list(links1,links2)[[which.max(c(length(links1),length(links2)))]]
  links <- links[grepl("Attractions",links) & !grepl("Ecuador",links)]
  paste0("https://www.tripadvisor.com",links)
}


# 2. get links to Types of attractions in Activities in each city url
get_links_types <- function(u){
  
  html <- read_html(u)
  #Types of Attractions 1 (not collapsed)
  selector <- ".ap_filter_wrap.filter_wrap_0.single_select > div.filter_list_0 > div > label > a"
  labels <- sapply(selector,function(s)html %>% html_nodes(s) %>% html_text(), USE.NAMES = F) %>% unlist
  links <- sapply(selector,function(s)html %>% html_nodes(s) %>% html_attr("href"), USE.NAMES = F) %>% unlist

  #Types of Attractions 2 (in case there are collapsed)
  selector <- ".ap_filter_wrap.filter_wrap_0.single_select > div.filter_list_0 > div.collapse > div > label > a"
  labels <- c(labels, unlist(sapply(selector,function(s)html %>% html_nodes(s) %>% html_text(), USE.NAMES = F)))
  links <- c(links, unlist(sapply(selector,function(s)html %>% html_nodes(s) %>% html_attr("href"), USE.NAMES = F)))

  splits <- strsplit(links,"-",fixed = T)
  cities <- unlist(lapply(splits,function(s)last(s)),use.names = F)
  cities <- gsub(".html","",cities,fixed = T)
  numbers <- as.integer(gsub("\\D+","",labels))
  labels <- gsub(" \\(\\d+\\)","",labels)
  links <- paste0("https://www.tripadvisor.com",links)
  return(data.table(city=cities, label=labels, link=links, number = numbers))
}

# 3. get links to attractions in each list of attractions found in 1.
get_links_attractions <- function(u){
  html <- read_html(u)
  selector <- ".listing_info > div.listing_title > a"
  links <- html %>% html_nodes(selector) %>% html_attr("href")
  links <- paste0("https://www.tripadvisor.com",links)
  labels <- html %>% html_nodes(selector) %>% html_text()
  out <- data.table(label=labels, link=links)
  
  #check for more pages
  s <- ".al_border.deckTools.btm > div > div > div > a"
  page_links <- html %>% html_nodes(s) %>% html_attr("href")

  if(length(page_links) > 0){
    page_links <- paste0("https://www.tripadvisor.com",page_links)
    for(u in page_links){
      html <- read_html(u)
      selector <- ".listing_info > div.listing_title > a"
      links <- html %>% html_nodes(selector) %>% html_attr("href")
      links <- paste0("https://www.tripadvisor.com",links)
      labels <- html %>% html_nodes(selector) %>% html_text()
      out <- rbindlist(list(out,data.table(label=labels, link=links)))
    }
  }
  return(out)
}

# Extract info from each attraction page
get_data <- function(u){
  
  html <- read_html(u)
  
  # Title
  s <- ".heading_title"
  title <- html %>% html_nodes(s) %>% html_text() %>% paste0("")
  
  # Ratings histogram
  s <- ".chart_row.clickable > span.row_label.row_cell"
  rating_scale <- html %>% html_nodes(s) %>% html_text()
  s <- ".chart_row.clickable > span.row_count.row_cell"
  rating_proportions <- html %>% html_nodes(s) %>% html_text() %>% paste0("")
  rating_proportions <- as.numeric(gsub("%","",rating_proportions))/100

  # Review count
  s <- ".rating_and_popularity > span.header_rating > div"
  reviews <- html %>% html_nodes(s) %>% html_text() %>% paste0("")
  reviews <- gsub(",","",reviews)
  reviews <- as.integer(gsub(" Reviews","",reviews))
  
  # Types of attraction
  s <- ".rating_and_popularity > span.header_detail.attraction_details > div"
  types <- html %>% html_nodes(s) %>% html_text()%>% trimws() %>% paste0("")

  # Formatted address
  #.prw_rup.prw_common_atf_header_bl.headerBL > div > div.blEntry.address.clickable.colCnt1
  #.section.location > div.detail_section.address
  s <- ".detail_section.address"
  formatted_address <- html %>% html_nodes(s) %>% html_text() %>% paste0("")

  # Telephone number
  s <- ".prw_rup.prw_common_atf_header_bl.headerBL > div > div.blEntry.phone"
  telf_num <- html %>% html_nodes(s) %>% html_text() %>% paste0("")
  
  #coordinates
  s <- '//*[@id="taplc_footer_js_globals_0"]/script[4]/text()'
  js <- html %>% html_nodes(xpath = s) %>% html_text %>% esprima_parse %>% fromJSON
  coord <- js[["body"]][["expression"]][["arguments"]][[1]][["body"]][["body"]][[2]][[
    "expression"]][["arguments"]][[8]][["elements"]][[2]][["properties"]][[1]][[
      "value"]][["value"]][15] %>% strsplit(",") %>% unlist %>% as.numeric
  lat <- coord[1]
  lon <- coord[2]
    
  out <- data.table(title=title,reviews=reviews,types=types,formatted_address=formatted_address,
                    telf_num=telf_num, lat=lat, lon=lon, url=u)
  out[,(rating_scale):=data.table(t(rating_proportions))]
  #Sys.sleep(runif(1))
  return(out)
}


url_cities <- unlist(sapply(url_roots,get_links_cities),use.names = F)

url_types_cities <- rbindlist(lapply(url_cities,get_links_types))

url_types_cities <- url_types_cities[label!="Tours"]

url_attractions <- rbindlist(lapply(url_types_cities[,link],get_links_attractions))

url_attractions <- url_attractions[!duplicated(url_attractions)][link!="https://www.tripadvisor.comNA"]

url_data <- rbindlist(lapply(url_attractions[,link],get_data),use.names = T,fill = T)

while (nrow(url_data[title==""])>0) {
  url_data1 <- rbindlist(lapply(url_data[title=="",url],get_data),use.names = T,fill = T)
  url_data <- url_data[title!=""]
  url_data <- rbindlist(list(url_data,url_data1), use.names = T, fill = T)
  print(nrow(url_data[title==""]))
}

url_data <- url_data[!duplicated(url_data)][title!=""]

#test <- get_data(url_attractions[1000,link])

fwrite(url_data,"data/scraped/tripadvisor_attractions.csv")

