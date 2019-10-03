setwd("YOUR_WORKING_DIRECTORY")
options(device = Cairo::CairoWin, stringsAsFactors=FALSE)
pacman::p_load("httr")

API_KEY <- "API_KEY_FROM_MAPQUEST"
location <- "LOCATION_NAME" #e.g. Myllykatu 9, 65100 vaasa

path <- "http://open.mapquestapi.com/geocoding/v1/address"
request <- GET(url = path,
               query = list(key = API_KEY, 
                            location = location)
               )

request$status_code
response <- content(request, as = "text", encoding = "UTF-8")
response_flat <- fromJSON(response, flatten = TRUE)

response_flat$results[[1]]$locations[[1]]$latLng
