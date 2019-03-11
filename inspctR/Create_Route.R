# setwd('~/MS Analytics/Capstone')
library(reticulate)

nominatim_osm <- function(address = NULL) {
    if(suppressWarnings(is.null(address)))
        return(data.frame())
    tryCatch(
        d <- jsonlite::fromJSON( 
            gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
                 'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
        ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

coords <- nominatim_osm('405 N Wabash Ave, Chicago, IL')

complaints <- read.csv('Building_Complaints_Full.csv')

OptimalRouter <- import("OptimalRouter")

final_route <- OptimalRouter$TabuSearch(complaints, coords$lon, coords$lat)
