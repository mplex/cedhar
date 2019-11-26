
## 
## FUNCTION get.edh() to get data API from the Epigraphic Database Heidelber with R
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## Parameter description from https://edh-www.adw.uni-heidelberg.de/data/api
##
## Search parameters for inscriptions and geography:
## province (get list of valid values at https://edh-www.adw.uni-heidelberg.de/data/api/terms/province, case insensitive)
## country (get list of valid values at https://edh-www.adw.uni-heidelberg.de/data/api/terms/country, case insensitive)
## findspot_modern (add leading and/or trailing truncation by asterisk *, e.g. findspot_modern=k�ln*, case insensitive)
## findspot_ancient (add leading and/or trailing truncation by asterisk *, e.g. findspot_ancient=aquae*, case insensitive)
## bbox (bounding box in the format bbox=minLong , minLat , maxLong , maxLat , example: https://edh-www.adw.uni-heidelberg.de/data/api/inscriptions/search?bbox=11,47,12,48)
##
## Search parameters for inscriptions:
## hd_nr (HD-No of inscription)
## year_not_before (integer, BC years are negative integers)
## year_not_after (integer, BC years are negative integers)
## tm_nr (integer value)
## transcription (automatic leading & trailing truncation, brackets are ignored)
## type (of inscription, get list of values at https://edh-www.adw.uni-heidelberg.de/data/api/terms/type, case insensitive)
##
## Search parameters for geography:
## findspot (level of village, street etc.; add leading and/or trailing truncation by asterisk *, e.g. findspot_modern=k�ln*, case insensitive)
## pleiades_id (Pleiades identifier of a place; integer value)
## geonames_id (Geonames identifier of a place; integer value)
## 
## Additional parameters:
## search (whether to search in "inscriptions" or in "geography")
## addID (whether or not add numeric ID to the list)
## ... (extra parameters if required)


get.edh <-
function (search = c("inscriptions", "geography"), url = "https://edh-www.adw.uni-heidelberg.de/data/api", 
    hd_nr, province, country, findspot_modern, findspot_ancient, 
    year_not_before, year_not_after, tm_nr, transcription, type, 
    bbox, findspot, pleiades_id, geonames_id, addID, ...) 
{
## Resolve parameters
    url <- paste(url, match.arg(search), "search?", sep = "/")
    ifelse(missing(hd_nr) == FALSE, NA, hd_nr <- "")
    ifelse(missing(province) == FALSE, NA, province <- "")
    ifelse(missing(country) == FALSE, NA, country <- "")
    ifelse(missing(findspot_modern) == FALSE, NA, findspot_modern <- "")
    ifelse(missing(findspot_ancient) == FALSE, NA, findspot_ancient <- "")
    ifelse(missing(year_not_before) == FALSE, NA, year_not_before <- "")
    ifelse(missing(year_not_after) == FALSE, NA, year_not_after <- "")
    ifelse(missing(tm_nr) == FALSE, NA, tm_nr <- "")
    ifelse(missing(transcription) == FALSE, NA, transcription <- "")
    ifelse(missing(type) == FALSE, NA, type <- "")
    ifelse(missing(bbox) == FALSE, NA, bbox <- "")
    ifelse(missing(findspot) == FALSE, NA, findspot <- "")
    ifelse(missing(pleiades_id) == FALSE, NA, pleiades_id <- "")
    ifelse(missing(geonames_id) == FALSE, NA, geonames_id <- "")
    ifelse(missing(addID) == FALSE && isTRUE(addID == TRUE) == 
        FALSE, addID <- FALSE, addID <- TRUE)
    if (match.arg(search) == "inscriptions") {
        string <- paste(url, "hd_nr=", hd_nr, "&", "province=", 
            province, "&", "country=", country, "&", "findspot_modern=", 
            findspot_modern, "&", "findspot_ancient", findspot_ancient, 
            "&", "year_not_before=", year_not_before, "&", "year_not_after=", 
            year_not_after, "&", "tm_nr=", tm_nr, "&", "transcription=", 
            transcription, "&", "type=", type, "&", "bbox=", 
            bbox, sep = "")
    }
    else if (match.arg(search) == "geography") {
        string <- paste(url, "province=", province, "&", "country=", 
            country, "&", "findspot_modern=", findspot_modern, 
            "&", "findspot_ancient", findspot_ancient, "&", "findspot=", 
            findspot, "&", "bbox=", bbox, "&", "pleiades_id", 
            pleiades_id, "&", "geonames_id", geonames_id, sep = "")
    }
    else {
        stop("Only \"inscriptions\" and \"geography\" parameters are supported.")
    }
## Convert data from json into a list format
    raw.dat <- rjson::fromJSON(file = string)
## Return values with ID or ordered attribute names if needed
    if (isTRUE((raw.dat$total) == 0L) == TRUE || isTRUE(length(raw.dat) == 
        0L) == TRUE) {
        return(NULL)
    }
    else {
        dat <- raw.dat$items
        if (isTRUE(addID == TRUE) == TRUE) {
            switch(match.arg(search), inscriptions = {
                path <- ".*inschrift/HD"
            }, geography = {
                path <- ".*geographie/"
            })
        }
        else {
            NA
        }
        if (isTRUE(length(dat) == 1L) == TRUE) {
            dat[[1]] <- dat[[1]][order(names(dat[[1]]))]
            ifelse(isTRUE(addID == TRUE) == TRUE, return(c(ID = sub(path, 
                "", dat[[1]]$uri), dat[[1]])), return(dat[[1]]))
        }
        else {
            dato <- list()
            length(dato) <- length(dat)
            for (k in seq_len(length(dat))) {
                dato[[k]] <- dat[[k]][order(names(dat[[k]]))]
            }
            rm(k)
            if (isTRUE(addID == TRUE) == TRUE) {
                daton <- dato
                for (n in seq_len(length(dato))) {
                  daton[[n]] <- c(ID = sub(path, "", dato[[n]]$uri), 
                    dato[[n]])
                }
                return(daton)
            }
            else {
                return(dato)
            }
        }
    }
}
