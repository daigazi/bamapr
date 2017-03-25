

#' translate locations by amap API
#'
#'
#' Take in coordiantes and return the location changed by amap API
#'
#'
#' @param locations a vector a or matrix contains longtitude and latitude of the center of the map, or a character refers to the address
#' @param coordsys width of the map
#' @param mapkey height of the map
#' @param mtx  multiplicative factor for the number of pixels returned. possible values are 2 or anything else.
#' @export geoconvA
#' @importFrom rvest html_text
#' @importFrom rvest %>%
#' @importFrom rvest html_nodes
#' @importFrom xml2 read_html
#' @examples
#'
#' \dontrun{
#' amap_key='71b002162d43fea08afd5b5e31b030af'
#' geoconvA(mapkey = amap_key)
#' geoconvA(mapkey = amap_key,mtx=F)
#' }
geoconvA <- function(locations = "116.481499,39.990475|116.481499,39.990375", 
    coordsys = "baidu", mapkey, mtx = T) {
    location_vec <- unlist(strsplit(locations, split = "[|]"))
    num <- length(location_vec)
    if (num > 40) {
        location_changed <- c()
        N <- ceiling(num/40)
        for (i in 1:N) {
            
            if (40 + (i - 1) * 40 > num) {
                index <- c(((i - 1) * 40 + 1):N)
            } else {
                index <- c(1:40) + (i - 1) * 40
            }
            
            location_tmp <- paste(location_vec[index], collapse = "|")
            url_head <- "http://restapi.amap.com/v3/assistant/coordinate/convert?key="
            url <- paste(url_head, mapkey, "&locations=", location_tmp, 
                "&coordsys=", coordsys, "&output=xml", sep = "")
            web <- read_html(url, encoding = "utf-8")
            location_changed0 <- web %>% html_nodes("locations") %>% html_text()
            
            if (length(location_changed) == 0) {
                location_changed <- paste(location_changed, location_changed0, 
                  sep = ";")
                location_changed <- substr(location_changed, 2, nchar(location_changed))
                
            } else {
                location_changed <- paste(location_changed, location_changed0, 
                  sep = ";")
            }
            
        }
    } else {
        url_head <- "http://restapi.amap.com/v3/assistant/coordinate/convert?key="
        url <- paste(url_head, mapkey, "&locations=", locations, "&coordsys=", 
            coordsys, "&output=xml", sep = "")
        web <- read_html(url, encoding = "utf-8")
        location_changed <- web %>% html_nodes("locations") %>% html_text()
    }
    
    if (mtx) {
        location_changed <- polyline2mtx(location_changed)
    }
    return(location_changed)
}

