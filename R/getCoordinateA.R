#' get Coordinate from amap
#'
#'
#' get traffic status from amap,there are c(description,expedite,congested,blocked,unkown)
#'
#'
#' @param address character:defalut NULL ,should be rectangle/circle/NULL
#' @param city  character:location should be writtend like
#' @param key should be 'json' or 'xml', the type of the result
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom rvest %>%
#' @return A vector contains description,expedite,congested,blocked,unkown
#' @export  getCoordinateA
#' @examples
#' \dontrun{
#' ## rectangle
#' amap_key='71b002162d43fea08afd5b5e31b030af'
#' getCoordinateA(address='西门�<U+393C><U+3E39>',city='fuzhou',key=amap_key)
#' MapBarBus$location=NA
#' for(i in 1:nrow(MapBarBus)){
#'   MapBarBus$location[i]=getCoordinateA(address=MapBarBus$station[i],city='福州',key=amap_key)
#' }
#'
#' }

getCoordinateA <- function(address = NULL, city = "福州", key = NULL) {
    # url_head='http://restapi.amap.com/v3/geocode/geo?key=您的key&address=火车北站&city=福州'
    url_head <- "http://restapi.amap.com/v3/geocode/geo?key="
    url <- paste(url_head, key, "&address=", address, "&city=", city, "&output=xml", 
        sep = "")
    web <- read_html(url)
    coord <- web %>% html_nodes("location") %>% html_text()
    return(coord)
}





