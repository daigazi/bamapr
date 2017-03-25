
amap_key <- "71b002162d43fea08afd5b5e31b030af"
#' Get Traffic Status from amap
#'
#'
#' get traffic status from amap,there are c(description,expedite,congested,blocked,unkown)
#'
#'
#' @param type character:defalut NULL ,should be rectangle/circle/NULL
#' @param location  character:location should be writtend like
#' @param name should be 'json' or 'xml', the type of the result
#' @param radius logical value, return the coordinates or the original results
#' @param city character
#' @param citycode code of city.for examples fuzhou is 0591
#' @param key amap key
#' @param output xml or json
#' @importFrom XML htmlParse
#' @importFrom XML xpathSApply
#' @return A vector contains description,expedite,congested,blocked,unkown
#' @export  geturlRouteA
#' @examples
#' \dontrun{
#' ## rectangle
#' location='119.278861,26.106208;119.336454,26.078304'
#' a=getTrafficStatus(location = location,key = amap_key,type='rectangle')
#'
#' ## circle
#' location='119.278861,26.106208'
#' b=getTrafficStatus(type='circle',location = location,key = amap_key)
#'
#'## road
#'location='119.278861,26.106208'
#'c=getTrafficStatus(type='name',key = amap_key,name='古田跐?<U+3E66>')
#' }

geturlRouteA <- function(orgin = "116.481499,39.990475", destination = "116.465063,39.999538",
    city = "beijing", strategy = 2, mapkey = "71b002162d43fea08afd5b5e31b030af") {
    url_head <- "http://restapi.amap.com/v3/direction/transit/integrated?"
    url <- paste(url_head, "origin=", orgin, "&city=", city, "&destination=",
        destination, "&key=", mapkey, "&output=xml", "&strategy=", strategy,
        sep = "")

    return(url)
}
#' Get Traffic Status from amap
#'
#'
#' get traffic status from amap,there are c(description,expedite,congested,blocked,unkown)
#'
#'
#' @param type character:defalut NULL ,should be rectangle/circle/NULL
#' @param location  character:location should be writtend like
#' @param name should be 'json' or 'xml', the type of the result
#' @param radius logical value, return the coordinates or the original results
#' @param city character
#' @param citycode code of city.for examples fuzhou is 0591
#' @param key amap key
#' @param output xml or json
#' @param minione  select by shortest distance
#' @param firstone select the first one
#' @importFrom XML htmlParse
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest %>%
#' @importFrom XML xpathSApply
#' @return A vector contains description,expedite,congested,blocked,unkown
#' @export getRouteA
#' @examples
#' \dontrun{
#' ## rectangle
#' location='119.278861,26.106208;119.336454,26.078304'
#' a=getTrafficStatus(location = location,key = amap_key,type='rectangle')
#'
#' ## circle
#' location='119.278861,26.106208'
#' b=getTrafficStatus(type='circle',location = location,key = amap_key)
#'
#'## road
#'location='119.278861,26.106208'
#'c=getTrafficStatus(type='name',key = amap_key,name='古田跐?<U+3E66>')
#' }
getRouteA <- function(orgin = "116.481499,39.990475", destination = "116.465063,39.999538",
    city = "beijing", strategy = 2, minione = F, firstone = T, mapkey = "71b002162d43fea08afd5b5e31b030af") {
    url_head <- "http://restapi.amap.com/v3/direction/transit/integrated?"
    url0 <- paste(url_head, "origin=", orgin, "&city=", city, "&destination=",
        destination, "&key=", mapkey, "&output=xml", "&strategy=", strategy,
        sep = "")
    web0 <- read_html(url0, encoding = "utf-8")

    parsed_doc <- htmlParse(url0)
    path0 <- "/html/body/response/route/transits/transit/distance"
    print(xpathSApply(parsed_doc, path = path0, fun = xmlValue))
    distance_min <- which.min(xpathSApply(parsed_doc, path = path0, fun = xmlValue))
    # count=min(web0%>% html_nodes('walking_distance')%>% html_text())
    if (firstone) {
        bus_polyline <- as.character(web0 %>% html_nodes("bus polyline") %>%
            html_text())[1]
        minione <- F
    } else if (minione) {
        bus_polyline <- as.character(web0 %>% html_nodes("bus polyline") %>%
            html_text())[distance_min]
    } else {
        bus_polyline <- as.character(web0 %>% html_nodes("bus polyline") %>%
            html_text())
    }
    return(bus_polyline)
}
# url0=geturlRouteA()
# routeout=getRouteA(orgin='119.34367,26.03017',destination='119.29506,26.09337')


