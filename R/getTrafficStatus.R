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
#' @return A vector contains description,expedite,congested,blocked,unkown
#' @export  getTrafficStatus
#' @importFrom XML htmlParse
#' @importFrom XML xpathSApply
#' @examples
#' \dontrun{
#' ## rectangle
#' location='119.278861,26.106208;119.336454,26.078304'
#' # the rectangle can't be too big,diagonal must be shorted than 10km
#' a=getTrafficStatus(location = location,key = amap_key,type='rectangle')
#'
#' ## circle
#' location='119.278861,26.106208'
#' b=getTrafficStatus(type='circle',location = location,key = amap_key)
#'
#'## road
#'location='119.278861,26.106208'
#'c=getTrafficStatus(type='name',key = amap_key,name='古田路')
#' }
getTrafficStatus <- function(type = "rectangle", location = NULL, name = NULL,
    radius = NULL, city = NULL, citycode = "350100",
    key = amap_key, output = "XML") {
    if (length(city) > 0) {
        city <-  curl_escape(city)
    }
    if (length(name) > 0) {
        name <- curl_escape(name)
    }
    if (type == "rectangle") {
        url_head <- "http://restapi.amap.com/v3/traffic/status/rectangle?rectangle="
        url <- paste(url_head, location, "&output=", output, "&key=", key,
            sep = "")
    } else if (type == "circle") {
        url_head <- "http://restapi.amap.com/v3/traffic/status/circle?location="
        url <- paste(url_head, location, "&radius=", radius, "&output=",
            output, "&key=", key, sep = "")
    } else {
        url_head <- "http://restapi.amap.com/v3/traffic/status/road?name="
        url <- paste(url_head, name, "&adcode=", citycode, "&output=",
            output, "&key=", key, sep = "")
    }
    # doc=RCurl::getURI(url) bond <-xmlParse(doc) root=xmlRoot(bond)
    parsed_doc <- htmlParse(url)
    description <- xpathSApply(parsed_doc, "/html/body/response/trafficinfo/description",
        fun = xmlValue)
    expedite <- xpathSApply(parsed_doc, "/html/body/response/trafficinfo/evaluation/expedite",
        fun = xmlValue)
    congested <- xpathSApply(parsed_doc, "/html/body/response/trafficinfo/evaluation/congested",
        fun = xmlValue)
    blocked <- xpathSApply(parsed_doc, "/html/body/response/trafficinfo/evaluation/blocked",
        fun = xmlValue)
    unknown <- xpathSApply(parsed_doc, "/html/body/response/trafficinfo/evaluation/ unknown",
        fun = xmlValue)

    traffic <- c(description, expedite, congested, blocked, unknown)
    if(length(traffic)<5){
            traffic=rep(NA,5)
            cat("url:",url,"\n")
            cat("can't get traffic status,checked the web please\n")
    }
    names(traffic) <- c("description", "expedite", "congested", "blocked",
        "unknown")
    return(traffic)
}

#' Get Traffic Status from amap from grid cell
#'
#'
#' get traffic status from amap,there are c(description,expedite,congested,blocked,unkown)
#'
#'
#' @param location  character:location should be writtend like
#' @param citycode code of city.for examples fuzhou is 0591
#' @param key amap key
#' @param gird sides of rectangle,default 5000m
#' @return A vector contains description,expedite,congested,blocked,unkown
#' @export  getTrafficStatusPlus
#' @examples
#' \dontrun{
#' location='119.229335,26.122166;119.59875,25.87036'
#' df=getTrafficStatusPlus(location = location)
#' }


getTrafficStatusPlus <- function( location = NULL,key = amap_key,grid=5000){
        old <- options(digits = 10)
        x <- as.numeric(unlist(strsplit(location,split = "[^0-9\\.]")))
        rectangel <- matrix(x,ncol=2,byrow=T)
        df_grid <- setcoordMTX(rectangel,grid=5000,sep=';')
        df <- data.frame(matrix(NA,ncol=5,nrow=length(df_grid)^2))
        for (i in 1:length(df_grid)) {
               for (j in 1:length(df_grid)) {
                  tmp_vec=getTrafficStatus(type ="rectangle",location = df_grid[i,j],
                                   key = amap_key )
                     df[j+(i-1)*8,] <-tmp_vec

                }
        }

        colnames(df) <-  c("description", "expedite", "congested", "blocked","unknown")

        df <- na.omit(df)
        df$time <- Sys.time()
        index <- grep(df[,2],pattern = "[0-9\\.%]")
        df <- df[index,]
        on.exit(options(old), add = TRUE)
        return(df)

}



