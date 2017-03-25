amap_key <- "71b002162d43fea08afd5b5e31b030af"
#' get search API url
#'
#'
#' search something from amap,you can see http://lbs.amap.com/api/webservice/guide/api/trafficstatus/#inputtips
#'
#'
#' @param shape character:defalut NULL ,should be rectangle/circle/NULL
#' @param location  character:location should be writtend like
#' @param keywords should be 'json' or 'xml', the type of the result
#' @param offset code of city.for examples fuzhou is 0591
#' @param polygon polygon of location
#' @param radius logical value, return the coordinates or the original results
#' @param city character
#' @param mapkey amap key
#' @param extensions xml or json
#' @param page num
#' @return A vector contains description,expedite,congested,blocked,unkown
#' @export  geturl0
#' @examples
#' \dontrun{
#'rectangel=matrix(c(119.117295,26.201212,119.649092,25.840692),byrow = T,nrow = 2)
#'df_grid=setcoordMTX(rectangel=rectangel,grid=5000,sep=';')
#'amap_key='71b002162d43fea08afd5b5e31b030af'
#'url=geturl0(shape='polygon',polygon=df_grid[1,1],mapkey=amap_key,location=NULL,keywords='公交??<U+393C><U+3E39>')
#'url=geturl0(shape='polygon',polygon=df_grid[1,1],mapkey=amap_key,location=NULL,keywords='公交站|公交车站')
#'url=geturl0(shape='polygon',polygon=df_grid[1,1],mapkey=amap_key,location=NULL,keywords='道路',types='地名地址信息')
#'  }

geturl0 <- function(page = 1, shape = "around", location = "116.456299,39.960767",
    keywords = NULL, offset = 25, polygon = NULL, radius = 2000, city = NULL,
    mapkey = "71b002162d43fea08afd5b5e31b030af", types = "地名地址信息",
    extensions = "base") {
    url_head <- "http://restapi.amap.com/v3/place/"
    if (shape == "text") {
        if (length(city) == 0) {
            url <- paste(url_head, "text?&keywords=", keywords, "&output=xml&offset=",
                offset, "&page=", page, "&key=", mapkey, "&extensions=",
                extensions, "&location=", location, "&types=", types, sep = "")
        } else {
            url <- paste(url_head, "text?&keywords=", keywords, "&city=",
                city, "&output=xml&offset=", offset, "&page=", page, "&key=",
                mapkey, "&extensions=", extensions, "&types=", types, sep = "")
        }

    } else if (shape == "around") {
        url <- paste(url_head, "around?&keywords=", keywords, "&output=xml&offset=",
            offset, "&page=", page, "&key=", mapkey, "&extensions=", extensions,
            "&location=", location, "&types=", types, sep = "")
    } else if (shape == "polygon") {
        url <- paste(url_head, "polygon?&keywords=", keywords, "&city=",
            city, "&output=xml&offset=", offset, "&page=", page, "&key=",
            mapkey, "&extensions=", extensions, "&polygon=", polygon, "&types=",
            types, sep = "")
    } else {
        cat("shape must in text around polygon", "\n")
    }
    return(url)
}

#' search something from amap
#'
#'
#' search something from amap,you can see http://lbs.amap.com/api/webservice/guide/api/trafficstatus/#inputtips
#'
#'
#' @param shape character:defalut NULL ,should be rectangle/circle/NULL
#' @param location  character:location should be writtend like
#' @param keywords should be 'json' or 'xml', the type of the result
#' @param offset code of city.for examples fuzhou is 0591
#' @param polygon polygon of location
#' @param radius logical value, return the coordinates or the original results
#' @param city character
#' @param key amap key
#' @param extensions xml or json
#' @return A vector contains description,expedite,congested,blocked,unkown
#' @export  getSearch
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom rvest %>%
#' @examples
#' \dontrun{
#'rectangel=matrix(c(118.784364,26.478515,119.892801,25.436929),byrow = T,nrow = 2) #big
#'rectangel=matrix(c(119.117295,26.201212,119.649092,25.840692),byrow = T,nrow = 2) #saml
#'df_grid=setcoordMTX(rectangel=rectangel,grid=2000,sep=';')
#' }




getSearch <- function(shape = "polygon", location = "116.456299,39.960767",
    keywords = NULL, offset = 25, polygon = NULL, radius = 10000, city = NULL,
    key = amap_key, extensions = "base", types ="地名地址信息") {

    url_tmp <- geturl0(page = 0, shape, location, keywords, offset, polygon,
        radius, city, mapkey = key, types=types)
    web <- read_html(url_tmp, encoding = "utf-8")
    info <- web %>% html_nodes("info") %>% html_text()
    # if(info=="DAILY_QUERY_OVER_LIMIT"){
    #         amap_key <- "71b002162d43fea08afd5b5e31b030af"
    # }
    count <- web %>% html_nodes("count") %>% html_text()
    if (length(count) > 0) {
        count <- as.numeric(web %>% html_nodes("count") %>% html_text())
        pagenum <- ceiling(count/offset)
        tmp <- data.frame(matrix(rep(NA,6), ncol = 6))
        colnames(tmp) <- c("id","name", "location", "address","cityname","adname")
        for (i in 1:pagenum) {
            url_tmp <- geturl0(page = i, shape, location, keywords, offset,
                polygon, radius, city, mapkey = key,types = types)
            web <- read_html(url_tmp, encoding = "utf-8")
            df <- data.frame(
                    id=web %>% html_nodes("id") %>% html_text(),
                    name = web %>% html_nodes("name") %>% html_text(),
                location = web %>% html_nodes("location") %>% html_text(),
                address = web %>% html_nodes("address") %>% html_text(),
                cityname = web %>% html_nodes("cityname") %>% html_text(),
                adname = web %>% html_nodes("adname") %>% html_text(),
                stringsAsFactors = F)
            tmp <- rbind(tmp, df)
        }
        tmp <- tmp[-1, ]
    } else {
        tmp <- data.frame(matrix(rep(NA, 6), ncol = 6))
        colnames(tmp) <- c("id","name", "location", "address","cityname","adname")
    }


    return(tmp)

}


#' search something from amap
#'
#'
#' search something from amap,you can see http://lbs.amap.com/api/webservice/guide/api/trafficstatus/#inputtips
#'
#'
#' @param mtx character:defalut NULL ,should be rectangle/circle/NULL
#' @param grid  character:location should be writtend like
#' @param word should be 'json' or 'xml', the type of the result
#' @param Tobaidu code of city.for examples fuzhou is 0591
#' @export  getSearchPlus
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom rvest %>%
getSearchPlus <- function(mtx, grid = 5000, word = "bicycle", Tobaidu = T,
    types = "地名地址信息") {
    df_grid <- setcoordMTX(rectangel = mtx, grid, sep = ";")
    # print(df_grid)
    df <- data.frame(matrix(rep(NA, 6), nrow = 1))
    colnames(df) <- c("id","name", "location", "address","cityname","adname")
    for (i in 1:length(df_grid)) {
         #print(i)
        for (j in 1:length(df_grid)) {
            df_tmp <- getSearch(shape = "polygon", polygon = df_grid[i,
                j], key = amap_key, keywords = word, types=types)
            df <- rbind(df, df_tmp)
             #print(j)
        }
    }
    df <- na.omit(df)
    df <- df[!duplicated(df), ]
    if(nrow(df)>0){
            a <- unlist(strsplit(df$location, split = ","))
            df$lon <- a[seq(1, length(a), 2)]
            df$lat <- a[seq(2, length(a), 2)]
            if (Tobaidu) {
                    df_baidu <- geoconv(df[, c("lon", "lat")])
                    df$lon <- df_baidu[, 1]
                    df$lat <- df_baidu[, 2]
            }
    }

    return(df)
}







