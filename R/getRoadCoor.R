#' get road ployline coordinate by road id from amap
#'
#'
#' get road ployline from amap,default will convert to baidu coordinate,and will
#' convert to linedata and pointdata to show within REmap::remapb()
#'
#'
#' @param id road'id,can get by function getSearchPlus
#' @param tobaidu  whether convert amap to baidu coodinate,default to be T
#' @return a list within linedata and geodata
#' @export  getRoadCoor
#' @importFrom RCurl getURL
#' @examples
#' \dontrun{
#' rectangel=matrix(c(120.208958,27.327244,120.232389,27.30612),byrow = T,nrow = 2) #test
#' df=getSearchPlus(mtx = rectangel,word = '道路',types = '19',Tobaidu = F)
#' str(df)
#' x=getroadcoor()
#'  }

getRoadCoor <- function(id = "B0FFGQOZO5", tobaidu = T,name="hhh") {
        myheader <- c(`User-Agent` = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
                      Accept = "*/*", `Accept-Language` = "zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3",
                      Connection = "keep-alive", `Accept-Charset` = "GB2312,utf-8;q=0.7,*;q=0.7")
    url_head <- "http://ditu.amap.com/service/poiInfo?query_type=IDQ&id="
    url <- paste(url_head, id, sep = "")
    web <- getURL(url, encoding = "utf-8",httpHeader = myheader)
    tmp <- unlist(strsplit(web, split = "\\{"))
    N <- grep(tmp, pattern = "roadaoi")
    aoi <- unlist(strsplit(tmp[N], "\""))[4]
    if(length(aoi)>0){
            aoi0 <- unlist(strsplit(aoi, split = "\\|"))
            geodata <- data.frame(matrix(rep(NA, 3), ncol = 3))
            colnames(geodata) <- c("V1", "V2", "name")
            linedata <- data.frame(matrix(rep(NA, 2), ncol = 2))
            colnames(linedata) <- c("origin", "destination")
            for (i in 1:length(aoi0)) {
                    rec <- matrix(unlist(strsplit(aoi0[i], split = "[_,]")), ncol = 2,
                                  byrow = T)

                    if (tobaidu)
                            rec <- geoconv(rec)
                    geo_tmp <- mtx2geoData(mtx = rec, name = paste(name,i, "line", sep = ""))
                    line_tmp <- mtx2lineData(mtx = rec, name = paste(name,i, "line", sep = ""))
                    geodata <- rbind(geodata, geo_tmp)
                    linedata <- rbind(linedata, line_tmp)
                    geodata <- na.omit(geodata)
                    linedata <- na.omit(linedata)
            }
    }else{
            geodata <- data.frame(matrix(rep(NA, 3), ncol = 3))
            colnames(geodata) <- c("V1", "V2", "name")
            linedata <- data.frame(matrix(rep(NA, 2), ncol = 2))
            colnames(linedata) <- c("origin", "destination")
    }


    return(data = list(geodata = geodata, linedata = linedata))
}
