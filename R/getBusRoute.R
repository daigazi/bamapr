#' get bus station and coordinate id from amap
#'
#'
#' gived bus route'name and city num code,will get the bus station with
#' id/name/coordinate/direction/bus sequence from amap
#'
#'
#' @param citycode city num code which can be found in amap api web
#' @param keywords bus route'name
#' @param gettime  bus start time and end time
#' @param getcoord bus route path coordinate
#' @param verbose  default be F
#' @return a data.frame with  id/name/coordinate/direction/bus sequence
#' @export  getBusRoute
#' @importFrom RCurl getURL
#' @importFrom curl curl_escape
#' @examples
#' \dontrun{
#' x=getBusRoute(keywords="38路")
#' }
#'
#'

getBusRoute <- function(citycode = 350100,keywords = "38",gettime=F,getcoord=F,verbose=F) {
        myheader <- c(`User-Agent` = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
                      Accept = "*/*", `Accept-Language` = "zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3",
                      Connection = "keep-alive", `Accept-Charset` = "GB2312,utf-8;q=0.7,*;q=0.7")
        keyword_url <- curl_escape(keywords)
        url_head <- "http://ditu.amap.com/service/poiInfo?query_type=TQUERY&qii=true&city="
        url <- paste(url_head, citycode, "&keywords=", keyword_url, sep = "")
        web <- getURL(url, encoding = "utf-8", httpHeader = myheader, verbose = verbose)
        a <- unlist(strsplit(web, split = "(\\},\\{)"))
        index <- grep(a, pattern = "name")
        a <- a[index]
        df <- data.frame(matrix(rep(NA, length(a) * 6), ncol = 6))
        colnames(df) <- c("ID", "lat", "lng", "stationname", "num", "direction")
        for (i in 1:length(a)) {
                b <- unlist(strsplit(a[i], split = "[\"+\\[\\{:,]"))
                c0 <- unlist(strsplit(b, split = "\\}"))
                d <- c0[grep(c0, pattern = "id"):length(c0)][c(2, 5, 7, 9, 11)]
                df[i, ] <- c(d, -1)
                # print(i)
        }
        index <- grep(df$num, pattern = "[a-zA-Z]")
        df$direction[1:(index[1] - 1)] <- 1
        df <- df[-index, ]
        df$name <- keywords


        if(gettime){
                d=c()
                for(i in 2:5){
                        a <- unlist(strsplit(web,split="start_time|end_time"))
                        b <- unlist(strsplit(a[i],split = ","))[1]
                        c0 <- unlist(strsplit(b,'"'))[3]
                        d=c(d,c0)
                }
                df1=data.frame(matrix(d,ncol=2,byrow=T))
                colnames(df1)=c("starttime","endtime")
                df1$name=keywords
                df1$direction=c(1,-1)
        }
        if(getcoord){
                a <- unlist(strsplit(web,split="path"))
                g=c()
                for(i in 2:3){
                        b <- unlist(strsplit(a[i],split = ","))
                        c0 <- gsub(pattern = '[a-zA-Z\\{\\}:\\\\"]',
                                   replacement = "",x = b)
                        index=which(nchar(c0)==0)[1]-1
                        d=c0[1:index]
                        e <- gsub(pattern = "\\]?\\[+",replacement = "",x = d)
                        f <- paste(e,collapse = ",")
                        g=c(g,f)
                }
                df2=data.frame(path=g,stringsAsFactors = F)
                df2$name=keywords
                df2$direction=c(1,-1)

        }
        if(gettime&&getcoord){
           a <- list(df,df1,df2)

        }else if(gettime){
            a <- list(df,df1)

        }else if(getcoord){
             a <- list(df,df2)
        }else{
                a <- df
        }
        return(a)
}