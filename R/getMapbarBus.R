#' get MapBar Bus from 'http://bus.mapbar.com/'
#'
#'
#' get MapBar Bus from 'http://bus.mapbar.com/'
#'
#'
#' @param url address
#' @param Ch cause tmcn::toPinyin's bug,some chinese word need to be changed to pinyin
#' @param Py pinyin
#' @return df  bus station
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom tmcn toPinyin
#' @export getMapBarBus
#' @examples
#' df=getMapBarBus(url='http://bus.mapbar.com/fuzhou1/xianlu/',
#'  Ch=c('地铁','埔尾','幑�<U+3E66>','巑�<U+3E37>'),
#'  Py=c('ditie','puwei','guang','xiang'))
#'
#'
#'  df=getMapBarBus()
getMapBarBus <- function(url = "http://bus.mapbar.com/fuzhou1/xianlu/", 
    Ch = NULL, Py = c("ditie", "puwei", "guang", "xiang")) {
    web <- read_html(url, encoding = "utf-8")
    a <- web %>% html_nodes("dd a") %>% html_text()
    a_py <- a[1:(length(a) - 2)]  #delete '图吧导航' and '彩虹公交'
    for (i in 1:length(Ch)) {
        a_py <- gsub(Ch[i], Py[i], x = a_py)
    }
    a_py <- toPinyin(a_py)
    station_df <- data.frame(matrix(c(NA, NA), ncol = 2))
    colnames(station_df) <- c("route", "station")
    url_head <- url
    for (i in 1:length(a_py)) {
        url_tmp <- paste(url_head, a_py[i], "/", sep = "")
        tryCatch(expr = {
            web_tmp <- read_html(url_tmp, encoding = "utf-8")
            # print(i)
        }, error = function(e) {
            print(paste("ERROR NUM:", i, sep = ""))
            cat("ERROR :", conditionMessage(e), "\n")
        }, finally = {
            if (exists("web_tmp")) {
                station_tmp <- web_tmp %>% html_nodes("#scrollTr a") %>% 
                  html_text()
                station_tmp <- gsub(pattern = "[0-9]", replacement = "", 
                  x = station_tmp)
                df_tmp <- data.frame(route = rep(a[i], length(station_tmp)), 
                  station = station_tmp)
            } else {
                df_tmp <- data.frame(route = rep(a[i], 1), station = NA)
            }
        })
        station_df <- rbind(station_df, df_tmp)
    }
    station_df <- na.omit(station_df)
    return(station_df)
}








