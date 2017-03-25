#' Convert geocode
#'
#'
#' Take in geocode from the other source to baidu's geocode
#'
#'
#' @param geocde geocode from the other source,limit 100 geo point once
#' @param from takes intergers from 1 to 8. See more in details.
#' @param to 5 or 6. See more in details.
#' @details Go to http://developer.baidu.com/map/index.php?title=webapi/guide/changeposition to see what the intergers mean.
#' @importFrom rjson fromJSON
#' @importFrom RCurl getURL
#' @export geoconv
#' @examples
#'
#' vec=geoconv(geocode=c(119.348557,25.996298),from=3,to=5)
#'
#' mtx=matrix(c(119.348557,25.996298,119.350449,25.997036),byrow=T,ncol=2)
#' geo_mtx=geoconv(mtx)
#'
#' df=as.data.frame(mtx)
#' geo_df=geoconv(df)


geoconv <- function(geocode, from = 3, to = 5) {
    if (class(geocode) %in% c("data.frame", "matrix")) {
        loopNum <- ceiling(nrow(geocode)/100)  #the geocode point only 100 limited
        url_header <- "http://api.map.baidu.com/geoconv/v1/?coords="
        result_matrix <- matrix(NA, 1, 2)
        for (i in 1:loopNum) {
            if (i < loopNum) {
                index <- c(1:100) + (i - 1) * 100
                
            } else {
                index <- ((i - 1) * 100 + 1):nrow(geocode)
            }
            tmp <- geocode[index, ]
            tmp <- as.matrix(tmp)
            code <- apply(tmp, 1, function(x) paste0(x[1], ",", x[2]))
            code_url <- paste0(code, ";", collapse = "")
            code_url <- substr(code_url, 1, nchar(code_url) - 1)
            url <- paste0(url_header, code_url, "&from=", from, "&to=", 
                to, "&ak=", map_ak, collapse = "")
            result <- fromJSON(getURL(url))
            tmp <- sapply(result$result, function(t) c(t$x, t$y))
            tmp <- t(tmp)
            result_matrix <- rbind(result_matrix, tmp)
        }
        result_matrix <- result_matrix[-1, ]
    } else if (length(geocode) == 2) {
        code_url <- paste0(geocode[1], ",", geocode[2])
        url <- paste0(url_header, code_url, "&from=", from, "&to=", to, 
            "&ak=", map_ak, collapse = "")
        result <- fromJSON(getURL(url))
        result_matrix <- sapply(result$result, function(t) c(t$x, t$y))
        result_matrix <- t(result_matrix)
    } else {
        stop("Wrong geocodes!")
    }
    
    
    
    return(result_matrix)
}
