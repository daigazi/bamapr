#' polyline2mtx
#'
#'
#' polyline location to matrix type
#'
#'
#' @param polyline polyline location
#' @return matrix
#' @export  polyline2mtx
#' @examples
#' \dontrun{
#' mtx=polyline2mtx(routeout)
#' }
#'
#'
polyline2mtx <- function(polyline, split = ";") {
        split_char <- paste("[", split, "]", sep = "")
        str_vec <- unlist(strsplit(polyline, split = split_char))
        str_vec <- unlist(strsplit(str_vec, split = ","))
        df <- data.frame(lon = str_vec[seq(1, length(str_vec), 2)], lat = str_vec[seq(2,
                                                                                      length(str_vec), 2)], stringsAsFactors = F)
        df$lon <- as.numeric(df$lon)
        df$lat <- as.numeric(df$lat)
        return(df)
}
# mtx=polyline2mtx(routeout)




#' mtx2geoData
#'
#'
#' matirx data to geoData to use remapB
#'
#'
#' @param mtx matrix
#' @param name character
#' @return data.frame
#' @export  mtx2geoData
#' @examples
#' \dontrun{
#' geoData=mtx2geoData(mtx)
#' }
#'
#'
mtx2geoData <- function(mtx, name = "point") {
        df <- as.data.frame(mtx, stringsAsFactors = F)
        df[,1]=as.numeric(df[,1])
        df[,2]=as.numeric(df[,2])
        df$name <- paste(name, "_", 1:nrow(df), "point", sep = "")
        return(df)
}


#' mtx2lineData
#'
#'
#' matirx data to lineData to use remapB
#'
#'
#' @param mtx matrix
#' @param name character
#' @param color character,used to set line theme
#' @return data.frame
#' @export  mtx2lineData
#' @examples
#' \dontrun{
#' lineData=mtx2lineData(mtx,color ='blue')
#' }
#'
#'
mtx2lineData <- function(mtx, name = "point", color = NULL) {
        df <- mtx2geoData(mtx, name)
        origin_vec <- c(1:(nrow(df) - 1))
        destination_vec <- c(2:nrow(mtx))
        if (length(color) == 0) {
                df <- data.frame(origin = df$name[origin_vec],
                                 destination = df$name[destination_vec],
                                 stringsAsFactors = F)

        } else {
                df <- data.frame(origin = df$name[origin_vec],
                                 destination = df$name[destination_vec],
                                 color = rep(color, length(origin_vec)),
                                 stringsAsFactors = F)

        }

        return(df)
}


