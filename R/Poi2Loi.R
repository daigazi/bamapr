#' change POI data to LOI(line of interest)
#'
#'
#' change bus station poi to bus route LOI
#'
#'
#' @param data data from the other source(gaodemap)
#' @param strsplitcol data path to read
#' @param splitcha logical,read.csv(header=F)
#' @param to 5 or 6. See more in details.
#' @export Poi2Loi
#' @examples
#'
#' \dontrun{
#' data(bus)
#' df=Poi2Loi(bus,strsplitcol='address',splitcha='[;/.]')
#' }



Poi2Loi <- function(data, strsplitcol = "address", splitcha = "[;/.]") {
    
    tmp_df <- data.frame(matrix(c(rep(NA, ncol(data))), ncol = ncol(data)))
    colnames(tmp_df) <- colnames(data)
    for (i in 1:nrow(data)) {
        funcha <- paste("unlist(strsplit(data$", strsplitcol, "[i],split='", 
            splitcha, "'))", sep = "")
        
        tmp_vec <- eval(parse(text = funcha))
        N <- length(tmp_vec)
        tmp_df0 <- data.frame(matrix(c(rep(NA, N * 5)), ncol = 5))
        colnames(tmp_df0) <- colnames(data)
        for (j in 1:N) {
            tmp_df0[j, ] <- data[i, ]
        }
        tmp_df0[strsplitcol] <- tmp_vec
        tmp_df <- rbind(tmp_df, tmp_df0)
    }
    tmp_df <- na.omit(tmp_df)
    return(tmp_df)
}

#' transform Lontitude and latitude to meter
#'
#'
#' transform Lontitude and latitude to meter
#'
#'
#' @param mtx data to transform
#' @export loc2m
#' @examples
#' \dontrun{
#' xy=matrix(c(119.328934,26.101757,119.328957,26.101746),ncol=2,byrow=T)
#' loc2m(xy)
#' }
#'
loc2m <- function(mtx) {
    if (dim(mtx)[2] == 2) {
        if (is.matrix(mtx)) {
            mtx <- as.data.frame(mtx)
        }
        N <- nrow(mtx) - 1
        meter <- c()
        for (i in 1:N) {
            meter0 <- round(distm(mtx[i, ], mtx[i + 1, ], fun = distVincentyEllipsoid), 
                1)
            meter <- c(meter, meter0)
        }
        mtx$meter <- c(meter, 0)
    } else {
        stop("matrix must be two columns ")
    }
    return(mtx)
}


#'
#'
#'
#' transform Lontitude and latitude to meter
#'
#'
#' @param mtx colnames(mtx)=c('lng','lat')
#' @export Roadgrid
#' @examples
#' \dontrun{
#' mtx=matrix(c(119.328934,26.101757,119.328957,26.101746),ncol=2,byrow=T)
#' mtx0=Roadgrid(mtx)
#' }
#'
Roadgrid <- function(mtx, grid = 50) {
    old <- options(digits = 10)
    grid <- grid/111000
    if (dim(mtx)[2] == 2) {
        if (is.matrix(mtx)) {
            mtx <- as.data.frame(mtx)
        }
        N <- nrow(mtx) - 1
        df <- data.frame(matrix(rep(NA, 4 * N + 4), ncol = 4))
        colnames(df) <- c("lng_left", "lat_left", "lng_right", "lat_right")
        for (i in 1:N) {
            x0 <- mtx[i, 1]
            x1 <- mtx[i + 1, 1]
            y0 <- mtx[i, 2]
            y1 <- mtx[i + 1, 2]
            lat <- abs(y0 - y1)
            lng <- abs(x0 - x1)
            z <- sqrt(lat^2 + lng^2)
            sina <- (lat)/z
            cosa <- (lng)/z
            
            
            x00 <- x0 + sina * grid
            y00 <- y0 - cosa * grid
            x01 <- x0 - sina * grid
            y01 <- y0 + cosa * grid
            df[i, ] <- c(x01, y01, x00, y00)
            
            if (i == N) {
                x10 <- x1 + sina * grid
                y10 <- y1 - cosa * grid
                x11 <- x1 - sina * grid
                y11 <- y1 + cosa * grid
                df[i + 1, ] <- c(x11, y11, x10, y10)
            }
        }
    } else {
        stop("matrix must be two columns ")
    }
    mtx <- cbind(mtx, df)
    on.exit(options(old), add = TRUE)
    return(mtx)
}


