#' cut map rectangel to small cell
#'
#'
#' for example rectangel=matrix(c(119.232566,26.131948,119.47288,25.941584),byrow = T,nrow = 2),
#' but amap and baidumap can only search POI in a small cell ,so we need to cut big
#' one to many samll cell
#'
#'
#' @param rectangel matrix:
#' @param grid  numeic: samll cell length
#' @param sep see detail
#' @export setcoordMTX
#' @return a data.frame
#' @examples
#' \dontrun{
#' ## rectangle
#'
#' rectangel=matrix(c(119.281777,26.08224,119.324092,26.059111),byrow = T,nrow = 2)
#' df_grid=setcoordMTX(rectangel=rectangel,grid=5000,sep=';')
#' }

setcoordMTX <- function(rectangel, grid = 5000, sep = ";") {
    if (all(dim(rectangel) == c(2, 2))) {
        if (is.matrix(rectangel)) {
            x <- grid/111000
            grid <- round(x, digits = 4)
            lon_diff <- abs(as.numeric(rectangel[2, 1]) - as.numeric(rectangel[1, 
                1]))
            lat_diff <- abs(as.numeric(rectangel[2, 2]) - as.numeric(rectangel[1, 
                2]))
            if (grid > max(c(lon_diff, lat_diff))) {
                num1 <- paste(rectangel[1, 1], rectangel[1, 2], sep = ",")
                num2 <- paste(rectangel[2, 1], rectangel[2, 2], sep = ",")
                df_grid2 <- matrix(paste(c(num1, num2), collapse = sep))
            } else {
                lon_vec <- seq(from = rectangel[1, 1], to = rectangel[2, 
                  1], grid)
                lat_vec <- seq(rectangel[1, 2], rectangel[2, 2], length.out = length(lon_vec))
                df_grid <- data.frame(matrix(NA, ncol = length(lat_vec), 
                  nrow = length(lat_vec)))
                for (i in 1:length(lat_vec)) {
                  for (j in 1:length(lat_vec)) {
                    df_grid[i, j] <- paste(lon_vec[i], lat_vec[j], sep = ",")
                  }
                }
                df_grid2 <- data.frame(matrix(NA, ncol = length(lat_vec) - 
                  1, nrow = length(lat_vec) - 1))
                for (i in 1:c(length(lat_vec) - 1)) {
                  for (j in 1:c(length(lat_vec) - 1)) {
                    df_grid2[i, j] <- paste(df_grid[i, j], df_grid[i + 
                      1, j + 1], sep = sep)
                  }
                }
            }
            
            return(df_grid2)
        }
    } else {
        cat("rectangel must be data.frame or matrix,and two cols two rows")
    }
    
}
