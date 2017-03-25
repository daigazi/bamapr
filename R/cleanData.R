# path='C:/Users/dai_jl/Desktop/nsz.txt'
#' clean geocode
#'
#'
#' clean geocode from gaodemap to a list. this list has two dataframe ,one is
#' point data,another is line data.
#'
#'
#' @param data data from the other source(gaodemap)
#' @param path data path to read
#' @param header logical,read.csv(header=F)
#' @param name  character,data is road name
#' @param direction character,data's direction
#' @param sam logical while true,will sample the color of gecodata
#' @param npoint numeric sample num
#' @param to 5 or 6. See more in details.
#' @details Go to http://developer.baidu.com/map/index.php?title=webapi/guide/changeposition to see what the intergers mean.
#' @export cleanData
cleanData <- function(data = NULL, path, header = F, name = "sanhuang", 
    direction = "nsz", sam = T, npoint = 4, from = 3, to = 5) {
    
    pointdat <- read.csv(file = path, header = F, stringsAsFactors = F, 
        encoding = "UTF-8")
    omit_vec <- grep(pattern = "[[]", x = pointdat[, 1])
    pointdat <- pointdat[omit_vec, ]
    names(pointdat) <- c("Lon", "Lat")
    pointdat$Lon <- as.numeric(gsub(pattern = "[[]", x = pointdat[, 1], 
        ""))
    pointdat$Lat <- as.numeric(gsub(pattern = "[]]", x = pointdat[, 2], 
        ""))
    
    # pointdat[,1:2]=apply(pointdat[,1:2],1,function(x)
    # {geoconv(x,from=from,to=to)})
    pointdat[, 1:2] <- geoconv(pointdat[, 1:2], from = from, to = to)
    pointdat$index <- paste(name, 1:nrow(pointdat), sep = "")
    pointdat$name <- name
    pointdat$direction <- direction
    
    
    # lineData
    linedat <- data.frame(origin = pointdat$index[1:(nrow(pointdat) - 1)], 
        destination = c(pointdat$index[2:nrow(pointdat)]))
    
    if (sam) {
        sam_index <- sample(2:(nrow(linedat) - 1), npoint)
    }
    sam_index <- sam_index[order(sam_index, decreasing = F)]
    sam_index1 <- c(1, sam_index, nrow(linedat) + 1)
    sam_index2 <- sam_index1 - 1
    sam_vec <- sam_index2[2:length(sam_index2)] - sam_index2[1:(length(sam_index2) - 
        1)]
    
    color <- sample(c("#FF0000", "#EEEE00", "#00EE00"), size = npoint + 
        1, replace = T)
    color_vec <- rep(color, sam_vec)
    linedat$color <- color_vec
    linedat$symbol <- "#FFFFFF"
    symbol <- sample(c("#FF0000", "#EEEE00", "#00EE00"), size = npoint, 
        replace = T)
    linedat$symbol[sam_index] <- symbol
    
    dat <- list(point = pointdat, line = linedat)
    if (is.null(data)) {
        return(dat)
    } else {
        if (all(names(data$point) == names(dat$point))) {
            pointdat <- rbind(data$point, dat$point)
            linedat <- rbind(data$line, dat$line)
            dat <- list(point = pointdat, line = linedat)
            return(dat)
        } else {
            stop("data's colnames must be (Lon,Lat,index,name,direction,color,symbol)")
        }
        
    }
    
}



