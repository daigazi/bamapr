% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRoute.R
\name{getRoute}
\alias{getRoute}
\title{Get route from query}
\usage{
getRoute(...)
}
\arguments{
\item{origin}{the original location}

\item{destination}{the destination}

\item{mode}{'driving'(default), 'walking', or 'transit'}

\item{region}{the city of of original location and destination.
If original and destination is not in the same city,
set `origin_region` and `destination_region` seperately.}

\item{origin_region}{the city of original location. If not set, use `region` instead.}

\item{destination_region}{the city of destination. If not set, use `region` instead.}

\item{tactics}{10(no expressway), 11(default, shortest time), 12(shortest path).}

\item{coord_type}{'bd09ll'(default), 'gcj02'(which Google map and Soso map are using), 'wgs84' for GPS devices.}
}
\value{
a data frame contains longtitude and latitude of the route.
}
\description{
Take in a original location and destination, return the direction
}
\examples{
\dontrun{
bjMap = getBaiduMap('鍖椾吐�<U+3E62>', color='bw')
df = getRoute('棣栭兘鍥介檯鏈哄満', '鍖椾含鍗楄嫅鏈哄満', region = '鍖椾吐�<U+3E62>')
ggmap(bjMap) + geom_path(data = df, aes(lon, lat), alpha = 0.5, col = 'red')
}

}

