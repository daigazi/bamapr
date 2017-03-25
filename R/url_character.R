#' Transform the query character to raw character
#' Take in query and city, return the informations
#' @param a character
#' @return raw character with %. It's used in getPlace.
#' @examples
#'
#' \dontrun{
#' url_character('北京')
#' # '%e5%8c%97%e4%ba%ac'
#' }
url_character <- function(x) {
    raw <- as.character(charToRaw(x))
    paste0("%", raw, collapse = "")
}
