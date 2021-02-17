#' This function calculates the variance to mean ratio (AKA dispersion index)
#' @param x is a numeric vector
#' @export

dispersion <- function(x) {
  var(x) / mean(x)
}
