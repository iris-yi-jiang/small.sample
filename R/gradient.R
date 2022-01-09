#' Normal Gradient
#'
#' The logarithmic derivative of the normal density function.
#'
#' @param x point at which the gradient is computed.
#' @param mu mean.
#' @param sig standard deviation.
#' @return gradient of the log of the normal PDF.
grad_norm <- function(x, mu=0, sig=1) {
    return(-sig ^ 2 * (x - mu))
}

#' Chi-Squared Gradient
#'
#' The logarithmic derivative of the chi-squared density function.
#'
#' @param x point at which the gradient is computed.
#' @param df degrees-of-freedom.
#' @return gradient of the log of the chi-squared PDF.
grad_chisq <- function(x, df) {
    return(df / (2 * x) - 1 / x - 0.5)
}
