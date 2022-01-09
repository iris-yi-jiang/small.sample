#' Normal Points
#'
#' Generates points targeting a normal distribution.
#'
#' @param n number of points to generate.
#' @param mu mean.
#' @param sig standard deviation.
#' @param m positive scalar such that n points are selected from an
#'          initial set of (m * n) points by Stein Thinning.
#' @return n-vector containing the sampled selected points.
#' @export
gen_norm <- function(n, mu=0, sig=1, m=20) {
    x <- rnorm(m * n, mean=mu, sd=sig)
    dim(x) <- c(length(x), 1)
    g <- grad_norm(x, mu=mu, sig=sig)
    idx <- stein.thinning::thin(x, g, n)
    return(x[idx,])
}

#' Chi-Squared Points
#'
#' Generates points targeting a chi-squared distribution.
#'
#' @param n number of points to generate.
#' @param df degrees-of-freedom (must be greater than 2).
#' @param m positive scalar such that n points are selected from an
#'          initial set of (m * n) points by Stein Thinning.
#' @return n-vector containing the sampled selected points.
#' @export
gen_chisq <- function(n, df, m=20) {
    if (df < 3) {
        stop("df < 3 is not yet supported.")
    }
    x <- rchisq(m * n, df=df)
    dim(x) <- c(length(x), 1)
    g <- grad_chisq(x, df=df)
    idx <- stein.thinning::thin(x, g, n)
    return(x[idx,])
}
