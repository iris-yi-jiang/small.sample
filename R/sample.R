#' Uniform Points
#'
#' Generates points targeting a uniform distribution.
#'
#' @param n number of points to generate.
#' @param l lower bound.
#' @param u upper bound.
#' @return n-vector containing the sampled points.
#' @export
gen_unif <- function(n, l=0, u=1) {
    x <- c(NA)
    length(x) <- n
    b <- seq(from=l, to=u, length=(n + 1))
    for (i in 1:n) {
        x[i] <- runif(n=1, min=b[i], max=b[i + 1])
    }
    return(sample(x))
}

#' Normal Points
#'
#' Generates points targeting a normal distribution.
#'
#' @param n number of points to generate.
#' @param mu mean.
#' @param sig standard deviation.
#' @return n-vector containing the sampled points.
#' @export
gen_norm <- function(n, mu=0, sig=1) {
    u <- gen_unif(n, l=0, u=1)
    x <- qnorm(u, mean=mu, sd=sig)
    return(x)
}

#' Chi-Squared Points
#'
#' Generates points targeting a chi-squared distribution.
#'
#' @param n number of points to generate.
#' @param df degrees-of-freedom.
#' @return n-vector containing the sampled points.
#' @export
gen_chisq <- function(n, df) {
    u <- gen_unif(n, l=0, u=1)
    x <- qchisq(u, df=df)
    return(x)
}

#' Exponential Points
#'
#' Generates points targeting an Exponential distribution.
#'
#' @param n number of points to generate.
#' @param rate rate parameter.
#' @return n-vector containing the sampled points.
#' @export
gen_exp <- function(n, rate=1) {
    u <- gen_unif(n, l=0, u=1)
    x <- qexq(u, rate=rate)
    return(x)
}
