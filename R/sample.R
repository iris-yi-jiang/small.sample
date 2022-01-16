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
    x <- qexp(u, rate=rate)
    return(x)
}

#' Tukey's g-and-h Points
#'
#' Generates points targeting a g-and-h distribution.
#'
#' @param n number of points to generate.
#' @param a location parameter.
#' @param b scale parameter (> 0).
#' @param g skewness parameter.
#' @param h tail-shape parameter (>= 0).
#' @return n-vector containing the sampled points.
#' @export
gen_gh <- function(n, a=0, b=1, g=0, h=0) {
    u <- gen_unif(n, l=0, u=1)
    z <- qnorm(u, mean=0, sd=1)
    if (g != 0) {
        gz <- (exp(g * z) - 1) / (g * z)
    } else {
        gz <- 1
    }
    hz <- exp((h * z ^ 2) / 2)
    x <- a + b * gz * hz * z
    return(x)
}

#' g-and-k Points
#'
#' Generates points targeting a g-and-k distribution.
#'
#' @param n number of points to generate.
#' @param a location parameter.
#' @param b scale parameter (> 0).
#' @param g skewness parameter.
#' @param k tail-shape parameter (k > -0.5).
#' @return n-vector containing the sampled points.
#' @export
gen_gk <- function(n, a=0, b=1, g=0, k=0) {
    u <- gen_unif(n, l=0, u=1)
    z <- qnorm(u, mean=0, sd=1)
    gz <- 1 + 0.8 * (1 - exp(-g * z)) / (1 + exp(-g * z))
    kz <- (1 + z ^ 2) ^ k
    x <- a + b * gz * kz * z
    return(x)
}
