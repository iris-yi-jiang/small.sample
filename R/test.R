#' Test
#'
#' Plots the moments of the generated samples.
#'
#' @return list of matrices of sample moments:
#' \itemize{
#'   \item ss - first four moments of samples from \code{gen_norm}.
#'   \item id - first four moments of samples from \code{rnorm}.
#' }
#' @export
test <- function() {
    m = 10000
    n = 30
    ss <- matrix(NA, nrow=m, ncol=4)
    id <- matrix(NA, nrow=m, ncol=4)

    for (i in 1:m) {
        x_ss <- gen_norm(n)
        ss[i, 1] <- mean(x_ss)
        ss[i, 2] <- var(x_ss)
        ss[i, 3] <- moments::skewness(x_ss)
        ss[i, 4] <- moments::kurtosis(x_ss)

        x_id <- rnorm(n)
        id[i, 1] <- mean(x_id)
        id[i, 2] <- var(x_id)
        id[i, 3] <- moments::skewness(x_id)
        id[i, 4] <- moments::kurtosis(x_id)
    }

    png("README_FIG.png", width=9, height=4.5, units="in", res=300)
    par(mfrow=c(1, 2))
    plot(id[, 1:2], pch=20, col="grey", xlab="Mean", ylab="Variance")
    points(ss[, 1:2], pch=20, col="red")
    legend("topleft",
        legend=c("Independent", "small.sample"),
        horiz=TRUE,
        xpd=TRUE,
        inset=c(0, -0.15),
        bty="n",
        col=c("grey", "red"),
        pch=20)
    plot(id[, 3:4], pch=20, col="grey", xlab="Skewness", ylab="Kurtosis")
    points(ss[, 3:4], pch=20, col="red")
    dev.off()

    return(list("ss"=ss, "id"=id))
}
