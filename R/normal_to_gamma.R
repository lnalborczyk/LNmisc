#' Converting a Gaussian distribution to a Gamma distribution
#'
#' Plotting two distributions. A Normal distribution and a Gamma distribution
#' with same mean and standard deviation.
#'
#' @param mu Mean
#' @param sigma Standard deviation
#'
#' @importFrom stats dnorm dgamma
#' @importFrom graphics par plot
#' @import ggplot2
#'
#' @examples
#' normal_to_gamma(mu = 5, sigma = 10)
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @export

normal_to_gamma <- function(mu, sigma) {

    if (any(c(mu, sigma) <= 0) ) stop ("mu and sigma should be positive...")

    m <- matrix(c(1, .5, 1, 1), 2)
    res <- exp(solve(m, log(c(mu, sigma) ) ) )

    par(mfrow = c(1, 2) )

    plot(
        seq(mu - 5 * sigma, mu + 5 * sigma, 0.1),
        dnorm(
            seq(mu - 5 * sigma, mu + 5 * sigma, 0.1),
            mean = mu, sd = sigma
            ),
        type = "l", lwd = 2,
        xlab = "", ylab = "",
        main = paste0("Normal(", mu, ",", sigma, ")") )

    plot(
        seq(0, res[1] * res[2] * 10, 0.1),
        dgamma(seq(0, res[1] * res[2] * 10, 0.1), shape = res[1], scale = res[2]),
        type = "l", lwd = 2,
        xlab = "", ylab = "",
        main = paste0("Gamma(", res[1], ",", res[2], ")") )

    par(mfrow = c(1, 1) )

}
