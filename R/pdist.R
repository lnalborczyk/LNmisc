#' Plotting p-value distribution
#'
#' Plotting the distribution of p-values corresponding to a two-independant groups comparison,
#' as a function of sample size and effect size.
#'
#' @param cohensd Effect size (standardised mean difference)
#' @param n Sample size (per group)
#' @param nsims Number of simulations
#' @param alpha Alpha level
#'
#' @importFrom stats rnorm t.test
#' @importFrom magrittr %>%
#' @import ggplot2
#'
#' @examples
#' pdist(cohensd = 0.5, n = 50, nsims = 1e3, alpha = .05)
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @export

pdist <- function(cohensd = 0, n = 50, nsims = 1e4, alpha = .05) {

    p <- numeric(nsims)

    for (i in 1:nsims) {

        x <- rnorm(n = n, mean = 0, sd = 1)
        y <- rnorm(n = n, mean = 0 + cohensd, sd = 1)
        z <- t.test(x, y)

        p[i] <- z$p.value

    }

    empirical_power <- (sum(p < alpha) / nsims)

    data.frame(p = p) %>%
        ggplot(aes(x = p) ) +
        stat_density(fill = "grey60") +
        geom_vline(xintercept = alpha, size = 0.7, linetype = 3) +
        labs(x = "p-value", y = "") +
        ggtitle(
            paste0(
                "p-value distribution for d = ", cohensd, " and n = ", n,
                "\npower = ", round(empirical_power, digits = 2),
                " (at alpha = ", alpha, ")")
            ) +
        theme_bw(base_size = 14)

}
