#' Modified Brinley's plots
#'
#' Examining within-subject effects using a modified Brinley plot (Blampied, 2017).
#'
#' @param data A dataframe containing all relevant variables
#' @param formula A simple formula of the form y ~ x (nb: with only one predictor)
#' @param facet A facet... (should be set to NULL otherwise)
#' @param colour A colour... (should be set to NULL otherwise)
#' @param background Should the whole dataset be plotted in background ?
#'
#' @importFrom effsize cohen.d
#' @importFrom magrittr %>%
#' @importFrom Rmisc CI
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' \dontrun{
#' data(brinley_data)
#'
#' brinley(
#' brinley_data, formula = outcome ~ session, facet = "condition", colour = "pain",
#' background = FALSE)
#'
#' data(dominance)
#' brinley(data = dominance, formula = value ~ group, facet = "exp")
#' }
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @references Blampied, N. M. (2017). Analyzing Therapeutic Change Using
#' Modified Brinley Plots: History, Construction, and Interpretation. Behavior
#' Therapy, 48, 115-117.
#'
#' @export

brinley <- function(data, formula, facet = NULL, colour = NULL, background = TRUE) {

    # extracting right and left hand sides of the formula

    lhs <- all.vars(formula)[1]
    rhs <- all.vars(formula)[2]

    # extracting levels of rhs factor

    lev <-
        sapply(data, levels)[as.character(rhs)] %>%
        unlist %>%
        as.character

    # re-arranging dataframe

    df <-
        data %>%
        spread(key = rhs, value = lhs)

    # computing mean and confidence interval by group

    df2 <-
        data %>%
        select(
            all.vars(formula),
            ifelse(!is.null(facet), facet, all.vars(formula) ) ) %>%
        group_by_(
            all.vars(formula)[2],
            ifelse(!is.null(facet), facet, all.vars(formula)[2] ) ) %>%
        dplyr::summarise(
            m = mean(eval(as.name(lhs) ) ),
            lower = CI(eval(as.name(lhs) ) )[3],
            upper = CI(eval(as.name(lhs) ) )[1]
        ) %>%
        data.frame

    # computing mean and confidence interval by group

    means <-
        df2 %>%
        select(-lower, -upper) %>%
        spread(key = rhs, value = m, sep = "_")

    lower <-
        df2 %>%
        select(-m, -upper) %>%
        spread(key = rhs, value = lower, sep = "l")

    upper <-
        df2 %>%
        select(-lower, -m) %>%
        spread(key = rhs, value = upper, sep = "u")

    if (!is.null(facet) ) {

        sums <- left_join(means, lower, by = facet)
        sums <- left_join(sums, upper, by = facet)

    } else {

        sums <- cbind(means, lower, upper)

    }

    # computing effect size (cohen's d average)

    dav <-
        data %>%
        {if(!is.null(facet) ) group_by_(., facet) else .} %>%
        dplyr::summarise(
            dav = cohen.d(
                d = get(lhs), f = get(rhs), paired = FALSE, pooled = TRUE)$estimate,
            dl = cohen.d(
                d = get(lhs), f = get(rhs), paired = FALSE, pooled = TRUE)$conf.int[1],
            du = cohen.d(
                d = get(lhs), f = get(rhs), paired = FALSE, pooled = TRUE)$conf.int[2]
            ) %>%
        {if (!is.null(facet) ) dplyr::rename_(., condition = facet) else .} %>%
        data.frame

    #################################################################
    # plotting it
    ##################################################

    df %>%
        ggplot(
            aes_string(x = lev[1], y = lev[2], colour = colour) ) +
        # adding all datapoints in background (if facet)
        {if (!is.null(facet) && background) geom_point(
            data = select_(df, paste("-", facet) ),
            color = "grey85") } +
        # adding condition-specific points
        geom_point() +
        # adding identity abline
        geom_abline() +
        # adding effect size (d_av)
        geom_label(
            data = dav,
            aes(
                x = -Inf, y = Inf,
                label = paste0(
                    "d = ", round(dav, 2), " [", round(dl, 2), ", ",
                    round(du, 2), "]"),
                hjust = 0, vjust = 1),
            inherit.aes = FALSE,
            size = 5) +
        # vertical error bars
        geom_errorbar(
            data = sums,
            aes_string(
                x = paste0(rhs, "_", lev[1]),
                ymin = paste0(rhs, "l", lev[2]),
                ymax = paste0(rhs, "u", lev[2]) ),
            size = 1.5, width = 0, inherit.aes = FALSE) +
        # horizontal error bars
        geom_errorbarh(
            data = sums,
            aes_string(
                x = paste0(rhs, "_", lev[1]),
                y = paste0(rhs, "_", lev[2]),
                xmin = paste0(rhs, "l", lev[1]),
                xmax = paste0(rhs, "u", lev[1]) ),
            size = 1.5, height = 0, inherit.aes = FALSE) +
        # plotting it by condition (if there is one)
        {if (!is.null(facet) ) facet_wrap(facet) } +
        # keeping aspect ratio to 1
        coord_fixed() +
        # axis labels
        labs(x = lev[1], y = lev[2]) +
        # theme aesthetics
        theme_bw(base_size = 14)

}
