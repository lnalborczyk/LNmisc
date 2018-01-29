#' Modified Brinley's plots
#'
#' Plot...
#'
#' @param x A continuous predictor
#' @param y A continuous outcome variable
#' @param xlab Label of x-axis
#' @param ylab Label of y-axis
#'
#' @import ggplot2
#'
#' @examples
#' data(mtcars)
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @references Blampied, N. M. (2017). Analyzing Therapeutic Change Using
#' Modified Brinley Plots: History, Construction, and Interpretation. Behavior
#' Therapy, 48, 115-117.
#'
#' @export

brinley <- function(x = NULL, y = NULL, facet = NULL, colour = NULL, effsize = dav, background = TRUE, CI = TRUE) {

    ...

}

library(tidyverse)
library(lme4)
data(sleepstudy)

df <-
    sleepstudy %>%
    filter(Days == c(0, 1) ) %>%
    select(Days, Reaction)


# re-arranging dataframe

df <-
    DFclean %>%
    spread(key = Session, value = RUM) %>%
    rename(Post_induction = "Post-induction", Post_motor = "Post-motor")

# computing mean and confidence interval by group

df2 <-
    DFclean %>%
    filter(Session != "Baseline") %>%
    group_by(Session, Condition) %>%
    summarise(
        m = mean(RUM),
        lower = Rmisc::CI(RUM)[3],
        upper = Rmisc::CI(RUM)[1]
        )

# computing mean and confidence interval by group

means <-
    df2 %>%
    select(-lower, -upper) %>%
    spread(key = Session, value = m, sep = "_") %>%
    rename(
        Session_Post_induction = "Session_Post-induction",
        Session_Post_motor = "Session_Post-motor")

lower <-
    df2 %>%
    select(-m, -upper) %>%
    spread(key = Session, value = lower, sep = "l") %>%
    rename(
        SessionlPost_induction = "SessionlPost-induction",
        SessionlPost_motor = "SessionlPost-motor")

upper <-
    df2 %>%
    select(-lower, -m) %>%
    spread(key = Session, value = upper, sep = "u") %>%
    rename(
        SessionuPost_induction = "SessionuPost-induction",
        SessionuPost_motor = "SessionuPost-motor")

sums <- left_join(means, lower, by = "Condition")
sums <- left_join(sums, upper, by = "Condition")

# computing effect size

library(effsize)

dav <-
    DFclean %>%
    filter(Session != "Baseline") %>%
    mutate(Session = factor(Session) ) %>%
    group_by(Condition) %>%
    summarise(
        dav = effsize::cohen.d(RUM ~ Session, paired = FALSE, pooled = TRUE)$estimate,
        dl = effsize::cohen.d(RUM ~ Session, paired = FALSE, pooled = TRUE)$conf.int[1],
        du = effsize::cohen.d(RUM ~ Session, paired = FALSE, pooled = TRUE)$conf.int[2]
        )

# plot

df %>%
    ggplot(aes(x = Post_induction, y = Post_motor, colour = Verbality) ) +
    # adding all datapoints in background
    geom_point(
        data = df %>% select(-Condition),
        #alpha = 0.5,
        color = "grey85") +
    # adding condition-specific points
    geom_point() +
    # adding identity abline
    geom_abline() +
    # adding residuals lines
    #geom_segment(
    #    aes(
    #        xend = (Post_motor + Post_induction) / 2,
    #        yend = (Post_motor + Post_induction) / 2
    #    ),
    #    size = 0.3) +
    # adding effect size (d_av)
    geom_label(
        data = dav,
        aes(
            x = 70, y = 10,
            label = paste0("d_av = ", round(dav, 2), " [", round(dl, 2), ", ", round(du, 2), "]") ),
        inherit.aes = FALSE,
        size = 5) +
    # error bars
    geom_errorbar(
        data = sums,
        aes(
            x = Session_Post_induction,
            ymin = SessionlPost_motor, ymax = SessionuPost_motor),
        size = 1.5, width = 0, inherit.aes = FALSE) +
    geom_errorbarh(
        data = sums,
        aes(
            x = Session_Post_induction, y = Session_Post_motor,
            xmin = SessionlPost_induction, xmax = SessionuPost_induction),
        size = 1.5, height = 0, inherit.aes = FALSE) +
    # plotting it by condition
    facet_wrap(~Condition) +
    # keeping aspect ratio to 1
    theme(aspect.ratio = 2) +
    labs(x = "Post-Induction", y = "Post-Motor") +
    theme_bw(base_size = 14)
