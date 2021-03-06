% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logit_dotplot.R
\name{logit_dotplot}
\alias{logit_dotplot}
\title{Plot predictions of a logistic regression model along with raw
marginal distributions}
\usage{
logit_dotplot(x, y, xlab, ylab)
}
\arguments{
\item{x}{A continuous predictor}

\item{y}{A continuous outcome variable}

\item{xlab}{Label of x-axis}

\item{ylab}{Label of y-axis}
}
\description{
Plot inspired from: https://recology.info/2012/01/logistic-regression-barplot-fig/
and: http://rpubs.com/kohske/dual_axis_in_ggplot2
}
\examples{
data(howell)
d <- howell
logit_dotplot(d$height, d$male, xlab = "height", ylab = "p(male)")

}
\author{
Ladislas Nalborczyk <\email{ladislas.nalborczyk@gmail.com}>
}
