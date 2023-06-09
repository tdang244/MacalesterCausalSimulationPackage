% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{ipw_viz_box}
\alias{ipw_viz_box}
\title{Creating a boxplot to compare effectiveness of propensity score weighting}
\usage{
ipw_viz_box(data, x, y, weights)
}
\arguments{
\item{data}{the data used for the propensity scoring analysis}

\item{x}{the x axis for the boxplot (usually the outcome variable) - must be a factor}

\item{y}{the y axis for the boxplot (usually the variable we want to no longer have a relationship with outcome)}

\item{weights}{optional parameter to include the weights}
}
\description{
Creating a boxplot to compare effectiveness of propensity score weighting
}
\examples{
ipw_viz_box(tenure, factor(gncs), full_ratio)
ipw_viz_box(tenure, factor(gncs), full_ratio, ipw)
}
