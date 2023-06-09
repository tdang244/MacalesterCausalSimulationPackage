% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{set_x_in_fork}
\alias{set_x_in_fork}
\title{Simulates X values that are dependent on Y values in a fork. This function is used within the simple_fork_simulation(n, x_numeric, y_numeric, z_numeric) function.}
\usage{
set_x_in_fork(n, x_numeric, Y)
}
\arguments{
\item{n}{The number of observations of X for a single simulation}

\item{x_numeric}{TRUE for numeric X and FALSE for binary X}

\item{Y}{The values of Y in the big function to generate X values that are dependent on Y values}
}
\value{
Simulated X values
}
\description{
Simulates X values that are dependent on Y values in a fork. This function is used within the simple_fork_simulation(n, x_numeric, y_numeric, z_numeric) function.
}
