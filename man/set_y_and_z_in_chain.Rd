% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{set_y_and_z_in_chain}
\alias{set_y_and_z_in_chain}
\title{Simulates Y and Z values where Y values are dependent on X values for a chain. This function is used within the simple_chain_simulation(n, x_numeric, y_numeric, z_numeric).}
\usage{
set_y_and_z_in_chain(n, y_numeric, z_numeric, X)
}
\arguments{
\item{n}{The number of observations of Z for a single simulation}

\item{y_numeric}{TRUE for numeric Y and FALSE for binary Y}

\item{z_numeric}{TRUE for numeric Z and FALSE for binary Z}

\item{X}{The values of X in the big function to generate Y values that are dependent on X values}
}
\value{
A list containing simulated Y and Z values
}
\description{
Simulates Y and Z values where Y values are dependent on X values for a chain. This function is used within the simple_chain_simulation(n, x_numeric, y_numeric, z_numeric).
}
