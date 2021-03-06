% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pgp_utilities.r
\name{signal}
\alias{signal}
\title{Signal - Adds signal to pgp object.}
\usage{
signal(pgp)
}
\arguments{
\item{pgp}{}
}
\value{
pgp clas with signal column
}
\description{
The signal is true (hold risky) when RiskyPx >= AvgPx, otherwise false (hold
safe).  The signal is calculated at the end of the period and should be
applied to the following month.
}
\examples{
signal(pgp)
}

