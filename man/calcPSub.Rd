% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcBootEst.R
\name{calcPSub}
\alias{calcPSub}
\title{Calculate p-value}
\usage{
calcPSub(prop, altSide)
}
\arguments{
\item{prop}{numeric. Proportion of the null distribution less than the sample statistic.}

\item{altSide}{'both', 'high' or 'low'. The side of the null hypothesis the alternate hypothesis
is on.}
}
\value{
A single numeric, the p-value.
}
\description{
Calculate p-value based on a proportion
}
