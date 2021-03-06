% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggbootMISC.R, R/ggbootMV-helper.R
\name{getLocFunc}
\alias{getLocFunc}
\alias{getLocFunc}
\title{Return the selected location function.}
\usage{
getLocFunc(locType = "mean", .trim = trim)

getLocFunc(locType = "mean", .trim = trim)
}
\arguments{
\item{locType}{character. Specifies measure of location to use. Options are
mean, cmwed (column-wise median), oja, weisz (geometric median) and
gmed (a fast version for the geometric median).}

\item{locType}{character. Selects the location type to choose. Options
are: mean (column-wise median), gmed (geometric median), wei (fast approximate geometric median), oja and cwmed (column-wise median). Default is mean.}
}
\description{
Return the selected location function.

Based on \code{locType}, returns the measure of location desired.
}
